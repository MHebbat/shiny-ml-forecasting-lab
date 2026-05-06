# =====================================================================
# AI Analysis
# Sends a structured payload to an LLM (OpenAI / Anthropic / Perplexity)
# and parses an objective, blunt verdict + suggestions.
# Falls back to a deterministic heuristic verdict when no API key is set.
# Author: Malik Hebbat
# =====================================================================

# ---- System prompt: enforce blunt, objective tone --------------------
AI_SYSTEM_PROMPT <- paste(
  "You are a senior applied statistician and ML engineer reviewing a model run.",
  "Your job is to give an OBJECTIVE, BLUNT, technical verdict.",
  "Do not flatter the user. Do not hedge. Do not pad with marketing language.",
  "If the model is bad, say it is bad and explain exactly why.",
  "If residuals show bias, heteroscedasticity, or autocorrelation, say so.",
  "If the metric is good but suspicious (overfitting, leakage, tiny test set), flag it.",
  "Always respond in the EXACT JSON schema requested. No markdown fences around the JSON.",
  sep = " "
)

# Compose the user message with the run payload
.build_user_prompt <- function(payload) {
  paste0(
    "Analyze this model run and return a JSON object with keys:\n",
    "  grade        : one of 'PASS', 'WEAK', 'FAIL'\n",
    "  verdict      : one-sentence direct verdict (max 25 words)\n",
    "  analysis_md  : 4-8 sentence markdown analysis covering: ",
    "                 metric quality vs task baseline, residual diagnostics, ",
    "                 risk flags (overfit/leakage/calibration), data adequacy.\n",
    "  suggestions  : array of {action, reason} objects \u2014 concrete next steps ",
    "                 (hyperparameter ranges to try, alternative models from the ",
    "                 registry, feature engineering ideas).\n\n",
    "Run payload:\n",
    jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, na = "null"),
    "\n\nRespond with raw JSON only."
  )
}

# ---- Provider clients -------------------------------------------------
.call_openai <- function(prompt, model = "gpt-4o-mini",
                         api_key = Sys.getenv("OPENAI_API_KEY")) {
  if (!nzchar(api_key)) stop("OPENAI_API_KEY not set")
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = AI_SYSTEM_PROMPT),
      list(role = "user",   content = prompt)
    ),
    temperature = 0.2,
    response_format = list(type = "json_object")
  )
  res <- httr_post("https://api.openai.com/v1/chat/completions",
                   headers = c(Authorization = paste("Bearer", api_key),
                               `Content-Type` = "application/json"),
                   body = body)
  res$choices[[1]]$message$content
}

.call_anthropic <- function(prompt, model = "claude-3-5-sonnet-20241022",
                            api_key = Sys.getenv("ANTHROPIC_API_KEY")) {
  if (!nzchar(api_key)) stop("ANTHROPIC_API_KEY not set")
  body <- list(
    model = model,
    max_tokens = 2000,
    system = AI_SYSTEM_PROMPT,
    messages = list(list(role = "user", content = prompt))
  )
  res <- httr_post("https://api.anthropic.com/v1/messages",
                   headers = c(`x-api-key` = api_key,
                               `anthropic-version` = "2023-06-01",
                               `Content-Type` = "application/json"),
                   body = body)
  res$content[[1]]$text
}

.call_perplexity <- function(prompt, model = "sonar-pro",
                             api_key = Sys.getenv("PPLX_API_KEY")) {
  if (!nzchar(api_key)) stop("PPLX_API_KEY not set")
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = AI_SYSTEM_PROMPT),
      list(role = "user",   content = prompt)
    ),
    temperature = 0.2
  )
  res <- httr_post("https://api.perplexity.ai/chat/completions",
                   headers = c(Authorization = paste("Bearer", api_key),
                               `Content-Type` = "application/json"),
                   body = body)
  res$choices[[1]]$message$content
}

# Tiny httr wrapper; uses curl as fallback if httr2/httr unavailable
httr_post <- function(url, headers, body) {
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  if (requireNamespace("httr2", quietly = TRUE)) {
    req <- httr2::request(url) |>
      httr2::req_headers(!!!as.list(headers)) |>
      httr2::req_body_raw(body_json, type = "application/json") |>
      httr2::req_timeout(60) |>
      httr2::req_error(is_error = function(resp) FALSE)
    resp <- httr2::req_perform(req)
    txt <- httr2::resp_body_string(resp)
    if (httr2::resp_status(resp) >= 400) stop("HTTP ", httr2::resp_status(resp), ": ", txt)
    return(jsonlite::fromJSON(txt, simplifyVector = FALSE))
  }
  if (requireNamespace("httr", quietly = TRUE)) {
    h <- do.call(httr::add_headers, as.list(headers))
    resp <- httr::POST(url, h, body = body_json, encode = "raw",
                       httr::timeout(60))
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    if (httr::status_code(resp) >= 400) stop("HTTP ", httr::status_code(resp), ": ", txt)
    return(jsonlite::fromJSON(txt, simplifyVector = FALSE))
  }
  stop("Install 'httr2' or 'httr' to call AI providers.")
}

# ---- Heuristic fallback ----------------------------------------------
# Used when no API key is set OR the LLM call fails. Gives an honest
# rule-based verdict so the feature always works.
.heuristic_verdict <- function(payload) {
  m  <- payload$metrics %||% list()
  task <- payload$task_type %||% "regression"
  res  <- payload$residual_stats %||% list()
  n_test <- payload$n_test %||% NA

  grade <- "WEAK"; verdict <- ""; bullets <- character()

  if (task %in% c("regression","time_series","count","proportion")) {
    r2 <- m$r2 %||% NA_real_
    rmse <- m$rmse %||% NA_real_; mae <- m$mae %||% NA_real_; mape <- m$mape %||% NA_real_
    if (!is.na(r2)) {
      if (r2 >= 0.85) { grade <- "PASS"; verdict <- sprintf("Strong fit (R\u00b2=%.3f). Residuals are the only thing left to scrutinise.", r2) }
      else if (r2 >= 0.5) { grade <- "WEAK"; verdict <- sprintf("Moderate fit (R\u00b2=%.3f). The model captures structure but leaves substantial unexplained variance.", r2) }
      else if (r2 >= 0) { grade <- "FAIL"; verdict <- sprintf("Poor fit (R\u00b2=%.3f). Predictions barely beat the mean baseline.", r2) }
      else { grade <- "FAIL"; verdict <- sprintf("Worse than mean baseline (R\u00b2=%.3f). The model is actively harmful.", r2) }
    } else if (!is.na(rmse)) {
      verdict <- sprintf("RMSE=%.4f, MAE=%.4f. Compare to your target's standard deviation to judge usefulness.", rmse, mae)
    }
    if (!is.na(res$bias %||% NA) && abs(res$bias) > 0.1 * (sd(c(0, rmse), na.rm=TRUE) + 1e-9))
      bullets <- c(bullets, sprintf("- Predictions show systematic bias (mean residual = %.4f) \u2014 the model under- or over-shoots on average.", res$bias))
    if (!is.na(res$het_pval %||% NA) && res$het_pval < 0.05)
      bullets <- c(bullets, "- Residual variance is non-constant (heteroscedasticity). Confidence intervals are unreliable.")
    if (!is.na(res$acf1 %||% NA) && abs(res$acf1) > 0.3)
      bullets <- c(bullets, sprintf("- Residuals are autocorrelated (lag-1 ACF = %.2f). Important temporal structure is unmodeled.", res$acf1))
    if (!is.na(mape) && mape > 50)
      bullets <- c(bullets, sprintf("- MAPE = %.1f%% is very high \u2014 most predictions are off by half or more.", mape))
  } else {  # classification
    acc <- m$accuracy %||% NA_real_; f1 <- m$f1 %||% NA_real_
    if (!is.na(acc)) {
      if (acc >= 0.9) { grade <- "PASS"; verdict <- sprintf("Accuracy=%.3f. Strong, but verify against class balance and look at F1.", acc) }
      else if (acc >= 0.7) { grade <- "WEAK"; verdict <- sprintf("Accuracy=%.3f. Acceptable for some uses but a single class likely dominates.", acc) }
      else { grade <- "FAIL"; verdict <- sprintf("Accuracy=%.3f. Barely better than guessing.", acc) }
    }
    if (!is.na(f1) && f1 < 0.5)
      bullets <- c(bullets, sprintf("- F1=%.3f is poor on the positive class. Likely class imbalance or weak features.", f1))
  }

  if (!is.na(n_test) && n_test < 30)
    bullets <- c(bullets, sprintf("- Holdout has only %d rows \u2014 metrics are unstable. Use k-fold CV.", n_test))

  if (length(bullets) == 0)
    bullets <- "- No major red flags detected by heuristics. An LLM review would add depth (set OPENAI_API_KEY / ANTHROPIC_API_KEY / PPLX_API_KEY)."

  analysis_md <- paste(c(verdict, "", bullets), collapse = "\n")

  suggestions <- list(
    list(action = "Try a stronger model class",
         reason = "If grade is WEAK/FAIL, switch from linear -> tree-based -> gradient boosting in the registry."),
    list(action = "Add lag/rolling features",
         reason = "If task is time series, ensure lags 1\u20133 and a rolling mean are enabled in the recipe builder."),
    list(action = "Tune learning rate + depth",
         reason = "For XGBoost/LightGBM, try eta in {0.01, 0.05, 0.1} and max_depth in {3, 6, 9}."),
    list(action = "Use k-fold CV",
         reason = "Single-holdout estimates are noisy; switch validation scheme in Model Lab.")
  )

  list(grade = grade, verdict = verdict, analysis_md = analysis_md,
       suggestions = suggestions)
}

# ---- Build the payload sent to the AI --------------------------------
build_run_payload <- function(state) {
  lm_obj <- state$last_model
  if (is.null(lm_obj)) return(NULL)
  actual <- as.numeric(lm_obj$actual)
  pred   <- as.numeric(lm_obj$predicted)
  residuals <- actual - pred
  res_stats <- list()
  if (length(residuals) > 5 && all(is.finite(residuals))) {
    res_stats$bias <- mean(residuals, na.rm = TRUE)
    res_stats$sd_resid <- sd(residuals, na.rm = TRUE)
    # crude heteroscedasticity check: correlation between |resid| and pred
    res_stats$het_cor <- suppressWarnings(cor(abs(residuals), pred,
                                               use = "complete.obs"))
    res_stats$het_pval <- tryCatch(cor.test(abs(residuals), pred)$p.value,
                                    error = function(e) NA_real_)
    if (length(residuals) >= 10) {
      res_stats$acf1 <- tryCatch(as.numeric(acf(residuals, plot = FALSE,
                                                  lag.max = 1)$acf[2]),
                                  error = function(e) NA_real_)
    }
  }
  meta <- state$meta %||% list()
  list(
    task_type      = meta$task_type,
    target         = meta$target,
    frequency      = meta$frequency,
    model_id       = state$last_model$model_id %||% NA,
    metrics        = lm_obj$metrics,
    residual_stats = res_stats,
    n_train        = state$n_train %||% NA,
    n_test         = length(actual),
    horizon        = if (isTRUE(meta$task_type == "time_series")) length(actual) else NA,
    sample_predictions = utils::head(
      data.frame(actual = actual, predicted = pred), 20),
    hyperparameters = state$last_params %||% list()
  )
}

# ---- Public helpers (used by other modules) -------------------------
# Returns a list(provider=..., key=...) describing the selected provider.
ai_pick_provider <- function(provider = "auto") {
  if (is.null(provider)) provider <- "auto"
  if (provider == "auto") {
    if (nzchar(Sys.getenv("OPENAI_API_KEY")))         return(list(provider = "openai"))
    if (nzchar(Sys.getenv("ANTHROPIC_API_KEY")))      return(list(provider = "anthropic"))
    if (nzchar(Sys.getenv("PPLX_API_KEY")))           return(list(provider = "perplexity"))
    return(list(provider = "heuristic"))
  }
  list(provider = provider)
}

# Generic LLM call. Takes the provider list returned by ai_pick_provider
# and a prompt string. Returns the raw text response or stops with error.
ai_call <- function(prov, prompt) {
  switch(prov$provider %||% "heuristic",
    "openai"     = .call_openai(prompt),
    "anthropic"  = .call_anthropic(prompt),
    "perplexity" = .call_perplexity(prompt),
    stop("No live AI provider available"))
}

# ---- Public entry point ----------------------------------------------
run_ai_analysis <- function(state, provider = c("auto","openai","anthropic","perplexity","heuristic")) {
  provider <- match.arg(provider)
  payload  <- build_run_payload(state)
  if (is.null(payload)) stop("No model run to analyze.")

  # Auto-select first available provider with a key
  if (provider == "auto") {
    if (nzchar(Sys.getenv("OPENAI_API_KEY")))         provider <- "openai"
    else if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) provider <- "anthropic"
    else if (nzchar(Sys.getenv("PPLX_API_KEY")))      provider <- "perplexity"
    else                                              provider <- "heuristic"
  }

  if (provider == "heuristic") {
    out <- .heuristic_verdict(payload)
    out$provider <- "heuristic"
    return(out)
  }

  prompt <- .build_user_prompt(payload)
  raw <- tryCatch({
    switch(provider,
      "openai"     = .call_openai(prompt),
      "anthropic"  = .call_anthropic(prompt),
      "perplexity" = .call_perplexity(prompt))
  }, error = function(e) {
    warning("AI provider call failed: ", conditionMessage(e),
            ". Falling back to heuristic.")
    NULL
  })
  if (is.null(raw)) {
    out <- .heuristic_verdict(payload); out$provider <- "heuristic"; return(out)
  }
  # Try to parse JSON (strip code fences if present)
  txt <- gsub("```json|```", "", raw)
  parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (is.null(parsed)) {
    out <- .heuristic_verdict(payload); out$provider <- "heuristic"
    out$analysis_md <- paste0("(LLM returned non-JSON; falling back to heuristic.)\n\n",
                              out$analysis_md)
    return(out)
  }
  list(
    grade       = parsed$grade %||% "WEAK",
    verdict     = parsed$verdict %||% "",
    analysis_md = parsed$analysis_md %||% "",
    suggestions = parsed$suggestions %||% list(),
    provider    = provider
  )
}
