# =====================================================================
# Predictor analysis & causal-inference scaffolds
#
# Importance / partial-dependence helpers and small-form-factor
# causal estimators: DiD (2x2), IPW propensity-score, IV (via AER).
#
# Every function returns a list with a `notes` slot describing the
# methodological caveats, and gracefully returns NULL when an optional
# package is missing (callers render an inline notice in that case).
#
# Author: Malik Hebbat
# =====================================================================

# ---- Predictor importance --------------------------------------------
# Try, in order:
#   1. iml::FeatureImp (permutation importance) — if installed
#   2. model-specific built-ins (ranger, xgboost, glmnet, gam)
#   3. correlation with the target as a last-resort fallback
predictor_importance <- function(fit, df, target, method = c("auto","perm","builtin")) {
  method <- match.arg(method)
  out <- NULL
  model <- fit$model
  preds <- setdiff(names(df), target)
  if (length(preds) == 0L) return(NULL)

  # 1) iml permutation importance (works for any predict-able model)
  if (method %in% c("auto","perm") &&
      requireNamespace("iml", quietly = TRUE) && is.function(fit$predict)) {
    out <- tryCatch({
      pred_fn <- function(model, newdata) {
        p <- fit$predict(newdata)
        if (is.data.frame(p)) p$predicted else as.numeric(p)
      }
      preds_obj <- iml::Predictor$new(model = model, data = df[, preds, drop = FALSE],
                                       y = df[[target]], predict.function = pred_fn)
      imp <- iml::FeatureImp$new(preds_obj, loss = "rmse", n.repetitions = 5)
      data.frame(feature = imp$results$feature,
                 importance = imp$results$importance,
                 source = "iml::FeatureImp (permutation)",
                 stringsAsFactors = FALSE)
    }, error = function(e) NULL)
    if (!is.null(out) && nrow(out) > 0) return(out[order(-out$importance), , drop = FALSE])
  }

  # 2) Built-ins
  if (method %in% c("auto","builtin")) {
    if (inherits(model, "ranger")) {
      imp <- tryCatch(ranger::importance(model), error = function(e) NULL)
      if (!is.null(imp))
        return(data.frame(feature = names(imp), importance = as.numeric(imp),
                          source = "ranger::importance",
                          stringsAsFactors = FALSE))
    }
    if (inherits(model, "xgb.Booster") &&
        requireNamespace("xgboost", quietly = TRUE)) {
      imp <- tryCatch(xgboost::xgb.importance(model = model),
                      error = function(e) NULL)
      if (!is.null(imp) && nrow(imp) > 0)
        return(data.frame(feature = imp$Feature, importance = imp$Gain,
                          source = "xgboost::xgb.importance",
                          stringsAsFactors = FALSE))
    }
    if (inherits(model, "lm")) {
      co <- tryCatch(summary(model)$coefficients, error = function(e) NULL)
      if (!is.null(co) && nrow(co) > 1)
        return(data.frame(feature = rownames(co)[-1],
                          importance = abs(co[-1, "t value"]),
                          source = "abs(t-statistic)",
                          stringsAsFactors = FALSE))
    }
    if (inherits(model, "gam") && requireNamespace("mgcv", quietly = TRUE)) {
      sm <- tryCatch(summary(model)$s.table, error = function(e) NULL)
      if (!is.null(sm) && nrow(sm) > 0)
        return(data.frame(feature = rownames(sm),
                          importance = as.numeric(sm[, "F"]),
                          source = "mgcv::summary s.table F",
                          stringsAsFactors = FALSE))
    }
  }

  # 3) Correlation fallback
  num_preds <- preds[vapply(preds, function(p) is.numeric(df[[p]]), logical(1))]
  if (length(num_preds) > 0L && is.numeric(df[[target]])) {
    cors <- vapply(num_preds, function(p)
      suppressWarnings(stats::cor(df[[p]], df[[target]],
                                   use = "pairwise.complete.obs")),
      numeric(1))
    return(data.frame(feature = num_preds, importance = abs(cors),
                      source = "abs(Pearson correlation) — fallback",
                      stringsAsFactors = FALSE))
  }
  NULL
}

# ---- Partial dependence ----------------------------------------------
# Returns a list of data.frames (one per feature) with columns x, yhat.
partial_dependence <- function(fit, df, target, features, grid_n = 25) {
  if (length(features) == 0L) return(list())
  if (!is.function(fit$predict)) return(list())
  out <- list()
  use_pdp <- requireNamespace("pdp", quietly = TRUE)
  for (f in features) {
    if (!f %in% names(df)) next
    res <- if (use_pdp) {
      tryCatch({
        pp <- pdp::partial(fit$model, pred.var = f,
                            train = df, grid.resolution = grid_n,
                            pred.fun = function(object, newdata) {
                              p <- fit$predict(newdata)
                              if (is.data.frame(p)) p$predicted else as.numeric(p)
                            })
        data.frame(x = pp[[f]], yhat = pp$yhat,
                   stringsAsFactors = FALSE)
      }, error = function(e) NULL)
    } else NULL
    if (is.null(res)) {
      # Hand-rolled marginal-mean grid
      x <- df[[f]]
      grid <- if (is.numeric(x)) {
        seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = grid_n)
      } else {
        sort(unique(x))
      }
      yhat <- vapply(grid, function(v) {
        nd <- df; nd[[f]] <- v
        p <- tryCatch(fit$predict(nd), error = function(e) rep(NA_real_, nrow(nd)))
        pv <- if (is.data.frame(p)) p$predicted else as.numeric(p)
        suppressWarnings(mean(as.numeric(pv), na.rm = TRUE))
      }, numeric(1))
      res <- data.frame(x = grid, yhat = yhat, stringsAsFactors = FALSE)
    }
    res$feature <- f
    out[[f]] <- res
  }
  out
}

# ---- Causal scaffolds ------------------------------------------------
# 2x2 difference-in-differences. Returns ATE estimate, SE, p-value
# from the interaction term `treat:post` of an OLS regression.
did_2x2 <- function(df, y, treat, post, controls = character(0)) {
  for (col in c(y, treat, post))
    if (!col %in% names(df))
      stop(sprintf("did_2x2: column '%s' not found.", col), call. = FALSE)
  rhs <- paste(c(treat, post, sprintf("%s:%s", treat, post), controls),
               collapse = " + ")
  fmla <- stats::as.formula(sprintf("`%s` ~ %s", y, rhs))
  m <- stats::lm(fmla, data = df)
  co <- tryCatch(summary(m)$coefficients, error = function(e) NULL)
  int_term <- sprintf("%s:%s", treat, post)
  alt_term <- sprintf("%s:%s", post, treat)
  pick <- intersect(c(int_term, alt_term), rownames(co %||% data.frame()))
  if (length(pick) == 0L)
    return(list(ate = NA_real_, se = NA_real_, p = NA_real_,
                model = m,
                notes = "Interaction term not identified — check encoding of treat/post."))
  row <- co[pick[1], , drop = TRUE]
  list(ate = unname(row["Estimate"]),
       se  = unname(row["Std. Error"]),
       p   = unname(row[grep("Pr\\(", colnames(co))]),
       model = m,
       notes = paste("Two-period 2x2 DiD via OLS interaction.",
                     "Identification assumes parallel pre-trends.",
                     "Validate with an event-study before relying on the ATE."))
}

# Inverse-probability-weighted ATT/ATE via a propensity-score logit.
ipw_ps <- function(df, y, treat, covariates, estimand = c("ATE","ATT")) {
  estimand <- match.arg(estimand)
  for (col in c(y, treat, covariates))
    if (!col %in% names(df))
      stop(sprintf("ipw_ps: column '%s' not found.", col), call. = FALSE)
  fmla <- stats::as.formula(sprintf("`%s` ~ %s", treat,
                                     paste(covariates, collapse = " + ")))
  ps_fit <- tryCatch(stats::glm(fmla, data = df, family = stats::binomial()),
                     error = function(e) stop("Propensity model failed: ",
                                              conditionMessage(e), call. = FALSE))
  ps <- stats::predict(ps_fit, type = "response")
  t_vec <- as.numeric(df[[treat]])
  y_vec <- as.numeric(df[[y]])
  w <- if (estimand == "ATE") {
    t_vec / ps + (1 - t_vec) / (1 - ps)
  } else {
    t_vec + (1 - t_vec) * ps / (1 - ps)
  }
  ok <- is.finite(w) & is.finite(y_vec) & is.finite(t_vec)
  w <- w[ok]; y_vec <- y_vec[ok]; t_vec <- t_vec[ok]
  est <- sum(w * t_vec * y_vec) / sum(w * t_vec) -
         sum(w * (1 - t_vec) * y_vec) / sum(w * (1 - t_vec))
  list(estimate = est, estimand = estimand, ps_fit = ps_fit,
       n = length(y_vec),
       notes = paste("IPW from a logit propensity model. Trim or stabilise",
                     "weights if any unit has near-zero probability.",
                     "Validate with covariate balance after weighting."))
}

# Two-stage least squares via AER::ivreg.
iv_2sls <- function(df, y, x_endog, instruments, controls = character(0)) {
  if (!requireNamespace("AER", quietly = TRUE))
    return(list(ok = FALSE,
                notes = "Install 'AER' to enable IV: install.packages('AER')"))
  for (col in c(y, x_endog, instruments, controls))
    if (!col %in% names(df))
      stop(sprintf("iv_2sls: column '%s' not found.", col), call. = FALSE)
  rhs <- paste(c(x_endog, controls), collapse = " + ")
  iv  <- paste(c(instruments, controls), collapse = " + ")
  fmla <- stats::as.formula(sprintf("`%s` ~ %s | %s", y, rhs, iv))
  m <- AER::ivreg(fmla, data = df)
  list(ok = TRUE, model = m, summary = summary(m, diagnostics = TRUE),
       notes = paste("IV / 2SLS via AER::ivreg. Inspect first-stage F (>= 10",
                     "rule of thumb) and the Sargan over-identification test."))
}

# Robust-SE coefficient table for an lm/glm fit
robust_coef_table <- function(model, type = c("HC3","HC1","HC0")) {
  type <- match.arg(type)
  if (!requireNamespace("sandwich", quietly = TRUE) ||
      !requireNamespace("lmtest", quietly = TRUE)) {
    return(list(ok = FALSE,
                notes = "Install 'sandwich' and 'lmtest' for robust SEs."))
  }
  vcv <- tryCatch(sandwich::vcovHC(model, type = type),
                  error = function(e) NULL)
  if (is.null(vcv)) return(list(ok = FALSE, notes = "vcovHC failed."))
  tt <- lmtest::coeftest(model, vcov. = vcv)
  list(ok = TRUE, table = unclass(tt), type = type,
       notes = sprintf("Heteroskedasticity-robust SEs (%s) via sandwich+lmtest.", type))
}
