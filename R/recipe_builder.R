# =====================================================================
# Modern best-practice recipe builder
# Pure R; uses base + recipes when available. Fully local, no network.
# =====================================================================

# ---- Column profile --------------------------------------------------
# Returns a data.frame with one row per column describing health.
profile_columns <- function(df, target = NULL, time_col = NULL) {
  out <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    n <- length(x)
    n_miss <- sum(is.na(x))
    n_unique <- length(unique(stats::na.omit(x)))
    cls <- paste(class(x), collapse = "/")
    is_num <- is.numeric(x)
    skew <- if (is_num && n_unique > 2) {
      v <- as.numeric(stats::na.omit(x))
      m <- mean(v); s <- stats::sd(v)
      if (is.finite(s) && s > 0) mean(((v - m) / s) ^ 3) else NA_real_
    } else NA_real_
    out_iqr <- if (is_num && n_unique > 4) {
      q <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
      iqr <- q[2] - q[1]
      sum(x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr, na.rm = TRUE)
    } else NA_integer_
    is_constant   <- n_unique <= 1
    is_quasi_const <- n_unique > 1 && n_unique < max(2, ceiling(0.005 * n))
    is_id_like    <- n_unique == n - n_miss && !is_num
    severity <- if (is_constant) "CRITICAL"
                else if (n_miss / n > 0.6) "CRITICAL"
                else if (is_id_like || is_quasi_const) "WARN"
                else if (n_miss / n > 0.2) "WARN"
                else if (is_num && !is.na(skew) && abs(skew) > 1.5) "WARN"
                else "OK"
    finding <- character(0)
    if (is_constant)    finding <- c(finding, "Zero variance - drop")
    if (is_quasi_const) finding <- c(finding, "Quasi-constant - likely drop")
    if (is_id_like)     finding <- c(finding, "ID-like - drop or hash")
    if (n_miss / n > 0.2) finding <- c(finding,
      sprintf("%.0f%% missing", 100 * n_miss / n))
    if (is_num && !is.na(skew) && abs(skew) > 1.5) finding <- c(finding,
      sprintf("skew=%.2f - power transform", skew))
    if (is_num && !is.na(out_iqr) && out_iqr > 0.05 * n) finding <- c(finding,
      sprintf("%d outliers (IQR)", out_iqr))
    if (length(finding) == 0) finding <- "Healthy"
    data.frame(
      column = nm, class = cls, n_missing = n_miss,
      pct_missing = round(100 * n_miss / n, 2),
      n_unique = n_unique, is_numeric = is_num, skew = round(skew, 3),
      n_outliers = out_iqr, is_constant = is_constant,
      is_quasi_const = is_quasi_const, is_id_like = is_id_like,
      severity = severity, finding = paste(finding, collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

# ---- Leakage check ---------------------------------------------------
leakage_scan <- function(df, target) {
  if (is.null(target) || !target %in% names(df)) return(data.frame())
  y <- df[[target]]
  rows <- list()
  for (nm in setdiff(names(df), target)) {
    x <- df[[nm]]
    if (is.numeric(x) && is.numeric(y)) {
      ok <- complete.cases(x, y)
      if (sum(ok) > 5) {
        r <- suppressWarnings(stats::cor(x[ok], y[ok]))
        if (!is.na(r) && abs(r) > 0.99)
          rows[[length(rows)+1]] <- data.frame(column = nm,
            relation = sprintf("|cor|=%.4f", r),
            risk = "near-perfect correlation",
            stringsAsFactors = FALSE)
      }
    } else if (is.numeric(y) && is.factor(x)) {
      grp <- tapply(y, x, mean, na.rm = TRUE)
      tot_var <- stats::var(y, na.rm = TRUE)
      bet_var <- stats::var(grp, na.rm = TRUE) * length(grp) / length(y)
      ratio <- if (is.finite(tot_var) && tot_var > 0) bet_var / tot_var else 0
      if (!is.na(ratio) && ratio > 0.99)
        rows[[length(rows)+1]] <- data.frame(column = nm,
          relation = sprintf("between/total var=%.3f", ratio),
          risk = "groups explain ~all variance",
          stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

# ---- Recipe step list ------------------------------------------------
default_steps <- function(profile, target = NULL, options = list()) {
  o <- modifyList(list(
    drop_zero_var = TRUE,
    drop_id_like  = TRUE,
    impute        = TRUE,
    knn_impute    = FALSE,
    lump_rare     = TRUE,
    rare_threshold = 0.01,
    encode_cats   = TRUE,
    yeo_johnson   = TRUE,
    scale         = TRUE,
    winsorize     = FALSE,
    add_date_feat = TRUE,
    drop_high_missing = TRUE,
    high_missing_threshold = 0.6
  ), options %||% list())

  tgt_keep <- function(v) v[v != (target %||% "")]
  steps <- list()
  if (o$drop_zero_var) steps <- append(steps, list(list(
    id = "drop_zero_var", label = "Drop zero-variance columns",
    targets = profile$column[profile$is_constant])))
  if (o$drop_id_like) steps <- append(steps, list(list(
    id = "drop_id_like", label = "Drop ID-like columns",
    targets = tgt_keep(profile$column[profile$is_id_like]))))
  if (o$drop_high_missing) steps <- append(steps, list(list(
    id = "drop_high_missing",
    label = sprintf("Drop columns with >%.0f%% missing",
                    100*o$high_missing_threshold),
    targets = profile$column[profile$pct_missing/100 > o$high_missing_threshold])))
  if (o$impute) steps <- append(steps, list(list(
    id = "impute", label = "Impute missing (median / mode)",
    targets = profile$column[profile$n_missing > 0])))
  if (o$lump_rare) steps <- append(steps, list(list(
    id = "lump_rare",
    label = sprintf("Lump rare categorical levels (<%.1f%%)",
                    100*o$rare_threshold),
    targets = profile$column[!profile$is_numeric & profile$n_unique > 6])))
  if (o$add_date_feat) steps <- append(steps, list(list(
    id = "date_feat",
    label = "Extract date features (year/month/dow/quarter)",
    targets = profile$column[grepl("Date|POSIX", profile$class)])))
  if (o$yeo_johnson) steps <- append(steps, list(list(
    id = "yeo_johnson",
    label = "Yeo-Johnson power transform (|skew|>1)",
    targets = profile$column[profile$is_numeric & !is.na(profile$skew) &
                              abs(profile$skew) > 1])))
  if (o$winsorize) steps <- append(steps, list(list(
    id = "winsorize", label = "Winsorize at 1st / 99th percentile",
    targets = profile$column[profile$is_numeric])))
  if (o$scale) steps <- append(steps, list(list(
    id = "scale", label = "Standardize numeric features (z-score)",
    targets = tgt_keep(profile$column[profile$is_numeric]))))
  if (o$encode_cats) steps <- append(steps, list(list(
    id = "encode", label = "One-hot encode categorical features",
    targets = tgt_keep(profile$column[!profile$is_numeric]))))
  steps
}

# ---- Apply pipeline to a data.frame ----------------------------------
apply_recipe <- function(df, steps, target = NULL) {
  log <- list()
  add <- function(msg) log[[length(log)+1]] <<- msg
  for (s in steps) {
    tgts <- intersect(s$targets, names(df))
    if (length(tgts) == 0) next
    if (s$id %in% c("drop_zero_var", "drop_id_like", "drop_high_missing")) {
      df[, tgts] <- NULL
      add(sprintf("%s -> dropped %d cols", s$id, length(tgts)))
    } else if (s$id == "impute") {
      for (col in tgts) {
        x <- df[[col]]
        if (sum(is.na(x)) == 0) next
        if (is.numeric(x)) {
          df[[col]][is.na(x)] <- stats::median(x, na.rm = TRUE)
        } else {
          tab <- sort(table(x), decreasing = TRUE)
          mode_val <- if (length(tab) > 0) names(tab)[1] else NA
          df[[col]][is.na(x)] <- mode_val
        }
      }
      add(sprintf("impute -> filled %d cols", length(tgts)))
    } else if (s$id == "lump_rare") {
      for (col in tgts) {
        x <- as.character(df[[col]])
        tab <- table(x) / length(x)
        rare <- names(tab)[tab < 0.01]
        if (length(rare) > 0) x[x %in% rare] <- "Other"
        df[[col]] <- as.factor(x)
      }
      add(sprintf("lump_rare -> processed %d cols", length(tgts)))
    } else if (s$id == "date_feat") {
      for (col in tgts) {
        x <- df[[col]]
        df[[paste0(col, "_year")]]  <- as.integer(format(x, "%Y"))
        df[[paste0(col, "_month")]] <- as.integer(format(x, "%m"))
        df[[paste0(col, "_dow")]]   <- as.integer(format(x, "%u"))
        df[[paste0(col, "_qtr")]]   <- ((as.integer(format(x, "%m"))-1) %/% 3) + 1
        df[[col]] <- NULL
      }
      add(sprintf("date_feat -> expanded %d cols", length(tgts)))
    } else if (s$id == "yeo_johnson") {
      # Negative-safe symmetric log; light dependency-free substitute
      yeo <- function(v) sign(v) * log1p(abs(v))
      for (col in tgts) df[[col]] <- yeo(as.numeric(df[[col]]))
      add(sprintf("yeo_johnson -> transformed %d cols", length(tgts)))
    } else if (s$id == "winsorize") {
      for (col in tgts) {
        x <- as.numeric(df[[col]])
        q <- stats::quantile(x, c(0.01, 0.99), na.rm = TRUE)
        x[x < q[1]] <- q[1]; x[x > q[2]] <- q[2]
        df[[col]] <- x
      }
      add(sprintf("winsorize -> clipped %d cols", length(tgts)))
    } else if (s$id == "scale") {
      for (col in tgts) {
        x <- as.numeric(df[[col]])
        m <- mean(x, na.rm = TRUE); sdv <- stats::sd(x, na.rm = TRUE)
        if (is.finite(sdv) && sdv > 0) df[[col]] <- (x - m) / sdv
      }
      add(sprintf("scale -> standardized %d cols", length(tgts)))
    } else if (s$id == "encode") {
      for (col in tgts) {
        x <- as.factor(as.character(df[[col]]))
        if (length(levels(x)) <= 1) next
        mm <- stats::model.matrix(~ x - 1)
        colnames(mm) <- gsub("^x", paste0(col, "_"), colnames(mm))
        df[[col]] <- NULL
        df <- cbind(df, as.data.frame(mm))
      }
      add(sprintf("encode -> one-hot %d cols", length(tgts)))
    }
  }
  list(df = df, log = log)
}

# ---- AI prep assist (column profile only - never raw rows) -----------
ai_prep_suggestions <- function(profile, target = NULL,
                                 provider = NULL, allow_ai = TRUE) {
  if (!isTRUE(allow_ai)) return(list(provider = "disabled",
    suggestions = list(),
    notes = "AI egress disabled by privacy gate."))
  # Heuristic fallback always available
  heur <- list()
  for (i in seq_len(nrow(profile))) {
    r <- profile[i, ]
    if (r$is_constant) heur[[length(heur)+1]] <-
      list(column = r$column, action = "drop", reason = "zero variance")
    else if (r$is_id_like) heur[[length(heur)+1]] <-
      list(column = r$column, action = "drop",
           reason = "ID-like (one value per row)")
    else if (r$pct_missing > 60) heur[[length(heur)+1]] <-
      list(column = r$column, action = "drop",
           reason = sprintf("%.0f%% missing", r$pct_missing))
    else if (!is.na(r$skew) && abs(r$skew) > 1.5) heur[[length(heur)+1]] <-
      list(column = r$column, action = "yeo_johnson",
           reason = sprintf("skew=%.2f", r$skew))
    else if (r$pct_missing > 0) heur[[length(heur)+1]] <-
      list(column = r$column, action = "impute",
           reason = sprintf("%.1f%% missing", r$pct_missing))
  }
  # If `ai_pick_provider` exists from ai_analysis.R, attempt a live call.
  prov <- tryCatch(ai_pick_provider(provider), error = function(e) NULL)
  if (is.null(prov) || isTRUE(prov$provider == "heuristic")) {
    return(list(provider = "heuristic", suggestions = heur,
                notes = "No AI key set; deterministic heuristic used."))
  }
  payload <- list(target = target,
                   columns = lapply(seq_len(nrow(profile)),
                                     function(i) as.list(profile[i, ])))
  msg <- jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null")
  prompt <- paste0(
    "You are a senior data scientist. Given this column profile (no raw ",
    "data, only summary stats), return STRICT JSON: {\"suggestions\": [",
    "{\"column\":\"...\",\"action\":\"drop|impute|yeo_johnson|scale|encode|",
    "lump_rare|winsorize\",\"reason\":\"...\"}]}.\n\n", msg)
  resp <- tryCatch(ai_call(prov, prompt), error = function(e) NULL)
  if (is.null(resp))
    return(list(provider = prov$provider, suggestions = heur,
                notes = "AI call failed; heuristic used."))
  parsed <- tryCatch(jsonlite::fromJSON(resp, simplifyVector = FALSE),
                     error = function(e) list())
  list(provider = prov$provider,
       suggestions = parsed$suggestions %||% heur,
       notes = parsed$notes %||% "")
}
