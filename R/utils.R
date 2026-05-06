# =====================================================================
# Shared helpers
# =====================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

# ---- Robust date / datetime parsing ---------------------------------
# safe_as_date / safe_as_posix wrap base parsers in tryCatch and fall
# back to lubridate::parse_date_time with a battery of common orders.
# Returns vector of NA if every strategy fails (never throws).
# `column_name` is purely for error messaging surfaced via .date_warn().
.date_orders <- c("Ymd", "dmY", "mdY", "Y-m-d", "d-m-Y", "m/d/Y", "d/m/Y",
                  "Ymd HMS", "Ymd HM", "dmY HMS", "dmY HM",
                  "mdY HMS", "mdY HM", "Y-m-d H:M:S", "d-m-Y H:M:S")

.flag_date_warning <- function(column_name, sample) {
  msg <- sprintf(
    "Could not parse '%s' as a date — %s. Declare its format in Data Prep -> parse_datetime.",
    column_name, paste(utils::head(sample, 3), collapse = "; "))
  message("[date-parse] ", msg)
  msg
}

safe_as_date <- function(x, format = NULL, column_name = "(unknown)") {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  s <- as.character(x)
  out <- tryCatch({
    if (!is.null(format) && nzchar(format)) as.Date(s, format = format)
    else suppressWarnings(as.Date(s))
  }, error = function(e) rep(NA, length(s)))
  if (any(!is.na(s) & is.na(out))) {
    if (requireNamespace("lubridate", quietly = TRUE)) {
      lub <- tryCatch(
        suppressWarnings(lubridate::parse_date_time(s, orders = .date_orders, quiet = TRUE)),
        error = function(e) rep(NA, length(s)))
      idx <- !is.na(s) & is.na(out) & !is.na(lub)
      if (any(idx)) out[idx] <- as.Date(lub[idx])
    }
    if (any(!is.na(s) & is.na(out))) {
      bad <- s[!is.na(s) & is.na(out)]
      .flag_date_warning(column_name, unique(bad))
    }
  }
  out
}

safe_as_posix <- function(x, format = NULL, tz = "UTC", column_name = "(unknown)") {
  if (inherits(x, "POSIXt")) return(x)
  s <- as.character(x)
  out <- tryCatch({
    if (!is.null(format) && nzchar(format)) as.POSIXct(s, format = format, tz = tz)
    else suppressWarnings(as.POSIXct(s, tz = tz))
  }, error = function(e) rep(as.POSIXct(NA), length(s)))
  if (any(!is.na(s) & is.na(out))) {
    if (requireNamespace("lubridate", quietly = TRUE)) {
      lub <- tryCatch(
        suppressWarnings(lubridate::parse_date_time(s, orders = .date_orders,
                                                    tz = tz, quiet = TRUE)),
        error = function(e) rep(as.POSIXct(NA, tz = tz), length(s)))
      idx <- !is.na(s) & is.na(out) & !is.na(lub)
      if (any(idx)) out[idx] <- as.POSIXct(lub[idx], tz = tz)
    }
    if (any(!is.na(s) & is.na(out))) {
      bad <- s[!is.na(s) & is.na(out)]
      .flag_date_warning(column_name, unique(bad))
    }
  }
  out
}

# ---- Task auto-detection ---------------------------------------------
detect_task_type <- function(y, time_col_present = FALSE) {
  if (time_col_present) return("time_series")
  if (is.null(y)) return("regression")
  if (is.factor(y) || is.character(y) || is.logical(y)) {
    n_lvl <- length(unique(stats::na.omit(y)))
    if (n_lvl == 2) return("binary_classification")
    return("multiclass_classification")
  }
  if (is.numeric(y)) {
    yy <- stats::na.omit(y)
    if (all(yy == as.integer(yy)) && all(yy >= 0) &&
        length(unique(yy)) > 2 && max(yy) < 1e6) return("count")
    if (all(yy >= 0 & yy <= 1)) return("proportion")
    return("regression")
  }
  "regression"
}

# ---- File reading -----------------------------------------------------
# Supports tabular, columnar and statistical formats. For Stata/SPSS/SAS
# we keep variable + value labels via the `labelled` package so the
# survey/codebook tabs can render them.
read_uploaded <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  df <- switch(ext,
    "csv"     = readr::read_csv(path, show_col_types = FALSE),
    "tsv"     = readr::read_tsv(path, show_col_types = FALSE),
    "txt"     = readr::read_delim(path, show_col_types = FALSE),
    "xlsx"    = { req_pkg("readxl"); readxl::read_excel(path) },
    "xls"     = { req_pkg("readxl"); readxl::read_excel(path) },
    "parquet" = { req_pkg("arrow");  arrow::read_parquet(path) },
    "json"    = jsonlite::fromJSON(path, flatten = TRUE),
    "rds"     = readRDS(path),
    "dta"     = { req_pkg("haven"); haven::read_dta(path) },
    "sav"     = { req_pkg("haven"); haven::read_sav(path) },
    "por"     = { req_pkg("haven"); haven::read_por(path) },
    "sas7bdat" = { req_pkg("haven"); haven::read_sas(path) },
    "xpt"     = { req_pkg("haven"); haven::read_xpt(path) },
    stop(sprintf("Unsupported file extension: .%s", ext))
  )
  as.data.frame(df)
}

# ---- Variable & value label helpers (haven / labelled) ---------------
extract_labels <- function(df) {
  out <- list(var_labels = list(), value_labels = list())
  for (nm in names(df)) {
    col <- df[[nm]]
    vlab <- attr(col, "label", exact = TRUE)
    if (!is.null(vlab) && nzchar(vlab)) out$var_labels[[nm]] <- as.character(vlab)
    val_lab <- attr(col, "labels", exact = TRUE)
    if (!is.null(val_lab) && length(val_lab) > 0) {
      out$value_labels[[nm]] <- setNames(as.character(names(val_lab)),
                                          as.character(unname(val_lab)))
    }
  }
  out
}

# Convert haven_labelled columns into plain factors / numerics so models
# don't choke; preserves labels in attributes for the codebook tab.
harmonize_labelled <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      val_lab <- attr(col, "labels", exact = TRUE)
      if (!is.null(val_lab) && length(val_lab) <= 50) {
        df[[nm]] <- factor(unclass(col),
                            levels = unname(val_lab),
                            labels = names(val_lab))
      } else {
        df[[nm]] <- as.numeric(unclass(col))
      }
      # Re-attach variable label
      attr(df[[nm]], "label") <- attr(col, "label", exact = TRUE)
    }
  }
  df
}

# Heuristic detector for survey design columns based on common naming
# conventions in EN/DE social-science panels (PHF, SOEP, ESS, etc.)
detect_survey_columns <- function(df, labels = list()) {
  nm <- tolower(names(df))
  vlab <- vapply(seq_along(nm), function(i) {
    l <- labels$var_labels[[names(df)[i]]]
    if (is.null(l)) "" else tolower(l)
  }, character(1))
  hits <- function(needles) {
    pat <- paste(needles, collapse = "|")
    grepl(pat, nm) | grepl(pat, vlab)
  }
  list(
    weight  = names(df)[hits(c("weight", "\\bwt\\b", "gewicht", "hochrechnung"))],
    strata  = names(df)[hits(c("strata", "stratum", "schicht"))],
    psu     = names(df)[hits(c("psu", "cluster", "primary sampling"))],
    id      = names(df)[hits(c("^id$", "persnr", "hhnr", "respondent", "caseid"))],
    wave    = names(df)[hits(c("wave", "welle", "period", "^year$", "\\byr\\b"))]
  )
}

req_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf("Package '%s' is required. install.packages('%s')", pkg, pkg))
}

# ---- Training-shape guards -------------------------------------------
# Called from the top of each fit_* branch to surface clear errors when
# upstream Data Prep / validation-splitting produced an empty or
# misshapen training set.
.assert_train_shapes <- function(y, X, model_label) {
  ny <- length(y); nX <- if (is.null(dim(X))) length(X) else nrow(X)
  if (ny == 0L || nX == 0L)
    stop(sprintf("Cannot train %s: training set is empty (y=%d, X=%d). ",
                 model_label, ny, nX),
         "This usually means Data Prep removed all rows or all predictors. ",
         "Check the recipe and the validation split.", call. = FALSE)
  if (ny != nX)
    stop(sprintf("Cannot train %s: y has %d obs but X has %d rows (internal pipeline bug).",
                 model_label, ny, nX), call. = FALSE)
  invisible(TRUE)
}

.assert_lstm_shapes <- function(x, y, model_label = "LSTM") {
  if (length(dim(x)) != 3L)
    stop(sprintf("Cannot train %s: expected 3D array (samples, timesteps, features), got dims=%s",
                 model_label, paste(dim(x), collapse = "x")), call. = FALSE)
  if (dim(x)[1] == 0L || length(y) == 0L)
    stop(sprintf("Cannot train %s: empty training arrays. Check timesteps vs n_rows and val_split.",
                 model_label), call. = FALSE)
  if (dim(x)[1] != length(y))
    stop(sprintf("Cannot train %s: x has %d windows but y has %d targets.",
                 model_label, dim(x)[1], length(y)), call. = FALSE)
  invisible(TRUE)
}

# ---- Metric helpers ---------------------------------------------------
calc_regression_metrics <- function(actual, predicted) {
  ok <- complete.cases(actual, predicted)
  a <- as.numeric(actual)[ok]; p <- as.numeric(predicted)[ok]
  if (length(a) == 0) return(list(rmse = NA, mae = NA, mape = NA, r2 = NA))
  rmse <- sqrt(mean((a - p)^2))
  mae  <- mean(abs(a - p))
  mape <- mean(abs((a - p) / ifelse(a == 0, NA, a)), na.rm = TRUE) * 100
  ss_res <- sum((a - p)^2); ss_tot <- sum((a - mean(a))^2)
  r2 <- if (ss_tot > 0) 1 - ss_res/ss_tot else NA_real_
  list(rmse = round(rmse, 4), mae = round(mae, 4),
       mape = round(mape, 4), r2 = round(r2, 4))
}

calc_classification_metrics <- function(actual, predicted, probs = NULL) {
  actual <- as.factor(actual); predicted <- as.factor(predicted)
  acc <- mean(actual == predicted, na.rm = TRUE)
  cm  <- table(actual, predicted)
  out <- list(accuracy = round(acc, 4))
  if (length(levels(actual)) == 2) {
    pos <- levels(actual)[2]
    tp <- sum(actual == pos & predicted == pos)
    fp <- sum(actual != pos & predicted == pos)
    fn <- sum(actual == pos & predicted != pos)
    prec <- ifelse(tp+fp == 0, NA, tp/(tp+fp))
    rec  <- ifelse(tp+fn == 0, NA, tp/(tp+fn))
    f1   <- ifelse(is.na(prec)|is.na(rec)|prec+rec==0, NA, 2*prec*rec/(prec+rec))
    out$precision <- round(prec, 4); out$recall <- round(rec, 4); out$f1 <- round(f1, 4)
  }
  out
}

# ---- Pretty printing --------------------------------------------------
fmt_metrics <- function(m) {
  if (is.null(m) || length(m) == 0) return("—")
  paste(sprintf("%s=%s", names(m), unlist(m)), collapse = " · ")
}

# ---- Frequency to forecast horizon helper ----------------------------
horizon_label <- function(freq, h) {
  unit <- switch(freq,
    "daily" = "day", "weekly" = "week", "monthly" = "month",
    "quarterly" = "quarter", "yearly" = "year", "step")
  paste(h, paste0(unit, ifelse(h == 1, "", "s")), "ahead")
}

# Simple toast helper
flash <- function(msg, type = "default") {
  showNotification(msg, type = type, duration = 4)
}

# ---- Bootstrap helpers -----------------------------------------------
# Pure-R bootstrap of a scalar metric. Returns the point estimate, SE, and
# percentile-method 95% confidence interval. Used by the Diagnostics card
# in the Results Dashboard.
bootstrap_metric <- function(actual, predicted,
                              metric_fn = function(a, p)
                                sqrt(mean((a - p)^2, na.rm = TRUE)),
                              R = 200L, seed = 42L) {
  ok <- complete.cases(actual, predicted)
  a <- as.numeric(actual)[ok]; p <- as.numeric(predicted)[ok]
  n <- length(a)
  if (n < 5) return(list(estimate = NA_real_, se = NA_real_,
                          lo = NA_real_, hi = NA_real_, R = 0L, n = n))
  set.seed(seed)
  vals <- numeric(R)
  for (i in seq_len(R)) {
    idx <- sample.int(n, n, replace = TRUE)
    vals[i] <- tryCatch(metric_fn(a[idx], p[idx]), error = function(e) NA_real_)
  }
  vals <- vals[is.finite(vals)]
  if (length(vals) < 10) return(list(estimate = metric_fn(a, p),
                                       se = NA_real_, lo = NA_real_,
                                       hi = NA_real_, R = length(vals), n = n))
  q <- stats::quantile(vals, c(0.025, 0.975), na.rm = TRUE)
  list(estimate = metric_fn(a, p),
       se = stats::sd(vals, na.rm = TRUE),
       lo = unname(q[1]), hi = unname(q[2]),
       R = length(vals), n = n)
}
