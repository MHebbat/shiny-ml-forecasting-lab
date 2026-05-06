# =====================================================================
# Shared helpers
# =====================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

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
