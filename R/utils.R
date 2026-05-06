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
read_uploaded <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  switch(ext,
    "csv"     = readr::read_csv(path, show_col_types = FALSE),
    "tsv"     = readr::read_tsv(path, show_col_types = FALSE),
    "txt"     = readr::read_delim(path, show_col_types = FALSE),
    "xlsx"    = { req_pkg("readxl"); readxl::read_excel(path) },
    "xls"     = { req_pkg("readxl"); readxl::read_excel(path) },
    "parquet" = { req_pkg("arrow");  arrow::read_parquet(path) },
    "json"    = jsonlite::fromJSON(path, flatten = TRUE),
    "rds"     = readRDS(path),
    stop(sprintf("Unsupported file extension: .%s", ext))
  ) |> as.data.frame()
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
