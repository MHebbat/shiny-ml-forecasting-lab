# =====================================================================
# Recipe builder — extensible, JSON-serialisable preprocessing pipeline.
#
# Each step is a list with:
#   id        : transform id (must match a branch in apply_recipe)
#   label     : human-readable label
#   targets   : either a character vector of column names OR a named
#               selector token: "all_numeric", "all_categorical",
#               "all_predictors", "all"
#   params    : list of step-specific parameters (optional)
#
# The active recipe is consumed by apply_recipe(), which:
#   * resolves selector tokens against the current data frame,
#   * removes the declared time column (state$meta$time_col) from
#     numeric transforms (only date_features / parse_datetime ever
#     touch the time column),
#   * applies the step deterministically,
#   * appends a one-line entry to the apply log per step.
#
# Author: Malik Hebbat
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

# ---- Step vocabulary -------------------------------------------------
# Single source of truth for: the available transforms, the parameter spec
# the UI should render, and the descriptions. apply_recipe() must have a
# branch for each id below.
RECIPE_STEP_VOCAB <- list(
  drop_zero_var      = list(label = "Drop zero-variance columns",
                              desc = "Remove columns with a single unique value."),
  drop_id_like       = list(label = "Drop ID-like columns",
                              desc = "Remove non-numeric columns where every row has a unique value."),
  drop_high_missing  = list(label = "Drop high-missing columns",
                              desc = "Remove columns whose missingness exceeds the threshold.",
                              params = list(threshold = 0.6)),
  impute             = list(label = "Impute missing (median / mode)",
                              desc = "Median for numeric, mode for categorical."),
  lump_rare          = list(label = "Lump rare categorical levels",
                              desc = "Categorical levels rarer than `threshold` collapsed to 'Other'.",
                              params = list(threshold = 0.01)),
  date_features      = list(label = "Extract date features",
                              desc = "Expand a Date / POSIXct column into year, month, day-of-week, week, quarter, is_weekend, days_since_epoch. Time column is permitted here."),
  parse_datetime     = list(label = "Parse string -> date / datetime",
                              desc = "Coerce a character column to Date or POSIXct. Time column is permitted here.",
                              params = list(format = "", tz = "UTC", as = "Date")),
  yeo_johnson        = list(label = "Yeo-Johnson power transform",
                              desc = "Negative-safe symmetric power transform (light approximation)."),
  log                = list(label = "Log transform",
                              desc = "log(x + offset). When the data has zeros, offset defaults to 1 so log1p is applied.",
                              params = list(offset = 0)),
  log10              = list(label = "Log10 transform",
                              desc = "log10(x + offset).",
                              params = list(offset = 0)),
  log2               = list(label = "Log2 transform",
                              desc = "log2(x + offset).",
                              params = list(offset = 0)),
  boxcox             = list(label = "Box-Cox transform",
                              desc = "Box-Cox; positive values only. Negative values per column are skipped with a warning.",
                              params = list(lambda = NA_real_)),
  sqrt               = list(label = "Square-root transform",
                              desc = "sqrt(max(x, 0)). Negative values are clipped to 0."),
  inverse            = list(label = "Inverse (1/x)",
                              desc = "1/x. Zeros are skipped."),
  zscore             = list(label = "Standardize (z-score)",
                              desc = "(x - mean) / sd."),
  min_max            = list(label = "Min-max scale to [0, 1]",
                              desc = "(x - min) / (max - min)."),
  robust_scale       = list(label = "Robust scale (median / IQR)",
                              desc = "(x - median) / IQR. IQR=0 falls back to z-score."),
  quantile_normal    = list(label = "Quantile-normalise to standard normal",
                              desc = "Rank-based map to N(0, 1)."),
  clip               = list(label = "Clip / winsorise",
                              desc = "Clamp values into [lower, upper]. Values may be quantiles (0-1) or absolute.",
                              params = list(lower = 0.01, upper = 0.99,
                                              by_quantile = TRUE)),
  bin_equal_width    = list(label = "Bin (equal width)",
                              desc = "Cut into n equal-width bins; outputs an ordered factor.",
                              params = list(n_bins = 5)),
  bin_quantile       = list(label = "Bin (equal frequency / quantile)",
                              desc = "Cut into n quantile bins; outputs an ordered factor.",
                              params = list(n_bins = 5)),
  target_encode      = list(label = "Target encode (mean / proportion)",
                              desc = "Encode categorical -> mean target (regression) or smoothed proportion (classification). Requires target.",
                              params = list(smoothing = 10)),
  ordinal_encode     = list(label = "Ordinal encode",
                              desc = "Map categorical -> integer per the supplied level ordering. Unknown levels become NA.",
                              params = list(levels = list())),
  frequency_encode   = list(label = "Frequency encode",
                              desc = "Categorical -> relative frequency in the column."),
  pca                = list(label = "PCA decomposition",
                              desc = "Replace numeric targets with the first n principal components.",
                              params = list(n_components = 3, var_explained = NA_real_)),
  one_hot            = list(label = "One-hot encode categoricals",
                              desc = "Expand non-numeric columns into dummy variables.")
)

# ---- Selector resolution ---------------------------------------------
# Resolves a step's `targets` against the current data frame.
# Always strips the declared time_col from numeric transforms; date
# transforms (date_features, parse_datetime) keep the time column.
.recipe_resolve_targets <- function(step, df, target = NULL, time_col = NULL) {
  tgt <- step$targets
  # NULL targets => default to all predictors (used by manual UI when no
  # scope is set). An EXPLICIT empty character vector means "no columns";
  # do NOT fall back to all_predictors — that would let drop_* steps wipe
  # the entire predictor matrix.
  if (is.null(tgt)) tgt <- "all_predictors"
  if (length(tgt) == 1 && is.character(tgt) && tgt %in%
      c("all", "all_numeric", "all_categorical", "all_predictors")) {
    cols <- switch(tgt,
      "all_numeric"     = names(df)[vapply(df, is.numeric, logical(1))],
      "all_categorical" = names(df)[vapply(df, function(c)
                                  is.factor(c) || is.character(c),
                                  logical(1))],
      "all_predictors"  = setdiff(names(df), target %||% character(0)),
      "all"             = names(df))
    resolved <- cols
  } else if (is.character(tgt) && length(tgt) == 0) {
    resolved <- character(0)
  } else {
    resolved <- intersect(as.character(tgt), names(df))
  }
  # Time-column protection — applies to *numeric* transforms only.
  date_safe <- c("date_features", "parse_datetime")
  if (!is.null(time_col) && !step$id %in% date_safe)
    resolved <- setdiff(resolved, time_col)
  resolved
}

# ---- Step constructors -----------------------------------------------
# Produce JSON-serialisable step entries. UI / AI suggestions / defaults
# all build steps via these helpers.
recipe_step <- function(id, targets = "all_predictors", params = list(),
                        label = NULL) {
  spec <- RECIPE_STEP_VOCAB[[id]]
  if (is.null(spec))
    stop(sprintf("recipe_step: unknown id `%s`", id))
  list(id = id,
       label = label %||% spec$label,
       targets = targets,
       params = modifyList(spec$params %||% list(), params %||% list()))
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
  if (o$drop_zero_var)
    steps[[length(steps)+1]] <- recipe_step("drop_zero_var",
      targets = profile$column[profile$is_constant])
  if (o$drop_id_like)
    steps[[length(steps)+1]] <- recipe_step("drop_id_like",
      targets = tgt_keep(profile$column[profile$is_id_like]))
  if (o$drop_high_missing)
    steps[[length(steps)+1]] <- recipe_step("drop_high_missing",
      targets = profile$column[profile$pct_missing/100 > o$high_missing_threshold],
      params = list(threshold = o$high_missing_threshold),
      label = sprintf("Drop columns with >%.0f%% missing",
                      100 * o$high_missing_threshold))
  if (o$impute)
    steps[[length(steps)+1]] <- recipe_step("impute",
      targets = profile$column[profile$n_missing > 0])
  if (o$lump_rare)
    steps[[length(steps)+1]] <- recipe_step("lump_rare",
      targets = profile$column[!profile$is_numeric & profile$n_unique > 6],
      params = list(threshold = o$rare_threshold),
      label = sprintf("Lump rare categorical levels (<%.1f%%)",
                      100 * o$rare_threshold))
  if (o$add_date_feat)
    steps[[length(steps)+1]] <- recipe_step("date_features",
      targets = profile$column[grepl("Date|POSIX", profile$class)])
  if (o$yeo_johnson)
    steps[[length(steps)+1]] <- recipe_step("yeo_johnson",
      targets = profile$column[profile$is_numeric & !is.na(profile$skew) &
                                 abs(profile$skew) > 1])
  if (o$winsorize)
    steps[[length(steps)+1]] <- recipe_step("clip",
      targets = profile$column[profile$is_numeric],
      params = list(lower = 0.01, upper = 0.99, by_quantile = TRUE))
  if (o$scale)
    steps[[length(steps)+1]] <- recipe_step("zscore",
      targets = tgt_keep(profile$column[profile$is_numeric]))
  if (o$encode_cats)
    steps[[length(steps)+1]] <- recipe_step("one_hot",
      targets = tgt_keep(profile$column[!profile$is_numeric]))
  steps
}

# ---- Apply pipeline to a data.frame ----------------------------------
apply_recipe <- function(df, steps, target = NULL, time_col = NULL,
                         scope_columns = NULL) {
  apply_log_lines <- list()
  add <- function(msg) apply_log_lines[[length(apply_log_lines) + 1]] <<- msg

  for (s in steps) {
    tgts <- .recipe_resolve_targets(s, df, target = target,
                                     time_col = time_col)
    # Restrict to user-chosen scope, unless the step explicitly named its
    # own column targets (i.e. is not using a selector token).
    used_token <- length(s$targets) == 1 && is.character(s$targets) &&
      s$targets %in% c("all", "all_numeric", "all_categorical",
                        "all_predictors")
    if (!is.null(scope_columns) && length(scope_columns) > 0 && used_token)
      tgts <- intersect(tgts, scope_columns)

    if (length(tgts) == 0) {
      add(sprintf("%s -> no eligible columns (skipped)", s$id))
      next
    }

    p <- s$params %||% list()
    sid <- s$id
    if (sid %in% c("drop_zero_var", "drop_id_like", "drop_high_missing")) {
      df[, tgts] <- NULL
      add(sprintf("%s -> dropped %d cols", sid, length(tgts)))
    } else if (sid == "impute") {
      for (col in tgts) {
        x <- df[[col]]
        if (sum(is.na(x)) == 0) next
        if (is.numeric(x))
          df[[col]][is.na(x)] <- stats::median(x, na.rm = TRUE)
        else {
          tab <- sort(table(x), decreasing = TRUE)
          mode_val <- if (length(tab) > 0) names(tab)[1] else NA
          df[[col]][is.na(x)] <- mode_val
        }
      }
      add(sprintf("impute -> filled %d cols", length(tgts)))
    } else if (sid == "lump_rare") {
      thr <- as.numeric(p$threshold %||% 0.01)
      for (col in tgts) {
        x <- as.character(df[[col]])
        if (length(x) == 0) next
        tab <- table(x) / length(x)
        rare <- names(tab)[tab < thr]
        if (length(rare) > 0) x[x %in% rare] <- "Other"
        df[[col]] <- as.factor(x)
      }
      add(sprintf("lump_rare -> processed %d cols", length(tgts)))
    } else if (sid == "date_features") {
      for (col in tgts) {
        x <- df[[col]]
        if (!inherits(x, c("Date", "POSIXt"))) next
        df[[paste0(col, "_year")]]  <- as.integer(format(x, "%Y"))
        df[[paste0(col, "_month")]] <- as.integer(format(x, "%m"))
        df[[paste0(col, "_dow")]]   <- as.integer(format(x, "%u"))
        df[[paste0(col, "_week")]]  <- as.integer(format(x, "%V"))
        df[[paste0(col, "_qtr")]]   <- ((as.integer(format(x, "%m"))-1) %/% 3) + 1
        df[[paste0(col, "_is_weekend")]] <-
          as.integer(as.integer(format(x, "%u")) %in% c(6L, 7L))
        df[[paste0(col, "_days_since_epoch")]] <-
          as.integer(as.Date(x) - as.Date("1970-01-01"))
        # Keep the time column in place — only date features are added.
        if (!identical(col, time_col)) df[[col]] <- NULL
      }
      add(sprintf("date_features -> expanded %d cols", length(tgts)))
    } else if (sid == "parse_datetime") {
      fmt <- as.character(p$format %||% "")
      tz <- as.character(p$tz %||% "UTC")
      as_kind <- tolower(as.character(p$as %||% "Date"))
      for (col in tgts) {
        x <- df[[col]]
        if (inherits(x, c("Date", "POSIXt"))) next
        s <- as.character(x)
        v <- if (as_kind == "posix" || as_kind == "posixct" ||
                 as_kind == "datetime") {
          if (nzchar(fmt)) as.POSIXct(s, format = fmt, tz = tz)
          else as.POSIXct(s, tz = tz)
        } else {
          if (nzchar(fmt)) as.Date(s, format = fmt)
          else as.Date(s)
        }
        df[[col]] <- v
      }
      add(sprintf("parse_datetime -> coerced %d cols", length(tgts)))
    } else if (sid == "yeo_johnson") {
      yeo <- function(v) sign(v) * log1p(abs(v))
      for (col in tgts) df[[col]] <- yeo(as.numeric(df[[col]]))
      add(sprintf("yeo_johnson -> transformed %d cols", length(tgts)))
    } else if (sid %in% c("log", "log10", "log2")) {
      offset <- as.numeric(p$offset %||% 0)
      base_fn <- switch(sid,
        "log"   = log,
        "log10" = log10,
        "log2"  = log2)
      for (col in tgts) {
        x <- as.numeric(df[[col]])
        eff_offset <- offset
        if (any(x == 0, na.rm = TRUE) && eff_offset == 0) eff_offset <- 1
        if (any(x + eff_offset <= 0, na.rm = TRUE)) {
          add(sprintf("%s -> %s contains non-positive values; min shifted",
                       sid, col))
          eff_offset <- eff_offset - min(x, na.rm = TRUE) + 1e-9
        }
        df[[col]] <- base_fn(x + eff_offset)
      }
      add(sprintf("%s -> transformed %d cols", sid, length(tgts)))
    } else if (sid == "boxcox") {
      lambda_in <- p$lambda
      transformed <- 0L; skipped <- 0L
      for (col in tgts) {
        x <- as.numeric(df[[col]])
        if (any(x <= 0, na.rm = TRUE)) {
          add(sprintf("boxcox -> %s has non-positive values; skipped", col))
          skipped <- skipped + 1L
          next
        }
        if (is.null(lambda_in) || !is.finite(lambda_in) ||
            length(lambda_in) == 0 || is.na(lambda_in)) {
          # Lightweight lambda search via ML on log-likelihood
          ll <- function(l) {
            xt <- if (abs(l) < 1e-9) base::log(x) else (x^l - 1) / l
            v <- stats::var(xt, na.rm = TRUE)
            if (!is.finite(v) || v <= 0) return(Inf)
            -((l - 1) * sum(base::log(x), na.rm = TRUE) -
                length(x) / 2 * base::log(v))
          }
          lambda <- tryCatch(
            stats::optimize(ll, c(-2, 2))$minimum,
            error = function(e) 0)
        } else lambda <- as.numeric(lambda_in)
        df[[col]] <- if (abs(lambda) < 1e-9) base::log(x) else (x^lambda - 1) / lambda
        transformed <- transformed + 1L
      }
      add(sprintf("boxcox -> transformed %d / skipped %d", transformed, skipped))
    } else if (sid == "sqrt") {
      for (col in tgts) {
        x <- as.numeric(df[[col]])
        df[[col]] <- sqrt(pmax(x, 0))
      }
      add(sprintf("sqrt -> transformed %d cols", length(tgts)))
    } else if (sid == "inverse") {
      for (col in tgts) {
        x <- as.numeric(df[[col]])
        x[x == 0] <- NA_real_
        df[[col]] <- 1 / x
      }
      add(sprintf("inverse -> transformed %d cols", length(tgts)))
    } else if (sid %in% c("zscore", "scale")) {
      for (col in tgts) {
        x <- as.numeric(df[[col]])
        m <- mean(x, na.rm = TRUE); sdv <- stats::sd(x, na.rm = TRUE)
        if (is.finite(sdv) && sdv > 0) df[[col]] <- (x - m) / sdv
      }
      add(sprintf("zscore -> standardized %d cols", length(tgts)))
    } else if (sid == "min_max") {
      for (col in tgts) {
        x <- as.numeric(df[[col]])
        rng <- range(x, na.rm = TRUE, finite = TRUE)
        if (rng[2] > rng[1])
          df[[col]] <- (x - rng[1]) / (rng[2] - rng[1])
      }
      add(sprintf("min_max -> scaled %d cols", length(tgts)))
    } else if (sid == "robust_scale") {
      for (col in tgts) {
        x <- as.numeric(df[[col]])
        med <- stats::median(x, na.rm = TRUE)
        q <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
        iqr <- q[2] - q[1]
        if (is.finite(iqr) && iqr > 0)
          df[[col]] <- (x - med) / iqr
        else {
          sdv <- stats::sd(x, na.rm = TRUE)
          if (is.finite(sdv) && sdv > 0)
            df[[col]] <- (x - mean(x, na.rm = TRUE)) / sdv
        }
      }
      add(sprintf("robust_scale -> processed %d cols", length(tgts)))
    } else if (sid == "quantile_normal") {
      for (col in tgts) {
        x <- as.numeric(df[[col]])
        ok <- !is.na(x)
        r <- rank(x[ok], ties.method = "average") / (sum(ok) + 1)
        z <- stats::qnorm(r)
        df[[col]][ok] <- z
      }
      add(sprintf("quantile_normal -> mapped %d cols", length(tgts)))
    } else if (sid %in% c("clip", "winsorize")) {
      lo <- as.numeric(p$lower %||% 0.01)
      hi <- as.numeric(p$upper %||% 0.99)
      by_q <- isTRUE(p$by_quantile %||% TRUE)
      for (col in tgts) {
        x <- as.numeric(df[[col]])
        bounds <- if (by_q) stats::quantile(x, c(lo, hi), na.rm = TRUE) else
                   c(lo, hi)
        x[x < bounds[1]] <- bounds[1]
        x[x > bounds[2]] <- bounds[2]
        df[[col]] <- x
      }
      add(sprintf("clip -> bounded %d cols", length(tgts)))
    } else if (sid == "bin_equal_width") {
      n_bins <- max(2L, as.integer(p$n_bins %||% 5L))
      for (col in tgts) {
        x <- as.numeric(df[[col]])
        df[[col]] <- cut(x, breaks = n_bins, include.lowest = TRUE,
                          ordered_result = TRUE)
      }
      add(sprintf("bin_equal_width -> %d bins on %d cols",
                   n_bins, length(tgts)))
    } else if (sid == "bin_quantile") {
      n_bins <- max(2L, as.integer(p$n_bins %||% 5L))
      for (col in tgts) {
        x <- as.numeric(df[[col]])
        br <- unique(stats::quantile(x, probs = seq(0, 1, length.out = n_bins + 1),
                                     na.rm = TRUE))
        if (length(br) < 3) next
        df[[col]] <- cut(x, breaks = br, include.lowest = TRUE,
                          ordered_result = TRUE)
      }
      add(sprintf("bin_quantile -> %d bins on %d cols",
                   n_bins, length(tgts)))
    } else if (sid == "target_encode") {
      if (is.null(target) || !target %in% names(df)) {
        add("target_encode -> no target available; skipped"); next
      }
      y <- df[[target]]
      smoothing <- as.numeric(p$smoothing %||% 10)
      is_class <- !is.numeric(y)
      global_mean <- if (is_class) mean(as.character(y) ==
        names(sort(table(y), decreasing = TRUE))[1], na.rm = TRUE) else
        mean(as.numeric(y), na.rm = TRUE)
      for (col in tgts) {
        if (col == target) next
        x <- as.character(df[[col]])
        tbl <- if (is_class) {
          # Smoothed class proportion of the most common class
          top_class <- names(sort(table(y), decreasing = TRUE))[1]
          tab <- tapply(as.character(y) == top_class, x, mean, na.rm = TRUE)
          n   <- tapply(rep(1, length(y)), x, sum, na.rm = TRUE)
          (tab * n + global_mean * smoothing) / (n + smoothing)
        } else {
          tab <- tapply(as.numeric(y), x, mean, na.rm = TRUE)
          n   <- tapply(rep(1, length(y)), x, sum, na.rm = TRUE)
          (tab * n + global_mean * smoothing) / (n + smoothing)
        }
        df[[col]] <- as.numeric(tbl[x])
        df[[col]][is.na(df[[col]])] <- global_mean
      }
      add(sprintf("target_encode -> encoded %d cols", length(tgts)))
    } else if (sid == "ordinal_encode") {
      lv <- p$levels
      for (col in tgts) {
        x <- as.character(df[[col]])
        if (is.list(lv) && !is.null(lv[[col]])) {
          ord <- as.character(lv[[col]])
        } else if (is.character(lv) && length(lv) > 0) {
          ord <- as.character(lv)
        } else {
          ord <- sort(unique(stats::na.omit(x)))
        }
        df[[col]] <- match(x, ord)
      }
      add(sprintf("ordinal_encode -> encoded %d cols", length(tgts)))
    } else if (sid == "frequency_encode") {
      for (col in tgts) {
        x <- as.character(df[[col]])
        tab <- table(x) / length(x)
        df[[col]] <- as.numeric(tab[x])
      }
      add(sprintf("frequency_encode -> encoded %d cols", length(tgts)))
    } else if (sid == "pca") {
      n_comp <- as.integer(p$n_components %||% 3L)
      var_exp <- p$var_explained
      tgts_num <- tgts[vapply(df[tgts], is.numeric, logical(1))]
      if (length(tgts_num) < 2) {
        add("pca -> need at least 2 numeric columns; skipped"); next
      }
      X <- as.matrix(df[, tgts_num, drop = FALSE])
      X <- X[, apply(X, 2, function(c) is.finite(stats::sd(c)) &&
                                          stats::sd(c) > 0), drop = FALSE]
      if (ncol(X) < 2) {
        add("pca -> not enough variance; skipped"); next
      }
      pr <- tryCatch(stats::prcomp(X, center = TRUE, scale. = TRUE),
                     error = function(e) NULL)
      if (is.null(pr)) { add("pca -> prcomp failed; skipped"); next }
      if (!is.null(var_exp) && is.finite(var_exp) && !is.na(var_exp)) {
        cum <- cumsum(pr$sdev^2) / sum(pr$sdev^2)
        n_comp <- max(1L, which(cum >= as.numeric(var_exp))[1])
      }
      n_comp <- min(n_comp, ncol(pr$x))
      pcs <- as.data.frame(pr$x[, seq_len(n_comp), drop = FALSE])
      names(pcs) <- paste0("pca_", seq_len(n_comp))
      df[, colnames(X)] <- NULL
      df <- cbind(df, pcs)
      add(sprintf("pca -> %d cols -> %d components", ncol(X), n_comp))
    } else if (sid %in% c("one_hot", "encode")) {
      for (col in tgts) {
        x <- as.factor(as.character(df[[col]]))
        if (length(levels(x)) <= 1) next
        mm <- stats::model.matrix(~ x - 1)
        colnames(mm) <- gsub("^x", paste0(col, "_"), colnames(mm))
        df[[col]] <- NULL
        df <- cbind(df, as.data.frame(mm))
      }
      add(sprintf("one_hot -> encoded %d cols", length(tgts)))
    } else {
      add(sprintf("UNKNOWN step `%s` -> ignored", sid))
    }
  }
  list(df = df, log = apply_log_lines)
}

# ---- AI prep assist (column profile only - never raw rows) -----------
# Each suggestion now carries a fully-formed step that can be applied
# directly via apply_recipe(). The UI lets the user click "Apply" or
# "Apply all".
ai_prep_suggestions <- function(profile, target = NULL,
                                 provider = NULL, allow_ai = TRUE) {
  if (!isTRUE(allow_ai)) return(list(provider = "disabled",
    suggestions = list(),
    notes = "AI egress disabled by privacy gate."))

  to_step <- function(action, column, params = list(), reason = "") {
    spec <- RECIPE_STEP_VOCAB[[action]]
    label <- if (!is.null(spec)) spec$label else action
    list(column = column, action = action, reason = reason,
         step = recipe_step(action, targets = column, params = params,
                              label = label))
  }

  # Heuristic fallback always available
  heur <- list()
  for (i in seq_len(nrow(profile))) {
    r <- profile[i, ]
    if (r$is_constant)
      heur[[length(heur)+1]] <- to_step("drop_zero_var", r$column,
        reason = "zero variance")
    else if (r$is_id_like)
      heur[[length(heur)+1]] <- to_step("drop_id_like", r$column,
        reason = "ID-like (one value per row)")
    else if (r$pct_missing > 60)
      heur[[length(heur)+1]] <- to_step("drop_high_missing", r$column,
        reason = sprintf("%.0f%% missing", r$pct_missing))
    else if (!is.na(r$skew) && abs(r$skew) > 1.5)
      heur[[length(heur)+1]] <- to_step("yeo_johnson", r$column,
        reason = sprintf("skew=%.2f", r$skew))
    else if (r$pct_missing > 0)
      heur[[length(heur)+1]] <- to_step("impute", r$column,
        reason = sprintf("%.1f%% missing", r$pct_missing))
  }

  prov <- tryCatch(ai_pick_provider(provider), error = function(e) NULL)
  if (is.null(prov) || isTRUE(prov$provider == "heuristic")) {
    return(list(provider = "heuristic", suggestions = heur,
                notes = "No AI key set; deterministic heuristic used."))
  }
  payload <- list(target = target,
                   columns = lapply(seq_len(nrow(profile)),
                                     function(i) as.list(profile[i, ])),
                   actions = names(RECIPE_STEP_VOCAB))
  msg <- jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null")
  prompt <- paste0(
    "You are a senior data scientist. Given this column profile (no raw ",
    "data, only summary stats), return STRICT JSON: {\"suggestions\": [",
    "{\"column\":\"...\",\"action\":\"<one of `actions`>\",\"reason\":\"...\",",
    "\"params\":{...optional...}}]}.\n\n", msg)
  resp <- tryCatch(ai_call(prov, prompt), error = function(e) NULL)
  if (is.null(resp))
    return(list(provider = prov$provider, suggestions = heur,
                notes = "AI call failed; heuristic used."))
  parsed <- tryCatch(jsonlite::fromJSON(resp, simplifyVector = FALSE),
                     error = function(e) list())
  raw_sugs <- parsed$suggestions
  if (length(raw_sugs) == 0)
    return(list(provider = prov$provider, suggestions = heur,
                notes = "AI returned no suggestions; heuristic used."))
  hydrated <- lapply(raw_sugs, function(s) {
    action <- s$action
    if (is.null(action) || !action %in% names(RECIPE_STEP_VOCAB))
      return(NULL)
    to_step(action, s$column %||% "all_predictors",
             params = s$params %||% list(),
             reason = s$reason %||% "")
  })
  hydrated <- Filter(Negate(is.null), hydrated)
  if (length(hydrated) == 0)
    return(list(provider = prov$provider, suggestions = heur,
                notes = "AI returned only unrecognised actions; heuristic used."))
  list(provider = prov$provider,
       suggestions = hydrated,
       notes = parsed$notes %||% "")
}
