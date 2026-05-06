# =====================================================================
# Recipe smoke regression test
#
# Guards against the regression where Data Prep silently dropped every
# column except the observation id and the target. The default flow MUST
# preserve the predictor matrix.
#
# Run with:  Rscript -e "source('tests/test_recipe_smoke.R')"
# =====================================================================

local({
  here <- if (file.exists("R/utils.R")) "." else
          if (file.exists("../R/utils.R")) ".." else
          stop("test must be run from repo root or tests/")
  source(file.path(here, "R", "utils.R"))
  source(file.path(here, "R", "recipe_builder.R"))

  set.seed(1)
  df <- data.frame(
    obs_id   = 1:100,
    date     = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 100),
    x_num1   = stats::rnorm(100),
    x_num2   = stats::rexp(100, 1),
    x_cat    = sample(c("A", "B", "C"), 100, TRUE),
    x_skewed = stats::rlnorm(100, 0, 1),
    y        = stats::rnorm(100),
    stringsAsFactors = FALSE
  )

  prof  <- profile_columns(df, target = "y", time_col = "date")
  steps <- default_steps(prof, target = "y")
  res   <- apply_recipe(df, steps = steps, target = "y", time_col = "date")

  stopifnot(is.list(res), !is.null(res$df))
  stopifnot("y" %in% names(res$df))
  stopifnot(ncol(res$df) > 2)

  predictor_cols <- setdiff(names(res$df), c("y", "obs_id"))
  if (length(predictor_cols) < 3) {
    cat("FAIL: predictor_cols < 3. Got: ",
        paste(names(res$df), collapse = ", "), "\n")
    stop("recipe regression: too few predictors retained")
  }

  # Time-column related: 'date' or its derived features must survive.
  has_date_signal <- "date" %in% names(res$df) ||
    any(grepl("^date_", names(res$df)))
  stopifnot(has_date_signal)

  # Numeric predictors x_num1 / x_num2 / x_skewed (or transformed forms)
  # must still be present — none of them are zero-var or ID-like.
  for (nm in c("x_num1", "x_num2", "x_skewed"))
    stopifnot(nm %in% names(res$df))

  # Categorical x_cat: either present (untouched) or expanded to one-hot.
  cat_signal <- "x_cat" %in% names(res$df) ||
    any(grepl("^x_cat_", names(res$df)))
  stopifnot(cat_signal)

  cat("OK predictors retained: ",
      paste(predictor_cols, collapse = ", "), "\n")

  # Sanity: empty-target drop step is a no-op, not a wipeout.
  empty_drop <- recipe_step("drop_zero_var", targets = character(0))
  res2 <- apply_recipe(df, steps = list(empty_drop),
                       target = "y", time_col = "date")
  stopifnot(identical(sort(names(res2$df)), sort(names(df))))
  cat("OK empty drop_zero_var is a no-op\n")

  cat("recipe smoke: PASS\n")
})
