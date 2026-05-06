# =====================================================================
# MI pipeline test
#
# Synthesizes 5 implicates by adding noise to a target and a few
# predictors, runs mi_estimate with svymean and svyglm, and asserts
# that pooled SE >= average within-implicate SE (Rubin's between-
# imputation variance contributes), and that pooled output carries
# `fmi`, `lambda`.
#
# Skipped when 'mitools' or 'survey' is unavailable.
# =====================================================================

local({
  here <- if (file.exists("R/utils.R")) "." else
          if (file.exists("../R/utils.R")) ".." else
          stop("test must be run from repo root or tests/")
  source(file.path(here, "R", "utils.R"), local = TRUE)
  source(file.path(here, "R", "mi_pipeline.R"), local = TRUE)

  if (!requireNamespace("mitools", quietly = TRUE) ||
      !requireNamespace("survey", quietly = TRUE)) {
    cat("SKIP: mitools/survey not installed\n")
    cat("mi pipeline: SKIP\n")
    return(invisible(NULL))
  }

  set.seed(42)
  n <- 400
  M <- 5L
  base_x1 <- rnorm(n)
  base_x2 <- rnorm(n, mean = 1, sd = 0.7)
  base_y  <- 1.0 + 0.4 * base_x1 + 0.3 * base_x2 + rnorm(n, sd = 0.5)
  weight  <- runif(n, 0.5, 2.0)
  strata  <- sample(c("a","b","c"), n, TRUE)

  long_rows <- do.call(rbind, lapply(seq_len(M), function(k) {
    data.frame(
      implicate = k,
      y         = base_y  + rnorm(n, sd = 0.10),
      x1        = base_x1 + rnorm(n, sd = 0.05),
      x2        = base_x2 + rnorm(n, sd = 0.05),
      weight    = weight,
      strata    = strata,
      stringsAsFactors = FALSE
    )
  }))

  implicates <- mi_detect(long_rows, hint = list(kind = "long",
                                                  column = "implicate"))
  stopifnot(is.list(implicates))
  stopifnot(length(implicates) == M)
  cat("OK mi_detect: ", length(implicates), " implicates\n", sep = "")

  spec <- list(method = "standard", weight = "weight",
               strata = "strata", psu = NULL)
  designs <- mi_designs(implicates, spec)
  stopifnot(inherits(designs, "imputationList"))

  # ---- svymean -------------------------------------------------------
  est_mean <- mi_estimate(designs, formula = ~y, model = "svymean")
  stopifnot(is.data.frame(est_mean))
  required <- c("term","estimate","std.error","statistic","p.value",
                "df","fmi","riv","lambda")
  for (k in required) stopifnot(k %in% names(est_mean))
  cat("OK mi_estimate (svymean) returns Rubin columns; pooled est=",
      sprintf("%.4f", est_mean$estimate[1]), " SE=",
      sprintf("%.4f", est_mean$std.error[1]), "\n", sep = "")

  # Average within-implicate SE for the same target: build a quick
  # comparison using the per-imp svydesign list.
  per_se <- vapply(implicates, function(df) {
    des <- survey::svydesign(ids = ~1, weights = ~weight,
                              strata = ~strata, data = df, nest = TRUE)
    sm <- survey::svymean(~y, design = des, na.rm = TRUE)
    as.numeric(sqrt(diag(as.matrix(stats::vcov(sm)))))
  }, numeric(1))
  avg_within_se <- mean(per_se)
  stopifnot(est_mean$std.error[1] >= avg_within_se - 1e-9)
  cat("OK pooled SE >= avg within-imp SE (",
      sprintf("%.5f", est_mean$std.error[1]), " >= ",
      sprintf("%.5f", avg_within_se), ")\n", sep = "")

  # ---- svyglm --------------------------------------------------------
  est_glm <- mi_estimate(designs, formula = y ~ x1 + x2,
                          family = stats::gaussian, model = "svyglm")
  stopifnot(is.data.frame(est_glm))
  for (k in required) stopifnot(k %in% names(est_glm))
  stopifnot(nrow(est_glm) >= 2)  # intercept + 2 predictors
  cat("OK mi_estimate (svyglm) returns ", nrow(est_glm), " terms\n",
      sep = "")

  # Diagnostics returns a list with avg_fmi
  d <- mi_diagnostics(est_glm)
  stopifnot(is.list(d))
  stopifnot("avg_fmi" %in% names(d))
  cat("OK mi_diagnostics: avg_fmi=",
      sprintf("%.4f", d$avg_fmi),
      " avg_lambda=", sprintf("%.4f", d$avg_lambda %||% NA), "\n",
      sep = "")

  # ---- Wide format detection ----------------------------------------
  wide <- data.frame(weight = weight, strata = strata,
                      y_imp1 = base_y + rnorm(n, sd = 0.05),
                      y_imp2 = base_y + rnorm(n, sd = 0.05),
                      y_imp3 = base_y + rnorm(n, sd = 0.05),
                      y_imp4 = base_y + rnorm(n, sd = 0.05),
                      y_imp5 = base_y + rnorm(n, sd = 0.05),
                      stringsAsFactors = FALSE)
  imp_w <- mi_detect(wide)
  stopifnot(length(imp_w) == 5L)
  stopifnot("y" %in% names(imp_w[[1]]))
  cat("OK mi_detect wide format: ", length(imp_w),
      " implicates, target column rebuilt\n", sep = "")

  cat("mi pipeline: PASS\n")
})
