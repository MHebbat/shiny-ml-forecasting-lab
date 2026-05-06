# =====================================================================
# Project save/load round-trip test
#
# Saves a synthetic state, loads it back into a fresh container, and
# verifies recipe / survey design / run params / manifest survive the
# round-trip.
# =====================================================================

local({
  here <- if (file.exists("R/utils.R")) "." else
          if (file.exists("../R/utils.R")) ".." else
          stop("test must be run from repo root or tests/")
  source(file.path(here, "R", "utils.R"), local = TRUE)
  source(file.path(here, "R", "manifest.R"), local = TRUE)
  source(file.path(here, "R", "io_project.R"), local = TRUE)

  base <- tempfile("shinyml_proj_test_")
  dir.create(base, recursive = TRUE)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  # Build a state-like environment with the fields project_save reads
  state <- new.env()
  state$datasets <- list()
  state$active_id <- NULL
  state$raw_data <- data.frame(x = 1:10, y = stats::rnorm(10))
  state$dataset_name <- "demo_ds"
  state$dataset_id <- 7L
  state$meta <- list(target = "y", task_type = "regression",
                      frequency = "monthly",
                      sample_weights = "wgt", strata = "s", psu = "p",
                      replicate_weights = c("wr_1","wr_2"))
  state$labels <- list()
  state$survey_hints <- list()
  state$recipe <- list(list(id = "drop_zero_var", targets = "all_predictors",
                              params = list(), label = "drop"))
  state$prep_log <- list("drop_zero_var -> 0 cols")
  state$survey_design <- list(method = "replicate", weight = "wgt",
                                strata = "s", psu = "p",
                                repweights_pattern = "^wr_",
                                repdesign_type = "Fay",
                                kind = "replicate")
  state$last_model <- list(model_id = "lm_reg", metrics = list(RMSE = 1.2),
                             actual = stats::rnorm(20),
                             predicted = stats::rnorm(20),
                             train_config = list(validation = "holdout",
                                                  seed = 42))
  state$last_params <- list(intercept = TRUE)
  state$last_run_id <- 99L
  state$plot_theme <- "bundesbank"
  state$chrome_theme <- "bundesbank"

  bundle_path <- project_save(state, name = "rt_test", base_dir = base)
  stopifnot(dir.exists(bundle_path))
  stopifnot(any(grepl("\\.shinyml\\.rds$", list.files(bundle_path))))
  stopifnot(any(grepl("\\.shinyml\\.json$", list.files(bundle_path))))
  cat("OK project bundle written to ", bundle_path, "\n", sep = "")

  # Versioning: a second save should produce a _v2 directory
  bundle_path2 <- project_save(state, name = "rt_test", base_dir = base)
  stopifnot(bundle_path2 != bundle_path)
  stopifnot(grepl("_v2$", basename(bundle_path2)))
  cat("OK versioned save (", basename(bundle_path2), ")\n", sep = "")

  # Round-trip into a fresh state
  state2 <- new.env()
  project_load(state2, bundle_path)

  stopifnot(state2$dataset_name == "demo_ds")
  stopifnot(identical(state2$recipe[[1]]$id, "drop_zero_var"))
  stopifnot(identical(state2$survey_design$method, "replicate"))
  stopifnot(identical(state2$survey_design$repweights_pattern, "^wr_"))
  stopifnot(state2$last_run_id == 99L)
  stopifnot(state2$last_params$intercept == TRUE)
  stopifnot(state2$last_model$train_config$seed == 42)
  cat("OK recipe / survey design / run params survived round-trip\n")

  # Manifest written on save and present in payload
  json_files <- list.files(bundle_path, pattern = "\\.shinyml\\.json$",
                            full.names = TRUE)
  m <- jsonlite::fromJSON(json_files[1], simplifyVector = FALSE)
  stopifnot(!is.null(m$app), !is.null(m$dataset), !is.null(m$recipe),
            !is.null(m$model), !is.null(m$validation), !is.null(m$metrics))
  cat("OK manifest carries required keys\n")

  cat("project io: PASS\n")
})
