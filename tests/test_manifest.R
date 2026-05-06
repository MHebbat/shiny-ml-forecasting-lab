# =====================================================================
# Manifest smoke test
#
# Verifies make_manifest() returns the required top-level keys for a
# minimal state object.
# =====================================================================

local({
  here <- if (file.exists("R/utils.R")) "." else
          if (file.exists("../R/utils.R")) ".." else
          stop("test must be run from repo root or tests/")
  source(file.path(here, "R", "utils.R"), local = TRUE)
  source(file.path(here, "R", "manifest.R"), local = TRUE)

  # Minimal "state" — a plain list with the fields make_manifest reads.
  state <- list(
    dataset_name = "demo",
    raw_data = data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE),
    datasets = list(),
    active_id = NULL,
    recipe = list(list(id = "drop_zero_var", label = "drop zero var",
                        targets = "all_predictors", params = list())),
    prep_log = list("drop_zero_var -> 0 cols"),
    survey_design = NULL,
    last_model = NULL,
    last_params = list(),
    meta = list(target = "a", task_type = "regression", frequency = "monthly")
  )

  m <- make_manifest(state)
  required <- c("app", "dataset", "recipe", "survey_design",
                 "model", "validation", "metrics", "software",
                 "git_commit")
  for (k in required) {
    if (is.null(m[[k]]) && k != "survey_design")
      stop(sprintf("missing manifest key: %s", k))
  }
  cat("OK manifest top-level keys present\n")

  # Dataset block carries n_rows/n_cols
  stopifnot(m$dataset$n_rows == 5L, m$dataset$n_cols == 2L)
  stopifnot(!is.null(m$app$generated_at))
  cat("OK dataset block populated\n")

  # JSON serialisability
  txt <- jsonlite::toJSON(m, auto_unbox = TRUE, na = "null", null = "null")
  stopifnot(nchar(txt) > 50)
  cat("OK manifest JSON-serialisable\n")

  cat("manifest: PASS\n")
})
