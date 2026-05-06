# Verify each new TS model appears in the registry with the required
# fields (id, label, fn, available, params) — the fitter and predictor
# may not run end-to-end without optional packages, but the *registry
# entry* must always exist so the UI can show a missing-dependency
# notice rather than a broken card.
suppressMessages(source("R/utils.R"))
suppressMessages(suppressWarnings(try(source("R/model_registry.R"), silent = TRUE)))

stopifnot(exists("MODELS"))

new_ids <- c("naive", "snaive", "rwf_drift", "meanf", "theta",
             "stl_arima", "var", "vecm", "bvar", "dfm", "midas")

for (id in new_ids) {
  m <- MODELS[[id]]
  if (is.null(m)) stop("Missing registry entry for '", id, "'")
  for (k in c("id", "label", "fn", "available", "params", "task_types")) {
    if (is.null(m[[k]]))
      stop(sprintf("Model '%s' missing field '%s'", id, k))
  }
  if (!is.function(m$fn))
    stop(sprintf("Model '%s' fn is not a function", id))
  if (!is.function(m$available))
    stop(sprintf("Model '%s' available is not a function", id))
  if (!"time_series" %in% m$task_types)
    stop(sprintf("Model '%s' missing 'time_series' task type", id))
  if (!is.list(m$params) || length(m$params) < 1L)
    stop(sprintf("Model '%s' has no params spec", id))
  # Each param must have name, type, default
  for (p in m$params) {
    for (k2 in c("name", "type"))
      if (is.null(p[[k2]]))
        stop(sprintf("Model '%s' has malformed param: missing %s", id, k2))
  }
}

# The available() probe must not throw even when optional pkgs are missing.
for (id in new_ids) {
  ok <- tryCatch(MODELS[[id]]$available(), error = function(e) NULL)
  if (is.null(ok) || is.null(ok$ok))
    stop("available() malformed for '", id, "'")
}

# Install command lookup must return a non-empty string for every new model
for (id in new_ids) {
  cmd <- model_install_command(id)
  if (is.null(cmd) || !nzchar(cmd))
    stop("No install command for '", id, "'")
}

cat(sprintf("test_ts_models_registry.R: OK (%d new models)\n", length(new_ids)))
