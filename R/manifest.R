# =====================================================================
# Reproducibility manifest
#
# A manifest is a self-describing JSON-friendly list that captures the
# minimum information required to reproduce a Lab run: app version,
# dataset, recipe, survey design (if any), model spec, validation
# strategy, metrics, and software / git provenance.
#
# Author: Malik Hebbat
# =====================================================================

# Compute a SHA-256 hash of a data.frame's serialised representation.
# Used as a deterministic dataset fingerprint without having to ship the
# rows themselves.
.df_sha256 <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NA_character_)
  if (!requireNamespace("digest", quietly = TRUE)) return(NA_character_)
  tryCatch(digest::digest(df, algo = "sha256", serialize = TRUE),
           error = function(e) NA_character_)
}

# Try a few strategies to find the current git commit. Failures yield NA.
.git_commit <- function() {
  tryCatch({
    out <- suppressWarnings(system2("git", c("rev-parse", "HEAD"),
                                      stdout = TRUE, stderr = FALSE))
    if (length(out) > 0 && nzchar(out[1])) return(out[1])
    NA_character_
  }, error = function(e) NA_character_)
}

# Distil sessionInfo() into something JSON-friendly: R version, OS,
# loaded packages with versions.
.session_distill <- function() {
  si <- tryCatch(utils::sessionInfo(), error = function(e) NULL)
  if (is.null(si)) return(list(R = R.version.string, packages = list()))
  pkgs <- c(names(si$otherPkgs %||% list()),
            names(si$loadedOnly %||% list()))
  pkg_versions <- vapply(pkgs, function(p) {
    tryCatch(as.character(utils::packageVersion(p)),
             error = function(e) NA_character_)
  }, character(1))
  list(
    R          = R.version.string,
    platform   = si$platform %||% R.version$platform,
    os         = si$running %||% paste(Sys.info()[c("sysname","release")],
                                         collapse = " "),
    packages   = as.list(setNames(pkg_versions, pkgs))
  )
}

.python_distill <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) return(NULL)
  if (!isTRUE(reticulate::py_available(initialize = FALSE))) return(NULL)
  tryCatch({
    cfg <- reticulate::py_config()
    list(python = cfg$python %||% NA_character_,
         version = as.character(cfg$version %||% NA_character_))
  }, error = function(e) NULL)
}

# Build a manifest list from a Shiny state object.
make_manifest <- function(state) {
  ds_name <- state$dataset_name %||% NA_character_
  raw     <- state$raw_data
  rec     <- state$recipe
  if (is.null(rec) && is.list(state$prep_log)) rec <- state$prep_log
  recipe_log <- if (length(state$prep_log %||% list()) > 0)
    unlist(state$prep_log) else character(0)

  metrics <- if (!is.null(state$last_model$metrics)) state$last_model$metrics else list()
  bootstrap_ci <- list()
  if (!is.null(state$last_model$actual) && !is.null(state$last_model$predicted) &&
      requireNamespace("stats", quietly = TRUE)) {
    boot <- tryCatch(bootstrap_metric(state$last_model$actual,
                                       state$last_model$predicted,
                                       R = 200L),
                     error = function(e) NULL)
    if (!is.null(boot))
      bootstrap_ci <- list(metric = "RMSE",
                           estimate = boot$estimate,
                           lo = boot$lo, hi = boot$hi,
                           R = boot$R, n = boot$n)
  }

  model_spec <- list(
    id           = state$last_model$model_id %||% NA_character_,
    label        = NA_character_,
    params       = state$last_params %||% list(),
    available_backend = NA_character_
  )
  if (!is.null(state$last_model$model_id)) {
    m <- tryCatch(MODELS[[state$last_model$model_id]], error = function(e) NULL)
    if (!is.null(m)) {
      model_spec$label <- m$label %||% state$last_model$model_id
      av <- tryCatch(model_availability(state$last_model$model_id),
                     error = function(e) NULL)
      if (!is.null(av))
        model_spec$available_backend <- if (isTRUE(av$ok)) "ready" else
          (av$msg %||% "unavailable")
    }
  }

  validation <- list(
    strategy = state$last_model$train_config$validation %||% "holdout",
    params   = state$last_model$train_config %||% list(),
    seed     = state$last_model$train_config$seed %||% NA_integer_
  )

  list(
    app = list(
      name         = "Shiny ML & Forecasting Lab",
      version      = .git_commit(),
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S",
                             tz = "UTC", usetz = TRUE)
    ),
    dataset = list(
      name   = ds_name,
      n_rows = if (is.null(raw)) NA_integer_ else nrow(raw),
      n_cols = if (is.null(raw)) NA_integer_ else ncol(raw),
      sha256 = .df_sha256(raw),
      source = {
        aid <- state$active_id
        if (!is.null(aid) && !is.null(state$datasets[[aid]]))
          state$datasets[[aid]]$format %||% NA_character_
        else NA_character_
      }
    ),
    recipe = list(
      steps = state$recipe %||% list(),
      log   = as.list(recipe_log)
    ),
    survey_design = state$survey_design,
    model         = model_spec,
    validation    = validation,
    metrics = list(
      train = list(),
      test  = metrics,
      bootstrap_ci_95 = bootstrap_ci
    ),
    software = .session_distill(),
    python   = .python_distill(),
    git_commit = .git_commit()
  )
}

# Convenience: pretty JSON.
manifest_to_json <- function(m, pretty = TRUE) {
  jsonlite::toJSON(m, auto_unbox = TRUE, na = "null", null = "null",
                   pretty = pretty)
}
