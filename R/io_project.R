# =====================================================================
# Project save / load
#
# A "project bundle" lives at ~/.shinyml/projects/<name>/ and contains
#   <name>.shinyml.json   — the manifest (human-readable summary)
#   <name>.shinyml.rds    — full state snapshot (datasets, recipe,
#                            survey design, last model, last params)
#
# The bundle is self-contained: project_load() rehydrates the same
# Shiny state without needing the original raw file.
#
# Author: Malik Hebbat
# =====================================================================

PROJECT_DIR <- function() {
  d <- file.path(Sys.getenv("HOME", unset = "."), ".shinyml", "projects")
  if (!dir.exists(d))
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

.project_safe_name <- function(x) {
  s <- as.character(x %||% "project")
  s <- gsub("[^A-Za-z0-9_.-]+", "_", s)
  s <- sub("^_+", "", s); s <- sub("_+$", "", s)
  if (!nzchar(s)) s <- "project"
  s
}

# List all available projects (most recent first)
project_list <- function() {
  d <- PROJECT_DIR()
  dirs <- list.dirs(d, full.names = TRUE, recursive = FALSE)
  if (length(dirs) == 0) return(data.frame(name = character(0),
                                            path = character(0),
                                            saved_at = character(0),
                                            stringsAsFactors = FALSE))
  out <- lapply(dirs, function(p) {
    json <- list.files(p, pattern = "\\.shinyml\\.json$", full.names = TRUE)
    if (length(json) == 0) return(NULL)
    info <- file.info(json[1])
    data.frame(name = basename(p), path = p,
                saved_at = format(info$mtime, "%Y-%m-%d %H:%M:%S"),
                stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), out))
  if (is.null(out) || nrow(out) == 0) return(data.frame(name = character(0),
                                                         path = character(0),
                                                         saved_at = character(0),
                                                         stringsAsFactors = FALSE))
  out[order(out$saved_at, decreasing = TRUE), ]
}

# Save the current state as a project bundle. Returns the bundle path.
project_save <- function(state, name = NULL, base_dir = NULL) {
  base <- base_dir %||% PROJECT_DIR()
  nm <- .project_safe_name(name %||% state$dataset_name %||% "project")
  # Versioned: append _v2, _v3, ... if the directory exists.
  full <- file.path(base, nm)
  if (dir.exists(full)) {
    v <- 2L
    while (dir.exists(file.path(base, sprintf("%s_v%d", nm, v)))) v <- v + 1L
    nm <- sprintf("%s_v%d", nm, v)
    full <- file.path(base, nm)
  }
  dir.create(full, recursive = TRUE, showWarnings = FALSE)

  manifest <- tryCatch(make_manifest(state), error = function(e) list())
  json_path <- file.path(full, paste0(nm, ".shinyml.json"))
  rds_path  <- file.path(full, paste0(nm, ".shinyml.rds"))

  payload <- list(
    schema_version = 1L,
    saved_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S",
                              tz = "UTC", usetz = TRUE),
    name           = nm,
    datasets       = .reactive_to_list(state$datasets),
    active_id      = state$active_id,
    raw_data       = state$raw_data,
    dataset_name   = state$dataset_name,
    dataset_id     = state$dataset_id,
    meta           = .reactive_to_list(state$meta),
    labels         = .reactive_to_list(state$labels),
    survey_hints   = .reactive_to_list(state$survey_hints),
    recipe         = .reactive_to_list(state$recipe),
    prep_log       = .reactive_to_list(state$prep_log),
    survey_design  = .reactive_to_list(state$survey_design),
    last_model     = state$last_model,
    last_params    = .reactive_to_list(state$last_params),
    last_run_id    = state$last_run_id,
    plot_theme     = state$plot_theme,
    chrome_theme   = state$chrome_theme,
    manifest       = manifest
  )

  tryCatch({
    jsonlite::write_json(manifest, json_path, auto_unbox = TRUE,
                          pretty = TRUE, na = "null", null = "null")
  }, error = function(e) {
    writeLines(jsonlite::toJSON(list(error = conditionMessage(e)),
                                  auto_unbox = TRUE), json_path)
  })
  saveRDS(payload, rds_path)
  invisible(full)
}

# Load a project bundle by directory path. Mutates the reactiveValues
# `state` so that downstream modules see the rehydrated dataset.
project_load <- function(state, path) {
  if (!dir.exists(path))
    stop(sprintf("Project not found: %s", path))
  rds <- list.files(path, pattern = "\\.shinyml\\.rds$", full.names = TRUE)
  if (length(rds) == 0)
    stop(sprintf("No .shinyml.rds bundle in %s", path))
  payload <- readRDS(rds[1])

  state$datasets       <- payload$datasets       %||% list()
  state$active_id      <- payload$active_id
  state$raw_data       <- payload$raw_data
  state$dataset_name   <- payload$dataset_name
  state$dataset_id     <- payload$dataset_id
  state$meta           <- payload$meta
  state$labels         <- payload$labels         %||% list()
  state$survey_hints   <- payload$survey_hints   %||% list()
  state$recipe         <- payload$recipe
  state$prep_log       <- payload$prep_log       %||% list()
  state$survey_design  <- payload$survey_design
  state$last_model     <- payload$last_model
  state$last_params    <- payload$last_params
  state$last_run_id    <- payload$last_run_id
  state$plot_theme     <- payload$plot_theme     %||% "studio"
  state$chrome_theme   <- payload$chrome_theme   %||% "bundesbank"
  invisible(payload)
}

# Helper: deep-copy reactive values (or list-likes) into plain lists for
# saveRDS. reactiveValues do serialize, but we want a stable structure.
.reactive_to_list <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x) && !is.data.frame(x))
    return(lapply(x, function(el) if (is.list(el) && !is.data.frame(el))
                                      .reactive_to_list(el) else el))
  x
}
