# =====================================================================
# install_R.R - cross-platform R package installer for the Shiny ML Lab
# Idempotent: re-running this script only installs what's missing.
# Works on macOS, Windows, and Linux without modification.
# =====================================================================

local({
  # ---- Pick a fast, stable CRAN mirror -------------------------------
  repos <- getOption("repos")
  if (is.null(repos) || identical(repos[["CRAN"]], "@CRAN@") ||
      !nzchar(repos[["CRAN"]] %||% "")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  # `%||%` is normally defined in utils.R; redefine locally so the
  # installer is self-contained even before the app is sourced.
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

  # ---- Friendly OS report --------------------------------------------
  os <- if (.Platform$OS.type == "windows") "Windows"
        else if (Sys.info()[["sysname"]] == "Darwin") "macOS"
        else "Linux"
  message(sprintf("[install_R] %s | R %s.%s",
                  os, R.version$major, R.version$minor))

  # ---- Required packages (install must succeed) ----------------------
  needed <- c(
    # Core Shiny
    "shiny", "bslib", "shinyWidgets", "shinycssloaders", "htmltools",
    # Data IO (incl. Stata/SPSS/SAS via haven, labels via labelled)
    "DT", "readr", "readxl", "arrow", "jsonlite", "haven", "labelled",
    # Wrangling / viz
    "dplyr", "tidyr", "lubridate", "ggplot2", "plotly",
    "forcats", "stringr",
    # Persistence
    "DBI", "RSQLite",
    # Modeling - classical
    "glmnet", "MASS", "mgcv", "betareg",
    # Tree-based
    "ranger", "xgboost",
    # Time series
    "forecast",
    # Tidymodels (used in some workflows)
    "recipes", "yardstick",
    # Survey / panel analytics
    "survey", "srvyr",
    # Privacy / hashing
    "digest",
    # Python bridge
    "reticulate",
    # HTTP client for AI providers
    "httr2"
  )

  # ---- Optional packages (install attempt is best-effort) ------------
  # These are heavy or have native dependencies (Stan/rstan, system libs)
  # and may legitimately fail on some systems. The app degrades gracefully
  # when they are missing — the model registry shows an inline warning
  # and skips the affected models.
  optional <- c("prophet", "lightgbm", "catboost")

  binary_os <- (.Platform$OS.type == "windows" ||
                Sys.info()[["sysname"]] == "Darwin")
  pkg_type <- if (binary_os) "binary" else "source"

  installed <- rownames(installed.packages())

  # ---- Install required ---------------------------------------------
  to_install <- setdiff(needed, installed)
  if (length(to_install) == 0) {
    message("[install_R] All required packages already installed.")
  } else {
    message("[install_R] Installing ", length(to_install),
            " required package(s): ", paste(to_install, collapse = ", "))
    install.packages(to_install, dependencies = TRUE, type = pkg_type)
  }

  # ---- Install optional (best-effort, never aborts) ------------------
  installed <- rownames(installed.packages())
  optional_to_install <- setdiff(optional, installed)
  if (length(optional_to_install) > 0) {
    message("[install_R] Attempting optional package(s): ",
            paste(optional_to_install, collapse = ", "))
    for (pkg in optional_to_install) {
      ok <- tryCatch({
        install.packages(pkg, dependencies = TRUE, type = pkg_type)
        TRUE
      }, error = function(e) {
        message("[install_R] Optional '", pkg, "' install error: ",
                conditionMessage(e))
        FALSE
      }, warning = function(w) {
        message("[install_R] Optional '", pkg, "' install warning: ",
                conditionMessage(w))
        TRUE
      })
      if (!ok) next
    }
  }

  # ---- Verify --------------------------------------------------------
  installed <- rownames(installed.packages())
  still_missing_required <- setdiff(needed, installed)
  still_missing_optional <- setdiff(optional, installed)

  if (length(still_missing_optional) > 0) {
    message("[install_R] NOTE: optional package(s) not installed: ",
            paste(still_missing_optional, collapse = ", "),
            ". The app will still run; affected models will show a ",
            "missing-dependency notice in the Model Lab.")
  }

  if (length(still_missing_required) > 0) {
    warning("[install_R] Still missing required: ",
            paste(still_missing_required, collapse = ", "),
            "\n  Try installing manually: install.packages(c(",
            paste(sprintf("'%s'", still_missing_required), collapse = ", "),
            "))")
    quit(status = 1)
  }
  message("[install_R] Done. ", length(needed),
          " required package(s) ready (", length(optional) -
            length(still_missing_optional), "/", length(optional),
          " optional).")
})
