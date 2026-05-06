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

  # ---- Required packages ---------------------------------------------
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

  installed <- rownames(installed.packages())
  to_install <- setdiff(needed, installed)

  if (length(to_install) == 0) {
    message("[install_R] All required packages already installed.")
  } else {
    message("[install_R] Installing ", length(to_install),
            " package(s): ", paste(to_install, collapse = ", "))
    # Use binary builds where possible (Win/macOS) to avoid C++ toolchain
    # requirements on first install.
    install.packages(to_install,
                     dependencies = TRUE,
                     type = if (.Platform$OS.type == "windows" ||
                                Sys.info()[["sysname"]] == "Darwin")
                              "binary" else "source")
  }

  # ---- Verify --------------------------------------------------------
  still_missing <- setdiff(needed, rownames(installed.packages()))
  if (length(still_missing) > 0) {
    warning("[install_R] Still missing: ",
            paste(still_missing, collapse = ", "),
            "\n  Try installing manually: install.packages(c(",
            paste(sprintf("'%s'", still_missing), collapse = ", "),
            "))")
    quit(status = 1)
  }
  message("[install_R] Done. ", length(needed), " package(s) ready.")
})
