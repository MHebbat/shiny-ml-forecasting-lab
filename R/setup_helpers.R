# =====================================================================
# Setup helpers
#
# Shared logic between install_R.R and setup.R so the two installers
# stay in lockstep. install_r_packages() is idempotent: re-running only
# installs what is missing.
# =====================================================================

# Local %||% so the helper file is self-contained (avoids a load-order
# dependency on utils.R when sourced from setup.R).
if (!exists("%||%", mode = "function"))
  `%||%` <- function(x, y)
    if (is.null(x) || length(x) == 0 ||
        (length(x) == 1 && is.na(x))) y else x

# Required packages — install must succeed.
.shinyml_required <- c(
  # Core Shiny
  "shiny", "bslib", "shinyWidgets", "shinycssloaders", "htmltools",
  # Data IO (incl. Stata/SPSS/SAS via haven, labels via labelled)
  "DT", "readr", "readxl", "arrow", "jsonlite", "haven", "labelled",
  # Wrangling / viz
  "dplyr", "tidyr", "lubridate", "ggplot2", "plotly",
  "forcats", "stringr",
  # Persistence
  "DBI", "RSQLite",
  # Modeling — classical
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

# Optional — best-effort. The app degrades gracefully when they are
# missing. keras3 / keras need a Python TF runtime configured separately.
.shinyml_optional <- c("prophet", "lightgbm", "catboost",
                       "keras3", "keras", "TSLSTMplus",
                       "themis", "future",
                       # Bundesbank-style time-series additions
                       "vars", "urca", "BVAR", "dfms", "midasr",
                       # Predictor / causal analysis adapters
                       "iml", "pdp", "sandwich", "lmtest", "AER",
                       # PDF report rendering (Chrome-print)
                       "pagedown",
                       # Markdown -> PDF fallback
                       "rmarkdown")

install_r_packages <- function(dry_run = FALSE) {
  repos <- getOption("repos")
  if (is.null(repos) || identical(repos[["CRAN"]], "@CRAN@") ||
      !nzchar(repos[["CRAN"]] %||% "")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  binary_os <- (.Platform$OS.type == "windows" ||
                Sys.info()[["sysname"]] == "Darwin")
  pkg_type <- if (binary_os) "binary" else "source"

  installed <- rownames(installed.packages())
  to_install_req <- setdiff(.shinyml_required, installed)
  to_install_opt <- setdiff(.shinyml_optional, installed)

  if (dry_run) {
    message("[setup] DRY RUN - would install required: ",
            if (length(to_install_req)) paste(to_install_req, collapse = ", ")
            else "(none)")
    message("[setup] DRY RUN - would attempt optional: ",
            if (length(to_install_opt)) paste(to_install_opt, collapse = ", ")
            else "(none)")
    return(list(required_missing = to_install_req,
                optional_missing = to_install_opt,
                installed_required = setdiff(.shinyml_required, to_install_req),
                installed_optional = setdiff(.shinyml_optional, to_install_opt),
                dry_run = TRUE))
  }

  if (length(to_install_req) == 0) {
    message("[setup] All required packages already installed.")
  } else {
    message("[setup] Installing ", length(to_install_req),
            " required package(s): ", paste(to_install_req, collapse = ", "))
    tryCatch(install.packages(to_install_req, dependencies = TRUE,
                              type = pkg_type),
             error = function(e)
               message("[setup] Required install error: ", conditionMessage(e)))
  }

  if (length(to_install_opt) > 0) {
    message("[setup] Attempting optional package(s): ",
            paste(to_install_opt, collapse = ", "))
    for (pkg in to_install_opt) {
      tryCatch({
        install.packages(pkg, dependencies = TRUE, type = pkg_type)
      }, error = function(e) {
        message("[setup] Optional '", pkg, "' install error: ",
                conditionMessage(e))
      }, warning = function(w) {
        message("[setup] Optional '", pkg, "' install warning: ",
                conditionMessage(w))
      })
    }
  }

  installed <- rownames(installed.packages())
  list(
    required_missing   = setdiff(.shinyml_required, installed),
    optional_missing   = setdiff(.shinyml_optional, installed),
    installed_required = intersect(.shinyml_required, installed),
    installed_optional = intersect(.shinyml_optional, installed),
    dry_run = FALSE)
}
