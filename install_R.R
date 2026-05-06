# Run once to install the R packages used by the app.
# Heavy/optional ones are commented out — uncomment what you need.

needed <- c(
  # Core Shiny
  "shiny", "bslib", "shinyWidgets", "shinycssloaders",
  # Data IO
  "DT", "readr", "readxl", "arrow", "jsonlite",
  # Wrangling / viz
  "dplyr", "tidyr", "lubridate", "ggplot2", "plotly",
  # Persistence
  "DBI", "RSQLite",
  # Modeling — classical
  "glmnet", "MASS", "mgcv", "betareg",
  # Tree-based
  "ranger", "xgboost",
  # Time series
  "forecast", "tsibble", "fable", "feasts",
  # Tidymodels (optional but nice for tuning workflows)
  "recipes", "parsnip", "tune", "rsample", "yardstick", "workflows",
  # Python bridge
  "reticulate",
  # HTTP client for AI providers
  "httr2"
)

# Optional / heavy — uncomment to install
# optional <- c("lightgbm", "catboost", "torch", "keras", "tensorflow",
#               "lme4", "survival", "flexsurv", "gamlss", "prophet",
#               "h2o", "e1071", "rpart", "class", "nnet", "pscl",
#               "text2vec")

to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

cat("Done. Missing now:", length(setdiff(needed, rownames(installed.packages()))), "\n")
