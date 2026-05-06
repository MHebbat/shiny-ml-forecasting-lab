# =====================================================================
# Shiny ML & Forecasting Lab
# Pure R Shiny application (with optional Python via reticulate)
#
# Author : Malik Hebbat
# License: MIT
# =====================================================================
# Run locally:
#   1. install.packages(c("shiny","bslib","DT","ggplot2","dplyr","tidyr",
#        "readr","readxl","arrow","jsonlite","DBI","RSQLite","glmnet",
#        "ranger","xgboost","mgcv","forecast","tsibble","fable","feasts",
#        "lubridate","yardstick","recipes","parsnip","tune","rsample",
#        "workflows","reticulate","shinyWidgets","shinycssloaders","plotly"))
#   2. shiny::runApp("/home/user/workspace/shiny_ml_app")
# =====================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(jsonlite)
  library(DBI)
  library(RSQLite)
  library(lubridate)
  library(shinyWidgets)
  library(shinycssloaders)
  library(plotly)
})

# ---- Source modules ---------------------------------------------------
source("R/db.R", local = TRUE)
source("R/utils.R", local = TRUE)
source("R/model_registry.R", local = TRUE)
source("R/python_bridge.R", local = TRUE)
source("R/ai_analysis.R", local = TRUE)
source("R/mod_ingest.R", local = TRUE)
source("R/mod_explore.R", local = TRUE)
source("R/mod_modellab.R", local = TRUE)
source("R/mod_predict.R", local = TRUE)
source("R/mod_dashboard.R", local = TRUE)
source("R/mod_runs.R", local = TRUE)

# ---- Initialize DB on startup ----------------------------------------
db_init()

# ---- UI ---------------------------------------------------------------
ui <- page_navbar(
  id = "main_nav",
  title = tagList(
    icon("flask"),
    span("Shiny ML & Forecasting Lab", style = "font-weight:600;")
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#3fb950",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  fillable = TRUE,
  bg = "#0d1117",

  header = tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$style(HTML("
      .nav-link.active { font-weight:600 !important; }
      .card { border: 1px solid #30363d; }
      .card-header { background: #161b22; font-weight:600; }
      .badge-py { background:#3776ab; color:#fff; }
      .badge-r  { background:#276dc3; color:#fff; }
      pre, code { font-size: 0.85em; }
    "))
  ),

  nav_panel(
    "1 · Data Ingest",
    icon = icon("upload"),
    ingest_ui("ingest")
  ),
  nav_panel(
    "2 · Explore & Prep",
    icon = icon("magnifying-glass-chart"),
    explore_ui("explore")
  ),
  nav_panel(
    "3 · Model Lab",
    icon = icon("microscope"),
    modellab_ui("modellab")
  ),
  nav_panel(
    "4 · Forecast / Predict",
    icon = icon("chart-line"),
    predict_ui("predict")
  ),
  nav_panel(
    "5 · Results Dashboard",
    icon = icon("gauge-high"),
    dashboard_ui("dashboard")
  ),
  nav_panel(
    "6 · Runs & Compare",
    icon = icon("trophy"),
    runs_ui("runs")
  ),
  nav_spacer(),
  nav_item(
    tags$span(
      class = "badge bg-secondary",
      style = "margin-right:8px;",
      textOutput("py_status", inline = TRUE)
    )
  ),
  nav_item(
    tags$a(
      href = "https://shiny.posit.co/", target = "_blank",
      icon("circle-info"), "About"
    )
  )
)

# ---- Server -----------------------------------------------------------
server <- function(input, output, session) {

  # Shared reactive state passed between modules
  state <- reactiveValues(
    raw_data    = NULL,   # uploaded data.frame
    dataset_id  = NULL,   # SQLite id for this dataset
    meta        = NULL,   # list(frequency, target, task_type, time_col, group_cols)
    prepped     = NULL,   # post-recipe data
    recipe      = NULL,   # recipes::recipe object
    last_model  = NULL,   # most recently trained model object
    last_run_id = NULL
  )

  # Show python availability status in navbar
  output$py_status <- renderText({
    if (py_is_available()) "Python: ready" else "Python: optional"
  })

  ingest_server("ingest", state)
  explore_server("explore", state)
  modellab_server("modellab", state)
  predict_server("predict", state)
  dashboard_server("dashboard", state)
  runs_server("runs", state)

  # Auto-jump to Results Dashboard whenever a new run finishes
  observeEvent(state$last_run_id, {
    nav_select("main_nav", "5 · Results Dashboard")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
}

shinyApp(ui, server)
