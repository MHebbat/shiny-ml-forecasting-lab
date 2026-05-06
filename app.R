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
source("R/recipe_builder.R", local = TRUE)
source("R/model_registry.R", local = TRUE)
source("R/python_bridge.R", local = TRUE)
source("R/ai_analysis.R", local = TRUE)
source("R/mod_ingest.R", local = TRUE)
source("R/mod_explore.R", local = TRUE)
source("R/mod_dataprep.R", local = TRUE)
source("R/mod_privacy.R", local = TRUE)
source("R/mod_survey.R", local = TRUE)
source("R/mod_modellab.R", local = TRUE)
source("R/mod_predict.R", local = TRUE)
source("R/mod_dashboard.R", local = TRUE)
source("R/mod_studio.R", local = TRUE)
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
    tags$link(rel = "stylesheet", href = "studio.css"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@500;700;900&family=Inter:wght@300;400;500;600;700&display=swap"),
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
    "2 · Explore",
    icon = icon("magnifying-glass-chart"),
    explore_ui("explore")
  ),
  nav_panel(
    "3 · Data Prep",
    icon = icon("wand-magic-sparkles"),
    dataprep_ui("dataprep")
  ),
  nav_panel(
    "4 · Privacy Audit",
    icon = icon("shield-halved"),
    privacy_ui("privacy")
  ),
  nav_panel(
    "5 · Survey & Panel",
    icon = icon("clipboard-list"),
    survey_ui("survey")
  ),
  nav_panel(
    "6 · Model Lab",
    icon = icon("microscope"),
    modellab_ui("modellab")
  ),
  nav_panel(
    "7 · Forecast / Predict",
    icon = icon("chart-line"),
    predict_ui("predict")
  ),
  nav_panel(
    "8 · Results Dashboard",
    icon = icon("gauge-high"),
    dashboard_ui("dashboard")
  ),
  nav_panel(
    "9 · Editorial Studio",
    icon = icon("newspaper"),
    studio_ui("studio")
  ),
  nav_panel(
    "10 · Runs & Compare",
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
    # ---- Multi-dataset workspace ----
    datasets        = list(),  # named list keyed by ds_id ("ds_1", "ds_2", ...)
    active_id       = NULL,    # currently active key in $datasets
    # ---- Active-dataset shortcuts (mirrored by ingest_server) ----
    raw_data        = NULL,
    dataset_id      = NULL,    # SQLite id for the saved dataset
    dataset_name    = NULL,
    meta            = NULL,    # list(frequency, target, task_type, time_col, group_cols)
    labels          = list(),  # haven var/value labels
    survey_hints    = list(),  # auto-detected design columns
    # ---- Pipeline / model artifacts ----
    prepped         = NULL,    # post-recipe data
    recipe          = NULL,    # recipes::recipe object
    last_model      = NULL,
    last_run_id     = NULL,
    last_params     = NULL,
    n_train         = NULL,
    privacy_allow_ai = TRUE,   # AI egress gate
    prep_log        = list(),  # applied prep steps
    survey_design   = NULL     # declared survey design
  )

  # Show python availability status in navbar
  output$py_status <- renderText({
    if (py_is_available()) "Python: ready" else "Python: optional"
  })

  ingest_server("ingest", state)
  explore_server("explore", state, parent_session = session)
  dataprep_server("dataprep", state, parent_session = session)
  privacy_server("privacy", state)
  survey_server("survey", state)
  modellab_server("modellab", state)
  predict_server("predict", state)
  dashboard_server("dashboard", state)
  studio_server("studio", state)
  runs_server("runs", state)

  # Auto-jump to Results Dashboard whenever a new run finishes
  observeEvent(state$last_run_id, {
    nav_select("main_nav", "8 · Results Dashboard")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
}

shinyApp(ui, server)
