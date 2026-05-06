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

# ---- Upload size cap --------------------------------------------------
# Default: 5 GB. Override with env var SHINYML_MAX_UPLOAD_GB (numeric).
local({
  gb_env <- Sys.getenv("SHINYML_MAX_UPLOAD_GB", unset = "")
  gb <- if (nzchar(gb_env)) suppressWarnings(as.numeric(gb_env)) else 5
  if (!is.finite(gb) || gb <= 0) gb <- 5
  options(shiny.maxRequestSize = as.numeric(gb) * 1024^3)
  message(sprintf("[shinyml] Upload limit: %.1f GB (set SHINYML_MAX_UPLOAD_GB to change)",
                  gb))
})

# ---- Source modules ---------------------------------------------------
source("R/db.R", local = TRUE)
source("R/utils.R", local = TRUE)
source("R/themes_plot.R", local = TRUE)
source("R/recipe_builder.R", local = TRUE)
source("R/model_registry.R", local = TRUE)
source("R/python_bridge.R", local = TRUE)
source("R/ai_analysis.R", local = TRUE)
source("R/manifest.R", local = TRUE)
source("R/io_project.R", local = TRUE)
source("R/report_render.R", local = TRUE)
source("R/mi_pipeline.R", local = TRUE)
source("R/docs_content.R", local = TRUE)
source("R/mod_docs.R", local = TRUE)
source("R/mod_codebook.R", local = TRUE)
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
  # Default chrome: Bundesbank Light. The user can switch to dark
  # ("Editorial Dark") or "Light Minimal" via the navbar picker.
  theme = bs_theme(
    version = 5,
    primary = "#003D7C",
    base_font = font_google("Inter"),
    heading_font = font_google("Source Serif 4"),
    bg = "#FAFAF7",
    fg = "#1A1A1A"
  ),
  fillable = TRUE,
  bg = "#FAFAF7",

  header = tags$head(
    # Set the initial chrome class on <body>. The picker re-applies it
    # client-side without a reload.
    tags$script(HTML(
"document.addEventListener('DOMContentLoaded', function(){
  document.body.classList.add('chrome-bundesbank');
});
if (window.Shiny) {
  Shiny.addCustomMessageHandler('shinyml_set_chrome', function(m){
    var b = document.body;
    b.classList.remove('chrome-bundesbank','chrome-dark','chrome-light');
    if (m && m.cls) b.classList.add(m.cls);
  });
  Shiny.addCustomMessageHandler('shinyml_copy_clipboard', function(m){
    if (!m || typeof m.text !== 'string') return;
    if (navigator && navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(m.text).catch(function(){});
    } else {
      var ta = document.createElement('textarea');
      ta.value = m.text; document.body.appendChild(ta);
      ta.select(); try { document.execCommand('copy'); } catch(e) {}
      document.body.removeChild(ta);
    }
  });
}"
    )),
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$link(rel = "stylesheet", href = "studio.css"),
    tags$link(rel = "stylesheet", href = "bundesbank.css"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@500;700;900&family=Source+Serif+4:wght@400;600;700&family=Inter:wght@300;400;500;600;700&display=swap"),
    tags$style(HTML("
      .nav-link.active { font-weight:600 !important; }
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
    "10 · Runs & Projects",
    icon = icon("trophy"),
    runs_ui("runs")
  ),
  nav_spacer(),
  nav_item(
    tags$div(class = "chrome-picker",
      tags$span(class = "studio-kicker",
                style = "font-size:0.7em;", "CHROME"),
      selectInput("global_chrome_theme", NULL,
                   choices = c("Bundesbank Light" = "bundesbank",
                                "Editorial Dark"   = "dark",
                                "Light Minimal"    = "light"),
                   selected = "bundesbank",
                   width = "160px")
    )
  ),
  nav_item(
    tags$div(style = "display:flex; align-items:center; gap:6px; margin-right:8px;",
      tags$span(class = "studio-kicker",
                style = "font-size:0.7em;", "PLOT THEME"),
      selectInput("global_plot_theme", NULL,
                   choices = c("Bundesbank" = "bundesbank",
                                "Studio (dark)" = "studio",
                                "Light minimal" = "light"),
                   selected = "bundesbank",
                   width = "150px"),
      tags$div(id = "plot_theme_swatch",
               style = "width:60px; height:18px; border:1px solid #D9D9D9;",
               plotly::plotlyOutput("plot_theme_preview",
                                      width = "60px", height = "18px"))
    )
  ),
  nav_item(
    tags$div(style = "display:flex; align-items:center; gap:6px; margin-right:8px;",
      tags$span(class = "studio-kicker",
                style = "font-size:0.7em;", "PROJECT"),
      uiOutput("project_badge_ui", inline = TRUE),
      uiOutput("project_picker_ui", inline = TRUE),
      actionButton("project_load_btn", "Load",
                    class = "btn-outline-primary btn-sm")
    )
  ),
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
    survey_design   = NULL,    # declared survey design
    plot_theme      = "bundesbank", # active plot theme: bundesbank | studio | light
    chrome_theme    = "bundesbank", # active chrome: bundesbank | dark | light
    current_project = NULL,         # name of currently loaded project bundle
    last_project_save = NULL        # path to last saved project (refresh hook)
  )

  # Show python availability status in navbar
  output$py_status <- renderText({
    if (py_is_available()) "Python: ready" else "Python: optional"
  })

  # ---- Plot theme: keep state$plot_theme in sync + render preview ----
  observe({
    state$plot_theme <- input$global_plot_theme %||% "bundesbank"
  })
  output$plot_theme_preview <- plotly::renderPlotly({
    plot_theme_preview(input$global_plot_theme %||% "bundesbank")
  })

  # ---- Chrome theme: toggle a class on <body> via JS, persist in state
  observe({
    th <- input$global_chrome_theme %||% "bundesbank"
    state$chrome_theme <- th
    session$sendCustomMessage("shinyml_set_chrome",
      list(cls = paste0("chrome-", th)))
  })

  # ---- Project badge -------------------------------------------------
  output$project_badge_ui <- renderUI({
    nm <- state$current_project
    if (is.null(nm) || !nzchar(nm))
      tags$span(class = "badge bg-secondary",
                style = "font-size:0.78em;", "(unsaved)")
    else
      tags$span(class = "badge bg-primary",
                style = "font-size:0.78em;",
                title = sprintf("Loaded project: %s", nm),
                substr(nm, 1, 28))
  })

  # ---- Project picker UI ---------------------------------------------
  output$project_picker_ui <- renderUI({
    df <- tryCatch(project_list(), error = function(e) data.frame())
    if (is.null(df) || nrow(df) == 0) {
      return(tags$span(class = "text-muted",
                        style = "font-size:0.85em;",
                        "(none saved)"))
    }
    selectInput("project_pick", NULL,
                 choices = setNames(df$path,
                                     sprintf("%s · %s",
                                              df$name, df$saved_at)),
                 width = "180px")
  })

  observeEvent(input$project_load_btn, {
    p <- input$project_pick
    if (is.null(p) || !nzchar(p)) {
      flash("No saved project to load.", "warning"); return()
    }
    res <- tryCatch(project_load(state, p),
                    error = function(e) {
                      flash(paste("Load failed:",
                                    conditionMessage(e)), "error"); NULL })
    req(res)
    state$current_project <- res$name %||% basename(p)
    flash(sprintf("Loaded project '%s'.", state$current_project),
          "message")
  })

  ingest_server("ingest", state)
  explore_server("explore", state, parent_session = session)
  dataprep_server("dataprep", state, parent_session = session)
  privacy_server("privacy", state)
  survey_server("survey", state, parent_session = session)
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
