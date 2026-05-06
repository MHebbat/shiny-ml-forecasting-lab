# =====================================================================
# Module 5: Runs & Compare
#   Leaderboard of all training runs, persisted in SQLite.
# =====================================================================

runs_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      div(style = "display:flex; justify-content:space-between; align-items:center;",
          "Run leaderboard",
          actionButton(ns("refresh"), "Refresh", icon = icon("rotate"),
                       class = "btn-sm btn-outline-light"))
    ),
    DT::DTOutput(ns("table")),
    hr(),
    h6("Run details"),
    verbatimTextOutput(ns("detail")),
    plotlyOutput(ns("detail_plot"), height = "400px")
  )
}

runs_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    runs_data <- reactiveVal(db_get_runs())

    observeEvent(input$refresh, runs_data(db_get_runs()))
    # Refresh whenever a new run is saved
    observeEvent(state$last_run_id, runs_data(db_get_runs()),
                 ignoreNULL = TRUE)

    output$table <- DT::renderDT({
      d <- runs_data()
      if (nrow(d) == 0) return(DT::datatable(data.frame(message = "No runs yet")))
      # parse metrics
      mtr <- lapply(d$metrics_json, function(x) tryCatch(jsonlite::fromJSON(x),
                                                          error = function(e) list()))
      d$metric_summary <- vapply(mtr, fmt_metrics, character(1))
      d$created_at <- substr(d$created_at, 1, 19)
      d$duration_sec <- round(d$duration_sec, 2)
      DT::datatable(
        d[, c("id","created_at","dataset","task_type","model_id",
              "metric_summary","horizon","duration_sec","status")],
        selection = "single",
        options = list(pageLength = 15, dom = "tip", scrollX = TRUE,
                       order = list(list(0, "desc"))),
        rownames = FALSE, class = "compact stripe"
      )
    })

    selected_run <- reactive({
      sel <- input$table_rows_selected
      d <- runs_data()
      if (is.null(sel) || nrow(d) == 0) return(NULL)
      d[sel, ]
    })

    output$detail <- renderText({
      r <- selected_run(); if (is.null(r)) return("Select a row to view details.")
      paste(
        sprintf("Run #%s · %s · %s", r$id, r$model_id, r$task_type),
        sprintf("Dataset: %s", r$dataset),
        sprintf("Created: %s", r$created_at),
        sprintf("Duration: %.2fs · Horizon: %s", r$duration_sec, r$horizon %||% "—"),
        "",
        "Hyperparameters:",
        jsonlite::toJSON(jsonlite::fromJSON(r$params_json), pretty = TRUE,
                         auto_unbox = TRUE),
        "",
        "Metrics:",
        jsonlite::toJSON(jsonlite::fromJSON(r$metrics_json), pretty = TRUE,
                         auto_unbox = TRUE),
        sep = "\n"
      )
    })

    output$detail_plot <- renderPlotly({
      r <- selected_run(); req(r)
      preds <- db_get_predictions(r$id)
      if (nrow(preds) == 0) return(plotly_empty())
      plot_ly(preds, x = ~idx) |>
        add_lines(y = ~actual,    name = "Actual",
                  line = list(color = "#c9d1d9")) |>
        add_lines(y = ~predicted, name = "Predicted",
                  line = list(color = "#3fb950")) |>
        layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
               font = list(color = "#c9d1d9"),
               xaxis = list(title = "Index"), yaxis = list(title = ""))
    })
  })
}
