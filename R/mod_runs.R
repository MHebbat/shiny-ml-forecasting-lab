# =====================================================================
# Module 10: Runs & Projects
#
# Two sub-tabs:
#   * Runs     — leaderboard of all training runs (SQLite).
#   * Projects — saved project bundles in ~/.shinyml/projects/. Each
#                row exposes Open, Reproduce, Diff, Delete actions.
# =====================================================================

runs_ui <- function(id) {
  ns <- NS(id)
  navset_card_underline(
    title = div(style = "display:flex; justify-content:space-between; align-items:center;",
                "Runs & Projects",
                actionButton(ns("refresh"), "Refresh", icon = icon("rotate"),
                             class = "btn-sm btn-outline-light")),
    nav_panel("Projects",
      icon = icon("folder-open"),
      tags$p(class = "text-muted",
             "Project bundles saved to ~/.shinyml/projects/. ",
             "Open re-loads the workspace; Reproduce replays the recipe and ",
             "compares the primary metric within a configurable tolerance; ",
             "Diff shows a side-by-side comparison of two bundles."),
      tags$div(style = "display:flex; gap:8px; flex-wrap:wrap; align-items:flex-end;",
        numericInput(ns("repro_tolerance"), "Reproduce tolerance (epsilon)",
                      value = 1e-6, min = 0, max = 1, step = 1e-6,
                      width = "220px"),
        actionButton(ns("diff_btn"), "Diff selected pair",
                      icon = icon("code-compare"),
                      class = "btn-sm btn-outline-info")),
      tags$small(class = "text-muted", style = "display:block; margin:6px 0;",
        "Select exactly 2 rows then click Diff."),
      DT::DTOutput(ns("projects_table")),
      hr(),
      uiOutput(ns("project_action_panel")),
      hr(),
      h6("Diff result"),
      DT::DTOutput(ns("diff_table"))
    ),
    nav_panel("Leaderboard",
      icon = icon("trophy"),
      DT::DTOutput(ns("table")),
      hr(),
      h6("Run details"),
      verbatimTextOutput(ns("detail")),
      plotlyOutput(ns("detail_plot"), height = "400px")
    ),
    nav_panel("Stats Explorer",
      icon = icon("magnifying-glass"),
      tags$p(class = "text-muted",
             "Every metric, hyperparameter, and diagnostic across every run — fully searchable. Use the column filters to drill down (e.g. find every xgboost run with eta=0.05)."),
      DT::DTOutput(ns("stats_table"))
    )
  )
}

runs_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    runs_data     <- reactiveVal(db_get_runs())
    stats_data    <- reactiveVal(db_flatten_stats())
    projects_data <- reactiveVal(project_list_rich())
    diff_result   <- reactiveVal(NULL)

    refresh_all <- function() {
      runs_data(db_get_runs())
      stats_data(db_flatten_stats())
      projects_data(project_list_rich())
    }

    observeEvent(input$refresh, refresh_all())
    observeEvent(state$last_run_id, refresh_all(), ignoreNULL = TRUE)
    observeEvent(state$last_project_save, refresh_all(),
                 ignoreNULL = TRUE, ignoreInit = TRUE)

    # ---- Projects table ----------------------------------------------
    output$projects_table <- DT::renderDT({
      d <- projects_data()
      if (is.null(d) || nrow(d) == 0)
        return(DT::datatable(
          data.frame(message = "No projects saved yet. Use Save Project from Survey or Model Lab.")))
      view <- d[, c("name","saved_at","dataset","sha256_short",
                     "model_id","primary","manifest_hash"), drop = FALSE]
      DT::datatable(view,
        selection = "multiple",
        filter = "top",
        options = list(pageLength = 15, dom = "tip", scrollX = TRUE,
                       order = list(list(1, "desc"))),
        rownames = FALSE, class = "compact stripe")
    })

    selected_projects <- reactive({
      sel <- input$projects_table_rows_selected
      d <- projects_data()
      if (is.null(sel) || length(sel) == 0 || nrow(d) == 0) return(NULL)
      d[sel, , drop = FALSE]
    })

    output$project_action_panel <- renderUI({
      sel <- selected_projects()
      if (is.null(sel) || nrow(sel) == 0) {
        return(tags$div(class = "text-muted",
                        "Select a project to enable actions."))
      }
      if (nrow(sel) == 1) {
        tags$div(style = "display:flex; gap:6px; flex-wrap:wrap;",
          actionButton(ns("open_btn"), "Open",
                        icon = icon("folder-open"),
                        class = "btn-primary"),
          actionButton(ns("reproduce_btn"), "Reproduce",
                        icon = icon("rotate"),
                        class = "btn-outline-info"),
          actionButton(ns("delete_btn"), "Delete",
                        icon = icon("trash"),
                        class = "btn-outline-danger"))
      } else {
        tags$div(class = "text-muted",
                 sprintf("%d projects selected — click 'Diff selected pair' for a comparison.",
                         nrow(sel)))
      }
    })

    observeEvent(input$open_btn, {
      sel <- selected_projects(); req(sel); req(nrow(sel) == 1L)
      res <- tryCatch(project_load(state, sel$path[1]),
                       error = function(e) {
                         flash(paste("Open failed:", conditionMessage(e)),
                               "error"); NULL })
      req(res)
      flash(sprintf("Loaded project '%s'.", sel$name[1]), "message")
      # Switch to Model Lab if a parent_session is wired; otherwise the
      # user already sees the workspace populated.
      try(bslib::nav_select("main_nav", "6 · Model Lab"), silent = TRUE)
    })

    observeEvent(input$reproduce_btn, {
      sel <- selected_projects(); req(sel); req(nrow(sel) == 1L)
      tol <- as.numeric(input$repro_tolerance %||% 1e-6)
      res <- tryCatch(project_reproduce(sel$path[1], tolerance = tol),
                       error = function(e) {
                         flash(paste("Reproduce failed:",
                                       conditionMessage(e)), "error"); NULL })
      req(res)
      msg <- sprintf("[Reproduce '%s'] %s | original=%s replay=%s diff=%s tol=%g",
                     sel$name[1], res$message,
                     format(res$original), format(res$replay),
                     format(res$diff), tol)
      flash(msg, if (isTRUE(res$ok)) "message" else "warning")
    })

    observeEvent(input$delete_btn, {
      sel <- selected_projects(); req(sel); req(nrow(sel) == 1L)
      showModal(modalDialog(
        title = "Delete project bundle?",
        sprintf("This will permanently remove '%s' from disk.",
                sel$name[1]),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("delete_confirm"), "Delete",
                        class = "btn-danger")),
        easyClose = TRUE))
    })
    observeEvent(input$delete_confirm, {
      sel <- selected_projects(); removeModal()
      req(sel); req(nrow(sel) == 1L)
      ok <- tryCatch(project_delete(sel$path[1]),
                      error = function(e) FALSE)
      flash(if (isTRUE(ok)) sprintf("Deleted '%s'.", sel$name[1])
            else "Delete failed (path may be locked).",
            if (isTRUE(ok)) "message" else "error")
      refresh_all()
    })

    observeEvent(input$diff_btn, {
      sel <- selected_projects()
      if (is.null(sel) || nrow(sel) != 2L) {
        flash("Select exactly 2 projects to diff.", "warning"); return()
      }
      d <- tryCatch(project_diff(sel$path[1], sel$path[2]),
                     error = function(e) {
                       flash(paste("Diff failed:", conditionMessage(e)),
                             "error"); NULL })
      req(d)
      diff_result(list(a = sel$name[1], b = sel$name[2], df = d))
    })

    output$diff_table <- DT::renderDT({
      r <- diff_result()
      if (is.null(r))
        return(DT::datatable(
          data.frame(message = "No diff yet. Select 2 rows and click 'Diff selected pair'.")))
      df <- r$df
      names(df)[2:3] <- c(r$a, r$b)
      DT::datatable(df,
        options = list(dom = "tip", pageLength = 15, scrollX = TRUE),
        rownames = FALSE, class = "compact stripe")
    })

    # ---- Stats explorer (searchable) ---------------------------------
    output$stats_table <- DT::renderDT({
      d <- stats_data()
      if (nrow(d) == 0)
        return(DT::datatable(data.frame(message = "No runs yet — train a model first.")))
      DT::datatable(
        d,
        filter = "top",
        extensions = c("Buttons"),
        options = list(pageLength = 25, scrollX = TRUE,
                       dom = "Bfrtip",
                       buttons = c("copy","csv","excel"),
                       searchHighlight = TRUE,
                       order = list(list(0, "desc"))),
        rownames = FALSE, class = "compact stripe"
      )
    })

    # ---- Runs leaderboard --------------------------------------------
    output$table <- DT::renderDT({
      d <- runs_data()
      if (nrow(d) == 0) return(DT::datatable(data.frame(message = "No runs yet")))
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
