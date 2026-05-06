# =====================================================================
# Module 6: Results Dashboard
# Shows the outcome of the latest prediction run with KPI tiles,
# residual diagnostics, searchable per-row predictions, and an
# objective AI verdict (with heuristic fallback).
# Author: Malik Hebbat
# =====================================================================

dashboard_ui <- function(id) {
  ns <- NS(id)
  div(
    conditionalPanel(
      condition = sprintf("!output['%s']", ns("has_run")),
      tags$div(class = "alert alert-warning",
               "Run a model first \u2014 the dashboard populates automatically when training finishes.")
    ),
    conditionalPanel(
      condition = sprintf("output['%s']", ns("has_run")),

      # ---- KPI tile row ---------------------------------------------
      uiOutput(ns("kpi_row")),

      navset_card_underline(
        title = "Outcome",
        # Forecast / fit
        nav_panel("Forecast & Fit",
          icon = icon("chart-line"),
          withSpinner(plotlyOutput(ns("fit_plot"), height = "420px"))
        ),
        # Residuals
        nav_panel("Residual Diagnostics",
          icon = icon("wave-square"),
          layout_columns(col_widths = c(6, 6),
            withSpinner(plotlyOutput(ns("resid_fitted"), height = "320px")),
            withSpinner(plotlyOutput(ns("resid_hist"),   height = "320px"))),
          layout_columns(col_widths = c(6, 6),
            withSpinner(plotlyOutput(ns("resid_qq"),     height = "320px")),
            withSpinner(plotlyOutput(ns("resid_acf"),    height = "320px")))
        ),
        # Searchable predictions
        nav_panel("Predictions (searchable)",
          icon = icon("magnifying-glass"),
          div(style = "margin-bottom:8px;",
              downloadButton(ns("dl_csv"), "Download CSV",
                             class = "btn-sm btn-outline-success")),
          withSpinner(DT::DTOutput(ns("pred_table")))
        ),
        # All metrics as searchable cards
        nav_panel("Stats (searchable)",
          icon = icon("table-list"),
          textInput(ns("stat_search"),
                    NULL, placeholder = "Filter metrics & hyperparameters\u2026",
                    width = "100%"),
          uiOutput(ns("stat_cards"))
        ),
        # AI Verdict
        nav_panel("AI Verdict",
          icon = icon("robot"),
          div(style = "display:flex; gap:8px; align-items:center; flex-wrap:wrap; margin-bottom:10px;",
            selectInput(ns("ai_provider"), NULL,
                        choices = c("Auto-detect"="auto",
                                    "OpenAI"="openai",
                                    "Anthropic (Claude)"="anthropic",
                                    "Perplexity"="perplexity",
                                    "Heuristic (no API)"="heuristic"),
                        selected = "auto", width = "220px"),
            actionButton(ns("ai_run"), "Analyze with AI",
                         icon = icon("wand-magic-sparkles"),
                         class = "btn-primary"),
            uiOutput(ns("ai_provider_status"))
          ),
          withSpinner(uiOutput(ns("ai_panel")))
        )
      )
    )
  )
}

dashboard_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$has_run <- reactive({ !is.null(state$last_model) })
    outputOptions(output, "has_run", suspendWhenHidden = FALSE)

    # ---- helpers ----------------------------------------------------
    .residuals <- reactive({
      req(state$last_model)
      a <- as.numeric(state$last_model$actual)
      p <- as.numeric(state$last_model$predicted)
      a - p
    })

    .pred_df <- reactive({
      req(state$last_model)
      data.frame(
        idx       = seq_along(state$last_model$predicted),
        actual    = as.numeric(state$last_model$actual),
        predicted = as.numeric(state$last_model$predicted),
        residual  = as.numeric(state$last_model$actual) -
                    as.numeric(state$last_model$predicted),
        lower     = if (length(state$last_model$lower) ==
                        length(state$last_model$predicted))
                      as.numeric(state$last_model$lower) else NA_real_,
        upper     = if (length(state$last_model$upper) ==
                        length(state$last_model$predicted))
                      as.numeric(state$last_model$upper) else NA_real_
      )
    })

    # ---- KPI tile row -----------------------------------------------
    output$kpi_row <- renderUI({
      req(state$last_model, state$meta)
      m <- state$last_model$metrics
      task <- state$meta$task_type
      primary_name <- if (task %in% c("binary_classification","multiclass_classification"))
                        "accuracy" else "rmse"
      primary_val  <- m[[primary_name]] %||% NA
      n_test <- length(state$last_model$predicted)

      tile <- function(label, value, sub = NULL, color = "#3fb950") {
        tags$div(class = "card", style = "flex:1; min-width:160px; background:#161b22;",
          tags$div(class = "card-body", style = "padding:14px;",
            tags$div(style = "font-size:0.75em; color:#8b949e; text-transform:uppercase; letter-spacing:0.5px;", label),
            tags$div(style = sprintf("font-size:1.6em; font-weight:700; color:%s; margin-top:4px;", color),
                     as.character(value)),
            if (!is.null(sub))
              tags$div(style = "font-size:0.8em; color:#8b949e; margin-top:2px;", sub)
          ))
      }

      task_color <- switch(task,
        "regression"="#3fb950","time_series"="#3fb950",
        "binary_classification"="#58a6ff","multiclass_classification"="#58a6ff",
        "count"="#f1c40f","proportion"="#e67e22","#3fb950")

      tags$div(style = "display:flex; gap:10px; flex-wrap:wrap; margin-bottom:14px;",
        tile(toupper(primary_name), formatC(primary_val, digits = 4, format = "g"),
             sub = sprintf("%s \u2022 target=%s", task, state$meta$target),
             color = task_color),
        if (!is.null(m$mae))      tile("MAE",      formatC(m$mae,      4, format="g")),
        if (!is.null(m$mape))     tile("MAPE",     paste0(formatC(m$mape, 3, format="g"), "%")),
        if (!is.null(m$r2))       tile("R\u00b2",  formatC(m$r2,       4, format="g")),
        if (!is.null(m$f1))       tile("F1",       formatC(m$f1,       4, format="g")),
        if (!is.null(m$precision))tile("Precision",formatC(m$precision,4, format="g")),
        if (!is.null(m$recall))   tile("Recall",   formatC(m$recall,   4, format="g")),
        tile("Test rows", n_test, sub = sprintf("freq=%s", state$meta$frequency %||% "n/a"),
             color = "#8b949e"),
        tile("Run #", state$last_run_id %||% "\u2014",
             sub = state$last_model$model_id %||% "",
             color = "#8b949e")
      )
    })

    # ---- Plots -------------------------------------------------------
    output$fit_plot <- renderPlotly({
      d <- .pred_df()
      p <- plot_ly(d, x = ~idx) |>
        add_lines(y = ~actual,    name = "Actual",
                  line = list(color = "#c9d1d9")) |>
        add_lines(y = ~predicted, name = "Predicted",
                  line = list(color = "#3fb950"))
      if (!all(is.na(d$lower)))
        p <- add_ribbons(p, ymin = ~lower, ymax = ~upper, name = "PI",
                         fillcolor = "rgba(63,185,80,0.2)",
                         line = list(color = "transparent"))
      p |> layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
                  font = list(color = "#c9d1d9"),
                  xaxis = list(title = "Index"),
                  yaxis = list(title = state$meta$target %||% "y"))
    })

    output$resid_fitted <- renderPlotly({
      d <- .pred_df()
      plot_ly(x = d$predicted, y = d$residual, type = "scatter", mode = "markers",
              marker = list(color = "#3fb950", opacity = 0.7)) |>
        layout(title = "Residuals vs Fitted",
               paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
               font = list(color = "#c9d1d9"),
               xaxis = list(title = "Predicted"),
               yaxis = list(title = "Residual"),
               shapes = list(list(type = "line",
                                  x0 = min(d$predicted, na.rm=TRUE),
                                  x1 = max(d$predicted, na.rm=TRUE),
                                  y0 = 0, y1 = 0,
                                  line = list(dash = "dash", color = "#888"))))
    })

    output$resid_hist <- renderPlotly({
      r <- .residuals()
      plot_ly(x = r, type = "histogram", nbinsx = 30,
              marker = list(color = "#3fb950")) |>
        layout(title = "Residual Histogram",
               paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
               font = list(color = "#c9d1d9"),
               xaxis = list(title = "Residual"))
    })

    output$resid_qq <- renderPlotly({
      r <- .residuals()
      r <- r[is.finite(r)]
      if (length(r) < 3) return(plotly_empty())
      qq <- qqnorm(r, plot.it = FALSE)
      plot_ly(x = qq$x, y = qq$y, type = "scatter", mode = "markers",
              marker = list(color = "#3fb950", opacity = 0.7)) |>
        add_lines(x = c(min(qq$x), max(qq$x)),
                  y = c(min(qq$y), max(qq$y)),
                  line = list(dash = "dash", color = "#888"),
                  showlegend = FALSE) |>
        layout(title = "Q-Q (normality of residuals)",
               paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
               font = list(color = "#c9d1d9"),
               xaxis = list(title = "Theoretical"),
               yaxis = list(title = "Sample"))
    })

    output$resid_acf <- renderPlotly({
      r <- .residuals()
      r <- r[is.finite(r)]
      if (length(r) < 5) return(plotly_empty())
      lag.max <- min(20, length(r) - 1)
      ac <- acf(r, lag.max = lag.max, plot = FALSE)
      ci <- qnorm(0.975) / sqrt(length(r))
      plot_ly(x = as.numeric(ac$lag), y = as.numeric(ac$acf), type = "bar",
              marker = list(color = "#3fb950")) |>
        layout(title = "Residual ACF (autocorrelation by lag)",
               paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
               font = list(color = "#c9d1d9"),
               xaxis = list(title = "Lag"),
               yaxis = list(title = "ACF"),
               shapes = list(
                 list(type = "line", x0 = 0, x1 = lag.max, y0 = ci, y1 = ci,
                      line = list(dash = "dash", color = "#d9534f")),
                 list(type = "line", x0 = 0, x1 = lag.max, y0 = -ci, y1 = -ci,
                      line = list(dash = "dash", color = "#d9534f"))))
    })

    # ---- Searchable per-row predictions ----------------------------
    output$pred_table <- DT::renderDT({
      DT::datatable(.pred_df(),
        filter = "top",
        options = list(pageLength = 25, scrollX = TRUE, dom = "ftip",
                       searchHighlight = TRUE),
        rownames = FALSE, class = "compact stripe")
    })

    output$dl_csv <- downloadHandler(
      filename = function() sprintf("predictions_run_%s.csv",
                                     state$last_run_id %||% "latest"),
      content = function(file) write.csv(.pred_df(), file, row.names = FALSE)
    )

    # ---- Searchable stat cards --------------------------------------
    .all_stats <- reactive({
      req(state$last_model)
      m <- state$last_model$metrics
      params <- state$last_params %||% list()
      out <- list()
      for (nm in names(m))
        out[[length(out)+1]] <- list(kind = "metric", key = nm, value = m[[nm]])
      for (nm in names(params))
        out[[length(out)+1]] <- list(kind = "param", key = nm,
                                       value = params[[nm]])
      # Add residual diagnostics
      r <- .residuals(); r <- r[is.finite(r)]
      if (length(r) > 5) {
        out[[length(out)+1]] <- list(kind = "diagnostic", key = "bias",
                                       value = round(mean(r), 6))
        out[[length(out)+1]] <- list(kind = "diagnostic", key = "sd_residual",
                                       value = round(sd(r), 6))
        if (length(r) > 10) {
          ac <- tryCatch(as.numeric(acf(r, plot = FALSE,
                                          lag.max = 1)$acf[2]),
                          error = function(e) NA)
          out[[length(out)+1]] <- list(kind = "diagnostic", key = "acf_lag1",
                                         value = round(ac, 4))
        }
      }
      out
    })

    output$stat_cards <- renderUI({
      stats <- .all_stats()
      q <- tolower(input$stat_search %||% "")
      if (nzchar(q))
        stats <- Filter(function(s)
          grepl(q, tolower(paste(s$key, s$value, s$kind)), fixed = TRUE), stats)
      if (length(stats) == 0)
        return(tags$div(class = "text-muted", "No stats match your filter."))
      tiles <- lapply(stats, function(s) {
        kind_color <- switch(s$kind, "metric"="#3fb950","param"="#58a6ff",
                             "diagnostic"="#f1c40f","#8b949e")
        tags$div(class = "card", style = "min-width:180px; flex:0 0 auto; background:#161b22; margin-right:10px; margin-bottom:10px;",
          tags$div(class = "card-body", style = "padding:10px 12px;",
            tags$div(style = sprintf("font-size:0.65em; text-transform:uppercase; letter-spacing:0.4px; color:%s; font-weight:700;", kind_color),
                     s$kind),
            tags$div(style = "font-size:0.85em; color:#8b949e; margin-top:2px;", s$key),
            tags$div(style = "font-size:1.25em; font-weight:600; color:#c9d1d9; margin-top:2px;",
                     as.character(s$value))))
      })
      tags$div(style = "display:flex; flex-wrap:wrap;", tiles)
    })

    # ---- AI Verdict --------------------------------------------------
    output$ai_provider_status <- renderUI({
      keys <- c(
        OpenAI    = nzchar(Sys.getenv("OPENAI_API_KEY")),
        Anthropic = nzchar(Sys.getenv("ANTHROPIC_API_KEY")),
        Perplexity= nzchar(Sys.getenv("PPLX_API_KEY"))
      )
      avail <- names(keys)[keys]
      if (length(avail) == 0)
        tags$small(class = "text-muted",
          "No API keys detected. Heuristic fallback will be used. Set OPENAI_API_KEY, ANTHROPIC_API_KEY, or PPLX_API_KEY.")
      else
        tags$small(class = "text-muted",
                   sprintf("Detected keys: %s", paste(avail, collapse = ", ")))
    })

    ai_result <- reactiveVal(NULL)

    # Auto-load existing analysis when a run is selected
    observeEvent(state$last_run_id, {
      if (is.null(state$last_run_id)) return()
      a <- db_get_analysis(state$last_run_id)
      if (!is.null(a)) {
        ai_result(list(grade = a$grade, verdict = a$verdict,
                       analysis_md = a$analysis_md,
                       suggestions = tryCatch(jsonlite::fromJSON(a$suggestions_json,
                                                                   simplifyVector = FALSE),
                                                error = function(e) list()),
                       provider = a$provider))
      } else ai_result(NULL)
    })

    observeEvent(input$ai_run, {
      req(state$last_model, state$last_run_id)
      withProgress(message = "Asking the AI for an objective verdict\u2026", {
        out <- tryCatch(run_ai_analysis(state, provider = input$ai_provider),
                        error = function(e) {
                          flash(paste("AI failed:", conditionMessage(e)), "error")
                          NULL
                        })
        req(out)
        ai_result(out)
        db_save_analysis(state$last_run_id, out$provider %||% "heuristic",
                         out$verdict %||% "", out$grade %||% "WEAK",
                         out$analysis_md %||% "",
                         out$suggestions %||% list())
        flash(sprintf("AI verdict ready (%s)", out$provider), "message")
      })
    })

    output$ai_panel <- renderUI({
      r <- ai_result()
      if (is.null(r))
        return(tags$div(class = "alert alert-secondary",
                        "Click 'Analyze with AI' to generate an objective verdict."))
      grade <- r$grade %||% "WEAK"
      grade_color <- switch(grade, "PASS"="#3fb950","WEAK"="#f1c40f",
                              "FAIL"="#d9534f","#8b949e")
      sugg_ui <- if (length(r$suggestions) > 0) {
        tagList(
          tags$h6("Suggested next steps"),
          tags$ul(lapply(r$suggestions, function(s) {
            action <- s$action %||% s[[1]]
            reason <- s$reason %||% s[[2]] %||% ""
            tags$li(tags$b(action), if (nzchar(reason)) ": ", reason)
          }))
        )
      } else NULL

      tagList(
        tags$div(style = sprintf("display:inline-block; padding:6px 14px; border-radius:6px; background:%s; color:#0d1117; font-weight:700; margin-bottom:8px;", grade_color),
                 grade),
        tags$div(style = "font-size:1.05em; margin-bottom:10px; color:#c9d1d9;",
                 r$verdict %||% ""),
        tags$div(class = "alert alert-secondary",
                 style = "white-space:pre-wrap; line-height:1.5;",
                 r$analysis_md %||% ""),
        sugg_ui,
        tags$small(class = "text-muted",
                   sprintf("Provider: %s", r$provider %||% "heuristic"))
      )
    })
  })
}
