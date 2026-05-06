# =====================================================================
# Module 4: Forecast / Predict
#   Use the last trained model to produce out-of-sample forecasts /
#   predictions over a user-defined horizon, or apply to a new file.
# =====================================================================

predict_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header("Predict / Forecast"),
      conditionalPanel(
        condition = sprintf("output['%s']", ns("has_model")),
        uiOutput(ns("model_info")),
        hr(),
        conditionalPanel(
          condition = sprintf("output['%s']", ns("is_ts")),
          numericInput(ns("horizon"), "Forecast horizon (steps)",
                       value = 12, min = 1, max = 1000)
        ),
        conditionalPanel(
          condition = sprintf("!output['%s']", ns("is_ts")),
          fileInput(ns("new_file"), "Predict on new data (optional)",
                    accept = c(".csv",".tsv",".txt",".xlsx",".parquet",".json"))
        ),
        actionButton(ns("run"), "Predict ▶", class = "btn-primary w-100"),
        hr(),
        downloadButton(ns("download"), "Download predictions (CSV)",
                       class = "btn-outline-success w-100")
      ),
      conditionalPanel(
        condition = sprintf("!output['%s']", ns("has_model")),
        tags$div(class = "alert alert-warning",
                 "Train a model in Model Lab first.")
      )
    ),
    navset_card_underline(
      title = "Forecast / Predictions",
      nav_panel("Plot", withSpinner(plotlyOutput(ns("plot"), height = "500px"))),
      nav_panel("Table", withSpinner(DT::DTOutput(ns("table"))))
    )
  )
}

predict_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$has_model <- reactive({ !is.null(state$last_model) })
    outputOptions(output, "has_model", suspendWhenHidden = FALSE)

    output$is_ts <- reactive({
      !is.null(state$meta) && isTRUE(state$meta$task_type == "time_series")
    })
    outputOptions(output, "is_ts", suspendWhenHidden = FALSE)

    output$model_info <- renderUI({
      req(state$last_run_id, state$meta)
      tags$div(class = "alert alert-secondary",
               "Last run #", tags$b(state$last_run_id),
               " · ", tags$b(state$meta$task_type),
               br(), tags$small(fmt_metrics(state$last_model$metrics)))
    })

    pred_data <- reactiveVal(NULL)

    observeEvent(input$run, {
      req(state$last_model, state$meta)
      task <- state$meta$task_type
      fit  <- state$last_model$fit

      if (task == "time_series") {
        h <- input$horizon %||% 12
        out <- tryCatch(fit$predict(NULL, h = h),
                        error = function(e) {
                          flash(conditionMessage(e), "error"); NULL
                        })
        req(out)
        d <- if (is.data.frame(out)) out else
              data.frame(predicted = as.numeric(out), lower = NA, upper = NA)
        d$step <- seq_len(nrow(d))
        d$horizon <- horizon_label(state$meta$frequency, nrow(d))
        pred_data(d)
        flash(sprintf("Generated %d-step forecast", nrow(d)), "message")
      } else {
        # Apply to new uploaded file or to the holdout already in last_model
        if (!is.null(input$new_file)) {
          newdf <- tryCatch(read_uploaded(input$new_file$datapath, input$new_file$name),
                            error = function(e) { flash(conditionMessage(e), "error"); NULL })
          req(newdf)
          if (nrow(newdf) == 0) { flash("Uploaded file has no rows", "error"); return() }
          newdf[[state$meta$target]] <- NULL
          if (!is.null(state$meta$time_col) && state$meta$time_col != "(none)")
            newdf[[state$meta$time_col]] <- NULL
          out <- tryCatch(fit$predict(newdf),
                          error = function(e) { flash(conditionMessage(e), "error"); NULL })
          req(out); req(length(out) > 0)
          d <- data.frame(predicted = as.numeric(out))
          pred_data(d)
          flash(sprintf("Predicted %d new rows", nrow(d)), "message")
        } else {
          d <- data.frame(actual = as.numeric(state$last_model$actual),
                          predicted = as.numeric(state$last_model$predicted))
          pred_data(d)
        }
      }
    })

    output$plot <- renderPlotly({
      d <- pred_data(); req(d)
      if ("step" %in% names(d)) {
        # Time series forecast
        hist_y <- as.numeric(state$prepped[[state$meta$target]])
        hist_x <- seq_along(hist_y)
        fc_x   <- max(hist_x) + d$step
        p <- plot_ly() |>
          add_lines(x = hist_x, y = hist_y, name = "History",
                    line = list(color = "#c9d1d9")) |>
          add_lines(x = fc_x, y = d$predicted, name = "Forecast",
                    line = list(color = "#3fb950"))
        if (!all(is.na(d$lower)))
          p <- p |> add_ribbons(x = fc_x, ymin = d$lower, ymax = d$upper,
                                name = "PI",
                                fillcolor = "rgba(63,185,80,0.2)",
                                line = list(color = "transparent"))
        p |> layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
                    font = list(color = "#c9d1d9"),
                    xaxis = list(title = "Time"),
                    yaxis = list(title = state$meta$target))
      } else if ("actual" %in% names(d)) {
        plot_ly(d, x = ~actual, y = ~predicted, type = "scatter",
                mode = "markers",
                marker = list(color = "#3fb950", opacity = 0.7)) |>
          add_lines(x = ~actual, y = ~actual, name = "y=x",
                    line = list(dash = "dash", color = "#888")) |>
          layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
                 font = list(color = "#c9d1d9"))
      } else {
        plot_ly(x = seq_len(nrow(d)), y = d$predicted, type = "scatter",
                mode = "lines+markers",
                marker = list(color = "#3fb950")) |>
          layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
                 font = list(color = "#c9d1d9"))
      }
    })

    output$table <- DT::renderDT({
      d <- pred_data(); req(d)
      DT::datatable(d, options = list(pageLength = 25, dom = "tip", scrollX = TRUE),
                    rownames = FALSE, class = "compact stripe")
    })

    output$download <- downloadHandler(
      filename = function() sprintf("predictions_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        d <- pred_data(); req(d)
        write.csv(d, file, row.names = FALSE)
      }
    )
  })
}
