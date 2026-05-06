# =====================================================================
# Module 3: Model Lab
#   Pick model(s) for the detected task, tune hyperparameters,
#   train + evaluate on holdout / CV.
# =====================================================================

modellab_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header("Pick model & tune"),
      conditionalPanel(
        condition = sprintf("output['%s']", ns("ready")),
        uiOutput(ns("task_badge")),
        selectInput(ns("model_id"), "Model", choices = NULL),
        uiOutput(ns("model_engine")),
        hr(),
        h6("Hyperparameters"),
        uiOutput(ns("param_ui")),
        hr(),
        h6("Validation"),
        radioButtons(ns("val_scheme"), NULL,
                     choices = c("Holdout (train/test split)" = "holdout",
                                 "K-fold CV" = "kfold",
                                 "Rolling-origin (time series)" = "rolling"),
                     selected = "holdout"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'kfold'", ns("val_scheme")),
          numericInput(ns("kfolds"), "Folds (k)", value = 5, min = 2, max = 20)),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'rolling'", ns("val_scheme")),
          numericInput(ns("ro_initial"), "Initial window", value = 50, min = 10),
          numericInput(ns("ro_h"), "Forecast step (h)", value = 12, min = 1)),
        actionButton(ns("train"), "Train & Evaluate ▶",
                     class = "btn-primary w-100")
      ),
      conditionalPanel(
        condition = sprintf("!output['%s']", ns("ready")),
        tags$div(class = "alert alert-warning",
                 "Save a dataset (Ingest) and apply a pipeline (Explore) first.")
      )
    ),
    navset_card_underline(
      title = "Results",
      nav_panel("Metrics",      withSpinner(verbatimTextOutput(ns("metrics_box")))),
      nav_panel("Predictions",  withSpinner(plotlyOutput(ns("pred_plot"), height = "400px"))),
      nav_panel("Residuals",    withSpinner(plotlyOutput(ns("resid_plot"), height = "400px"))),
      nav_panel("Feature importance",
                withSpinner(plotlyOutput(ns("imp_plot"), height = "400px"))),
      nav_panel("Console log",  withSpinner(verbatimTextOutput(ns("log_box"))))
    )
  )
}

modellab_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    log_msgs <- reactiveVal(character(0))
    add_log <- function(...) log_msgs(c(log_msgs(),
      paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", paste0(..., collapse=""))))

    output$ready <- reactive({
      !is.null(state$prepped) && !is.null(state$meta)
    })
    outputOptions(output, "ready", suspendWhenHidden = FALSE)

    output$task_badge <- renderUI({
      req(state$meta)
      tags$div(class = "alert alert-info",
               style = "padding:6px 10px;",
               icon("crosshairs"),
               "Task: ", tags$b(state$meta$task_type),
               " · Target: ", tags$b(state$meta$target),
               " · Frequency: ", tags$b(state$meta$frequency))
    })

    available_models <- reactive({
      req(state$meta)
      models_for_task(state$meta$task_type)
    })

    observe({
      mods <- available_models()
      choices <- setNames(names(mods), sapply(mods, function(m) m$label))
      updateSelectInput(session, "model_id", choices = choices,
                        selected = names(mods)[1])
    })

    output$model_engine <- renderUI({
      req(input$model_id)
      m <- MODELS[[input$model_id]]
      if (is.null(m)) return(NULL)
      tagList(
        tags$div(style = "margin-top:-6px; margin-bottom:4px;",
                 engine_badge(m$engine),
                 if (m$engine == "Python")
                   tags$small(class="text-muted",
                     " · requires Python via reticulate")),
        if (!is.null(m$description) && nzchar(m$description))
          tags$div(
            class = "alert alert-secondary",
            style = "font-size:0.85em; padding:8px 10px; margin-top:6px; line-height:1.45;",
            icon("circle-info"), tags$b(" About this model"), br(),
            m$description
          )
      )
    })

    output$param_ui <- renderUI({
      req(input$model_id)
      m <- MODELS[[input$model_id]]
      if (is.null(m) || length(m$params) == 0)
        return(tags$em(class = "text-muted", "No tunable hyperparameters."))
      lapply(m$params, function(p) {
        nm <- ns(paste0("param_", p$name))
        switch(p$type,
          "numeric" = numericInput(nm, p$label, value = p$default,
                                   min = p$min, max = p$max, step = p$step),
          "integer" = numericInput(nm, p$label, value = p$default,
                                   min = p$min, max = p$max, step = p$step %||% 1),
          "logical" = checkboxInput(nm, p$label, value = isTRUE(p$default)),
          "select"  = selectInput(nm, p$label, choices = p$choices, selected = p$default),
          "text"    = textInput(nm, p$label, value = p$default)
        )
      })
    })

    collect_params <- reactive({
      m <- MODELS[[input$model_id]]
      if (is.null(m)) return(list())
      out <- list()
      for (p in m$params) {
        v <- input[[paste0("param_", p$name)]]
        if (is.null(v)) next
        if (p$type == "integer") v <- as.integer(v)
        if (p$type == "numeric") v <- as.numeric(v)
        out[[p$name]] <- v
      }
      out
    })

    # ---- Train! -------------------------------------------------------
    observeEvent(input$train, {
      req(state$prepped, state$meta, input$model_id)

      df <- state$prepped
      target <- state$meta$target
      tcol <- state$meta$time_col
      tcol <- if (!is.null(tcol) && !is.na(tcol) && tcol != "(none)") tcol else NULL
      task <- state$meta$task_type
      m <- MODELS[[input$model_id]]
      params <- collect_params()

      add_log("Training ", m$label, " (", m$engine, ")")

      # Prepare modelling frame: drop time column from features
      mdf <- df
      if (!is.null(tcol)) mdf[[tcol]] <- NULL

      # Decide split scheme
      vs <- input$val_scheme
      n <- nrow(mdf)
      idx_train <- seq_len(floor(n * 0.8))
      if (task == "time_series") {
        # Always temporal split: last 20% as holdout
        cut <- floor(n * 0.8)
        train_df <- mdf[seq_len(cut), , drop = FALSE]
        test_df  <- mdf[-seq_len(cut), , drop = FALSE]
      } else {
        set.seed(42)
        idx <- sample(seq_len(n), size = floor(n * 0.8))
        train_df <- mdf[idx, , drop = FALSE]
        test_df  <- mdf[-idx, , drop = FALSE]
      }

      t0 <- Sys.time()
      out <- tryCatch({
        fit <- m$fn(train_df, target, params, time_col = tcol)

        # Time series: predict horizon
        if (task == "time_series") {
          h <- nrow(test_df)
          preds <- fit$predict(test_df, h = h)
          if (is.data.frame(preds)) {
            pred_vec <- preds$predicted
            lower <- preds$lower; upper <- preds$upper
          } else {
            pred_vec <- as.numeric(preds); lower <- NA; upper <- NA
          }
          actual <- test_df[[target]]
          metrics <- calc_regression_metrics(actual, pred_vec)
          res <- list(actual = actual, predicted = pred_vec,
                      lower = lower, upper = upper,
                      metrics = metrics, fit = fit)
        } else {
          pred_vec <- fit$predict(test_df)
          actual <- test_df[[target]]
          metrics <- if (task %in% c("binary_classification","multiclass_classification"))
            calc_classification_metrics(actual, pred_vec)
          else calc_regression_metrics(actual, pred_vec)
          res <- list(actual = actual, predicted = pred_vec,
                      lower = NA, upper = NA,
                      metrics = metrics, fit = fit)
        }
        res
      }, error = function(e) {
        add_log("ERROR: ", conditionMessage(e))
        flash(paste("Training failed:", conditionMessage(e)), "error")
        NULL
      })

      duration <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      req(out)
      state$last_model <- out
      add_log(sprintf("Done in %.2fs · %s", duration, fmt_metrics(out$metrics)))

      # Save run + predictions
      run_id <- db_save_run(state$dataset_id, m$id, task, params, out$metrics,
                            horizon = if (task == "time_series") length(out$predicted) else NA,
                            duration = duration)
      preds_df <- data.frame(
        actual    = as.numeric(out$actual),
        predicted = as.numeric(out$predicted),
        lower     = if (length(out$lower) == length(out$predicted)) out$lower else NA_real_,
        upper     = if (length(out$upper) == length(out$predicted)) out$upper else NA_real_,
        ts        = NA_character_
      )
      db_save_predictions(run_id, preds_df)
      state$last_run_id <- run_id
    })

    # ---- Output panels ------------------------------------------------
    output$metrics_box <- renderText({
      req(state$last_model)
      m <- state$last_model$metrics
      paste(sprintf("  %-10s : %s", names(m), unlist(m)), collapse = "\n")
    })

    output$pred_plot <- renderPlotly({
      req(state$last_model)
      d <- data.frame(idx = seq_along(state$last_model$predicted),
                      actual = as.numeric(state$last_model$actual),
                      predicted = as.numeric(state$last_model$predicted))
      p <- plot_ly(d, x = ~idx) |>
        add_lines(y = ~actual, name = "Actual", line = list(color = "#c9d1d9")) |>
        add_lines(y = ~predicted, name = "Predicted",
                  line = list(color = "#3fb950"))
      if (length(state$last_model$lower) == nrow(d) &&
          !all(is.na(state$last_model$lower))) {
        p <- p |>
          add_ribbons(ymin = state$last_model$lower,
                      ymax = state$last_model$upper,
                      name = "PI", fillcolor = "rgba(63,185,80,0.2)",
                      line = list(color = "transparent"))
      }
      p |> layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
                  font = list(color = "#c9d1d9"),
                  xaxis = list(title = "Index"),
                  yaxis = list(title = state$meta$target))
    })

    output$resid_plot <- renderPlotly({
      req(state$last_model)
      a <- as.numeric(state$last_model$actual)
      p <- as.numeric(state$last_model$predicted)
      r <- a - p
      plot_ly(x = p, y = r, type = "scatter", mode = "markers",
              marker = list(color = "#3fb950", opacity = 0.7)) |>
        layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
               font = list(color = "#c9d1d9"),
               xaxis = list(title = "Predicted"),
               yaxis = list(title = "Residual"),
               shapes = list(list(type = "line", x0 = min(p,na.rm=TRUE),
                                  x1 = max(p,na.rm=TRUE), y0 = 0, y1 = 0,
                                  line = list(dash = "dash", color = "#888"))))
    })

    output$imp_plot <- renderPlotly({
      req(state$last_model)
      imp <- state$last_model$fit$feat_imp
      if (is.null(imp) || length(imp) == 0)
        return(plotly_empty(type = "scatter") |>
                 layout(title = "Not available for this model",
                        paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
                        font = list(color = "#c9d1d9")))
      imp <- sort(imp, decreasing = TRUE)
      d <- data.frame(feature = names(imp), importance = as.numeric(imp))
      d <- utils::head(d, 25)
      plot_ly(d, x = ~importance, y = ~reorder(feature, importance),
              type = "bar", orientation = "h",
              marker = list(color = "#3fb950")) |>
        layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
               font = list(color = "#c9d1d9"),
               xaxis = list(title = "Importance"), yaxis = list(title = ""))
    })

    output$log_box <- renderText({
      paste(log_msgs(), collapse = "\n")
    })
  })
}
