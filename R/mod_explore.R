# =====================================================================
# Module 2: Explore & Prep
# =====================================================================

explore_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header("Recipe / Preprocessing"),
      conditionalPanel(
        condition = sprintf("output['%s']", ns("has_data")),
        checkboxGroupInput(ns("steps"), "Pipeline steps:",
          choices = c(
            "Drop columns with >50% missing"     = "drop_high_na",
            "Median-impute numeric NAs"          = "impute_num",
            "Mode-impute categorical NAs"        = "impute_cat",
            "One-hot encode categoricals"        = "dummy",
            "Standardize numerics (z-score)"     = "scale",
            "Add time-series lags (h=1..3)"      = "lags",
            "Add rolling mean (window 7)"        = "rollmean"
          ),
          selected = c("impute_num","impute_cat")
        ),
        hr(),
        h6("Train / Test split"),
        sliderInput(ns("split_pct"), "Train %", 0.5, 0.95, 0.8, 0.05),
        actionButton(ns("apply"), "Apply pipeline",
                     class = "btn-primary w-100")
      ),
      conditionalPanel(
        condition = sprintf("!output['%s']", ns("has_data")),
        tags$div(class = "alert alert-warning",
                 "Upload & save a dataset first.")
      )
    ),
    navset_card_underline(
      title = "Diagnostics",
      nav_panel("Schema",     withSpinner(DT::DTOutput(ns("schema")))),
      nav_panel("Missingness",withSpinner(plotlyOutput(ns("miss_plot"), height = "350px"))),
      nav_panel("Target",     withSpinner(plotlyOutput(ns("target_plot"), height = "350px"))),
      nav_panel("Correlation",withSpinner(plotlyOutput(ns("cor_plot"),    height = "450px"))),
      nav_panel("Prepped data", withSpinner(DT::DTOutput(ns("prepped"))))
    )
  )
}

explore_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$has_data <- reactive({ !is.null(state$raw_data) })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # Schema
    output$schema <- DT::renderDT({
      req(state$raw_data)
      df <- state$raw_data
      sch <- data.frame(
        column   = names(df),
        type     = sapply(df, function(c) class(c)[1]),
        n_missing= sapply(df, function(c) sum(is.na(c))),
        n_unique = sapply(df, function(c) length(unique(c))),
        sample   = sapply(df, function(c) paste(utils::head(unique(c), 3), collapse = ", "))
      )
      DT::datatable(sch, options = list(pageLength = 25, dom = "tip"),
                    rownames = FALSE, class = "compact")
    })

    # Missingness
    output$miss_plot <- renderPlotly({
      req(state$raw_data)
      m <- data.frame(
        column = names(state$raw_data),
        pct = sapply(state$raw_data, function(c) mean(is.na(c)))*100
      )
      m <- m[order(-m$pct), ]
      plot_ly(m, x = ~pct, y = ~reorder(column, pct), type = "bar", orientation = "h",
              marker = list(color = "#3fb950")) |>
        layout(xaxis = list(title = "% missing"), yaxis = list(title = ""),
               paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
               font = list(color = "#c9d1d9"))
    })

    # Target distribution
    output$target_plot <- renderPlotly({
      req(state$raw_data, state$meta)
      y <- state$raw_data[[state$meta$target]]
      if (is.numeric(y)) {
        plot_ly(x = y, type = "histogram", marker = list(color = "#3fb950")) |>
          layout(title = paste("Distribution of", state$meta$target),
                 paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
                 font = list(color = "#c9d1d9"))
      } else {
        d <- as.data.frame(table(y))
        plot_ly(d, x = ~y, y = ~Freq, type = "bar",
                marker = list(color = "#3fb950")) |>
          layout(title = paste("Counts of", state$meta$target),
                 paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
                 font = list(color = "#c9d1d9"))
      }
    })

    # Correlation
    output$cor_plot <- renderPlotly({
      req(state$raw_data)
      num <- state$raw_data[, sapply(state$raw_data, is.numeric), drop = FALSE]
      if (ncol(num) < 2) return(plotly_empty())
      C <- cor(num, use = "pairwise.complete.obs")
      plot_ly(x = colnames(C), y = colnames(C), z = C,
              type = "heatmap", colors = colorRamp(c("#d9534f","#ffffff","#3fb950"))) |>
        layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
               font = list(color = "#c9d1d9"))
    })

    # ---- Apply pipeline ----------------------------------------------
    observeEvent(input$apply, {
      req(state$raw_data, state$meta)
      df <- state$raw_data
      target <- state$meta$target
      time_col <- state$meta$time_col

      steps <- input$steps %||% character(0)

      # Drop high-NA columns
      if ("drop_high_na" %in% steps) {
        keep <- sapply(df, function(c) mean(is.na(c)) <= 0.5)
        df <- df[, keep, drop = FALSE]
      }
      # Impute numeric
      if ("impute_num" %in% steps) {
        for (c in names(df)) if (is.numeric(df[[c]]))
          df[[c]][is.na(df[[c]])] <- median(df[[c]], na.rm = TRUE)
      }
      # Impute categorical
      if ("impute_cat" %in% steps) {
        for (c in names(df)) if (is.factor(df[[c]]) || is.character(df[[c]])) {
          v <- df[[c]]; tab <- sort(table(v), decreasing = TRUE)
          if (length(tab) > 0) v[is.na(v)] <- names(tab)[1]
          df[[c]] <- v
        }
      }
      # One-hot
      if ("dummy" %in% steps) {
        cat_cols <- setdiff(names(df)[sapply(df, function(c)
          is.character(c) || is.factor(c))], target)
        for (c in cat_cols) {
          if (c == time_col) next
          dum <- model.matrix(~ . -1, data = df[, c, drop = FALSE])
          colnames(dum) <- paste0(c, "_", make.names(colnames(dum)))
          df <- cbind(df[, setdiff(names(df), c)], as.data.frame(dum))
        }
      }
      # Scale
      if ("scale" %in% steps) {
        num <- setdiff(names(df)[sapply(df, is.numeric)], target)
        for (c in num) {
          v <- df[[c]]; s <- sd(v, na.rm = TRUE)
          if (!is.na(s) && s > 0) df[[c]] <- (v - mean(v, na.rm = TRUE)) / s
        }
      }
      # Time series transforms
      if ("lags" %in% steps && is.numeric(df[[target]])) {
        for (k in 1:3) df[[paste0("lag_", k)]] <- dplyr::lag(df[[target]], k)
      }
      if ("rollmean" %in% steps && is.numeric(df[[target]])) {
        df[["rollmean_7"]] <- zoo_rollmean(df[[target]], 7)
      }

      # Drop rows with NAs introduced by lags
      df <- df[complete.cases(df), , drop = FALSE]

      state$prepped <- df
      flash(sprintf("Pipeline applied: %d rows × %d cols", nrow(df), ncol(df)),
            "message")
    })

    # Show prepped data
    output$prepped <- DT::renderDT({
      req(state$prepped)
      DT::datatable(head(state$prepped, 200),
                    options = list(scrollX = TRUE, pageLength = 10, dom = "tip"),
                    rownames = FALSE, class = "compact stripe")
    })
  })
}

# Tiny rolling mean (no extra deps)
zoo_rollmean <- function(x, k) {
  n <- length(x); out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (i >= k) out[i] <- mean(x[(i-k+1):i], na.rm = TRUE)
  }
  out
}
