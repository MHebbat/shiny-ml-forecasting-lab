# =====================================================================
# Module 2: Explore (read-only diagnostics)
#
# Inspect raw data: schema, missingness, target distribution,
# correlations, prepped preview. Does NOT mutate state$prepped — that's
# the Data Prep tab's job. The header cross-links to Data Prep.
# =====================================================================

# ---------------------------------------------------------------------
# Pure helper: build a correlation summary that survives mixed-type data.
# Returned list fields:
#   ok            : TRUE if a matrix was produced
#   message       : explanation when ok = FALSE
#   matrix        : the correlation matrix (may be NULL)
#   pairs         : data.frame(var1, var2, cor, abs_cor) sorted desc
#   numeric_cols  : names of columns used
#   method        : echoed back
# Exported helper so it can be unit-tested without Shiny.
# ---------------------------------------------------------------------
explore_correlation <- function(df, method = c("pearson", "spearman", "kendall"),
                                top_n = 20) {
  method <- match.arg(method)
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
    return(list(ok = FALSE,
                message = "No data to compute correlations on.",
                matrix = NULL, pairs = NULL,
                numeric_cols = character(0), method = method))
  }
  # Coerce haven_labelled (kept by the haven reader) to numeric for cor()
  coerced <- lapply(df, function(col) {
    if (inherits(col, "haven_labelled")) return(as.numeric(unclass(col)))
    col
  })
  is_num <- vapply(coerced, function(c) is.numeric(c) || is.logical(c),
                   logical(1))
  num <- as.data.frame(coerced[is_num], stringsAsFactors = FALSE)

  # Drop columns that are entirely NA or constant — cor() returns NA for them
  num <- num[, vapply(num, function(c) {
    v <- stats::na.omit(c)
    length(v) >= 2 && stats::sd(v) > 0
  }, logical(1)), drop = FALSE]

  if (ncol(num) < 2) {
    return(list(ok = FALSE,
                message = "Need at least 2 numeric columns to compute correlations.",
                matrix = NULL, pairs = NULL,
                numeric_cols = colnames(num), method = method))
  }

  C <- tryCatch(
    suppressWarnings(stats::cor(num, use = "pairwise.complete.obs",
                                method = method)),
    error = function(e) NULL
  )
  if (is.null(C) || !is.matrix(C)) {
    return(list(ok = FALSE,
                message = paste0("Correlation could not be computed (",
                                 method, ")."),
                matrix = NULL, pairs = NULL,
                numeric_cols = colnames(num), method = method))
  }

  # Pair table (upper triangle, no self-correlations)
  nm <- colnames(C)
  pairs_df <- data.frame(var1 = character(0), var2 = character(0),
                         cor = numeric(0), abs_cor = numeric(0),
                         stringsAsFactors = FALSE)
  if (length(nm) >= 2) {
    idx <- which(upper.tri(C), arr.ind = TRUE)
    if (nrow(idx) > 0) {
      vals <- C[idx]
      pairs_df <- data.frame(
        var1   = nm[idx[, 1]],
        var2   = nm[idx[, 2]],
        cor    = round(vals, 4),
        abs_cor = round(abs(vals), 4),
        stringsAsFactors = FALSE
      )
      pairs_df <- pairs_df[!is.na(pairs_df$cor), , drop = FALSE]
      pairs_df <- pairs_df[order(-pairs_df$abs_cor), , drop = FALSE]
      if (!is.null(top_n) && nrow(pairs_df) > top_n)
        pairs_df <- utils::head(pairs_df, top_n)
    }
  }

  list(ok = TRUE, message = NULL, matrix = C, pairs = pairs_df,
       numeric_cols = nm, method = method)
}

# ---------------------------------------------------------------------
explore_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Premium hero card (compact intro variant — does NOT take full viewport)
    tags$div(class = "studio-intro",
      tags$div(class = "studio-page",
        tags$div(class = "studio-hero",
          tags$div(class = "studio-hero-meta",
            tags$span(class = "studio-kicker", "EXPLORE"),
            tags$span(class = "studio-dot", "•"),
            tags$span(class = "studio-kicker", "READ-ONLY DIAGNOSTICS")
          ),
          tags$h1(class = "studio-headline", "Look before you leap."),
          tags$p(class = "studio-deck",
            "Inspect your raw data: distributions, correlations, missingness, sample rows. ",
            "Nothing is changed here. When you're ready to clean it up, head to ",
            tags$b("Data Prep"), "."
          ),
          tags$div(class = "studio-rule"),
          tags$div(style = "margin-top:14px; display:flex; gap:8px; flex-wrap:wrap;",
            actionButton(ns("goto_dataprep"),
                         tagList("Open Data Prep", HTML(" →")),
                         class = "btn-warning"),
            actionButton(ns("goto_ingest"),
                         tagList(HTML("← "), "Back to Ingest"),
                         class = "btn-outline-secondary")
          )
        )
      )
    ),

    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Recipe / Preprocessing (legacy)"),
        conditionalPanel(
          condition = sprintf("output['%s']", ns("has_data")),
          tags$small(class = "text-muted",
            "Quick recipe shortcut. For the full pipeline, use Data Prep."),
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
        nav_panel("Correlation",
          tagList(
            tags$div(style = "padding:8px 4px 12px; display:flex; gap:12px; flex-wrap:wrap; align-items:flex-end;",
              tags$div(style = "min-width:160px;",
                selectInput(ns("cor_method"), "Method",
                            choices = c("Pearson"="pearson",
                                        "Spearman"="spearman",
                                        "Kendall"="kendall"),
                            selected = "pearson")),
              tags$div(style = "min-width:120px;",
                numericInput(ns("cor_top_n"), "Top |cor| pairs",
                             value = 20, min = 5, max = 200, step = 5))
            ),
            uiOutput(ns("cor_msg")),
            withSpinner(plotlyOutput(ns("cor_plot"), height = "450px")),
            tags$h6("Top correlated pairs", style = "margin-top:12px;"),
            DT::DTOutput(ns("cor_pairs"))
          )),
        nav_panel("Prepped data", withSpinner(DT::DTOutput(ns("prepped"))))
      )
    )
  )
}

explore_server <- function(id, state, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$has_data <- reactive({ !is.null(state$raw_data) })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # Cross-links to other top-level tabs
    observeEvent(input$goto_dataprep, {
      if (!is.null(parent_session))
        bslib::nav_select("main_nav", "3 · Data Prep",
                          session = parent_session)
    })
    observeEvent(input$goto_ingest, {
      if (!is.null(parent_session))
        bslib::nav_select("main_nav", "1 · Data Ingest",
                          session = parent_session)
    })

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

    # ---- Correlation -----------------------------------------------
    cor_result <- reactive({
      req(state$raw_data)
      explore_correlation(state$raw_data,
                          method = input$cor_method %||% "pearson",
                          top_n  = input$cor_top_n %||% 20)
    })

    output$cor_msg <- renderUI({
      r <- cor_result()
      if (isTRUE(r$ok)) {
        tags$div(class = "text-muted",
                 style = "font-size:0.85em; margin: 4px 4px 8px;",
                 sprintf("%d numeric column(s) used · method: %s · NAs handled pairwise",
                         length(r$numeric_cols), r$method))
      } else {
        tags$div(class = "alert alert-warning",
                 style = "padding:8px 10px; font-size:0.9em;",
                 icon("triangle-exclamation"),
                 " ", r$message %||% "No correlation available.")
      }
    })

    output$cor_plot <- renderPlotly({
      r <- cor_result()
      if (!isTRUE(r$ok)) return(plotly_empty(type = "scatter") |>
        layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
               font = list(color = "#c9d1d9")))
      C <- r$matrix
      # Heatmap with cell text (no extra package dependency)
      txt <- formatC(C, format = "f", digits = 2)
      plot_ly(
        x = colnames(C), y = colnames(C), z = C,
        type = "heatmap",
        text = txt, hovertemplate = "%{x} ↔ %{y}<br>r = %{z:.3f}<extra></extra>",
        zmin = -1, zmax = 1,
        colorscale = list(
          c(0,   "#d9534f"),
          c(0.5, "#0d1117"),
          c(1,   "#3fb950"))
      ) |>
        add_annotations(
          x = rep(colnames(C), each = nrow(C)),
          y = rep(rownames(C), times = ncol(C)),
          text = as.vector(txt),
          showarrow = FALSE,
          font = list(size = 10, color = "#c9d1d9")
        ) |>
        layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
               font = list(color = "#c9d1d9"),
               xaxis = list(tickangle = -45),
               margin = list(l = 80, b = 80))
    })

    output$cor_pairs <- DT::renderDT({
      r <- cor_result()
      if (!isTRUE(r$ok) || is.null(r$pairs) || nrow(r$pairs) == 0) {
        return(DT::datatable(
          data.frame(message = r$message %||%
                       "No correlated pairs to display."),
          options = list(dom = "t"), rownames = FALSE))
      }
      DT::datatable(r$pairs,
        options = list(pageLength = 10, dom = "tip",
                       order = list(list(3, "desc"))),
        rownames = FALSE, class = "compact stripe")
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
