# =====================================================================
# Module 6: Editorial Graphic Studio
#   Magazine-style presentation of the latest run: hero card,
#   prediction strip, editorial metric grid, exportable as PNG/HTML.
# =====================================================================

studio_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "studio-shell",
      # ----- Top control bar (compact) -----
      div(class = "studio-toolbar",
          div(class = "studio-toolbar-left",
              tags$span(class = "studio-issue", textOutput(ns("issue_label"), inline = TRUE))
          ),
          div(class = "studio-toolbar-right",
              selectInput(ns("layout"), NULL,
                          choices = c("Cover" = "cover",
                                      "Spread" = "spread",
                                      "Minimal" = "minimal"),
                          selected = "cover", width = "140px"),
              actionButton(ns("refresh"), "Refresh",
                           class = "btn btn-outline-light btn-sm",
                           icon = icon("rotate")),
              downloadButton(ns("export_html"), "Export HTML",
                             class = "btn btn-outline-warning btn-sm"),
              downloadButton(ns("export_csv"), "Forecast CSV",
                             class = "btn btn-outline-success btn-sm")
          )
      ),

      # ----- Editorial canvas -----
      uiOutput(ns("editorial"))
    )
  )
}

studio_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Issue label (top of magazine)
    output$issue_label <- renderText({
      sprintf("ISSUE %s  \u2022  THE FORECAST",
              format(Sys.Date(), "%Y \u2014 %m"))
    })

    # Reactive that pulls everything we need about the most recent run
    studio_payload <- reactive({
      input$refresh # invalidate on click
      req(state$last_run_id)
      run_id <- state$last_run_id
      preds  <- tryCatch(db_get_predictions(run_id), error = function(e) data.frame())
      runs   <- tryCatch(db_get_runs(),              error = function(e) data.frame())
      run_row <- if (nrow(runs) > 0) runs[runs$id == run_id, , drop = FALSE] else data.frame()
      analysis <- tryCatch(db_get_analysis(run_id),  error = function(e) NULL)
      metrics <- tryCatch(jsonlite::fromJSON(run_row$metrics_json[1]),
                          error = function(e) list())
      params  <- tryCatch(jsonlite::fromJSON(run_row$params_json[1]),
                          error = function(e) list())
      list(
        run_id   = run_id,
        run_row  = run_row,
        preds    = preds,
        metrics  = metrics,
        params   = params,
        analysis = analysis,
        meta     = state$meta
      )
    })

    # ---- Helpers -----------------------------------------------------
    fmt_num <- function(x, d = 3) {
      if (is.null(x) || length(x) == 0 || is.na(x)) return("\u2014")
      if (is.numeric(x)) formatC(x, format = "f", digits = d, big.mark = ",") else as.character(x)
    }
    grade_color <- function(g) switch(toupper(g %||% "WEAK"),
                                       "PASS" = "#3fb950",
                                       "WEAK" = "#d4af37",
                                       "FAIL" = "#d9534f",
                                       "#8b949e")
    horizon_text <- function(meta, run_row, n) {
      if (!is.null(meta) && isTRUE(meta$task_type == "time_series")) {
        h <- run_row$horizon[1]
        sprintf("%s-step horizon \u2022 %s", fmt_num(h, 0), meta$frequency %||% "ts")
      } else {
        sprintf("%d holdout observations", n)
      }
    }

    # ---- Hero forecast plotly ---------------------------------------
    studio_plot <- function(payload, height = 360) {
      d <- payload$preds
      if (is.null(d) || nrow(d) == 0) return(NULL)
      meta <- payload$meta
      is_ts <- !is.null(meta) && isTRUE(meta$task_type == "time_series")
      has_pi <- "lower" %in% names(d) && !all(is.na(d$lower))
      x <- seq_len(nrow(d))
      p <- plot_ly(height = height) |>
        plotly::add_lines(x = x, y = d$predicted, name = "Forecast",
                          line = list(color = "#d4af37", width = 3))
      if (has_pi) {
        p <- p |> plotly::add_ribbons(x = x, ymin = d$lower, ymax = d$upper,
                                       name = "PI",
                                       fillcolor = "rgba(212,175,55,0.18)",
                                       line = list(color = "transparent"))
      }
      if ("actual" %in% names(d) && !all(is.na(d$actual))) {
        p <- p |> plotly::add_lines(x = x, y = d$actual, name = "Actual",
                                     line = list(color = "#c9d1d9",
                                                 width = 2, dash = "dot"))
      }
      p |> plotly::layout(
        paper_bgcolor = "#0d0d0f",
        plot_bgcolor  = "#0d0d0f",
        font = list(color = "#e8e6e0", family = "Inter"),
        margin = list(l = 20, r = 20, t = 10, b = 30),
        showlegend = FALSE,
        xaxis = list(showgrid = FALSE, zeroline = FALSE,
                     showline = FALSE, title = ""),
        yaxis = list(gridcolor = "#1c1c20", zeroline = FALSE,
                     showline = FALSE, title = "")
      )
    }

    # ---- Editorial UI -----------------------------------------------
    output$editorial <- renderUI({
      payload <- studio_payload()

      if (is.null(payload$run_id) || nrow(payload$preds) == 0) {
        return(div(class = "studio-empty",
                   tags$h1("The Forecast"),
                   tags$p(class = "studio-empty-sub",
                          "Train a model in Model Lab \u2014 the editorial issue prints itself.")))
      }

      run_row  <- payload$run_row
      metrics  <- payload$metrics
      analysis <- payload$analysis
      meta     <- payload$meta
      n        <- nrow(payload$preds)
      model_id <- run_row$model_id[1]
      task     <- run_row$task_type[1] %||% (meta$task_type %||% "")
      grade    <- analysis$grade %||% "\u2014"

      # Headline: prefer AI verdict; fall back to model+task
      headline <- if (!is.null(analysis) && nzchar(analysis$verdict %||% "")) {
        analysis$verdict
      } else {
        sprintf("%s on %s", model_id, task)
      }

      # Editorial metric tiles (top 3 numeric metrics)
      m_keys <- intersect(c("RMSE","MAE","R2","MAPE","Accuracy","F1","LogLoss","Deviance"),
                          names(metrics))
      if (length(m_keys) == 0) m_keys <- head(names(metrics), 3)
      m_keys <- head(m_keys, 3)

      metric_tiles <- lapply(m_keys, function(k) {
        div(class = "studio-metric",
            div(class = "studio-metric-label", toupper(k)),
            div(class = "studio-metric-value", fmt_num(metrics[[k]], 4)))
      })

      caption <- sprintf("%s issued on %s. Model %s. %s.",
                         payload$meta$target %||% "Target",
                         format(Sys.time(), "%B %d, %Y"),
                         model_id,
                         horizon_text(meta, run_row, n))

      pull_quote <- if (!is.null(analysis) && nzchar(analysis$analysis_md %||% "")) {
        # Use first non-empty line as a pull-quote
        lines <- strsplit(analysis$analysis_md, "\n", fixed = TRUE)[[1]]
        first <- head(Filter(function(x) nchar(trimws(x)) > 0, lines), 1)
        if (length(first) == 0) "" else gsub("^[#>*\\- ]+", "", first[[1]])
      } else ""

      # ----- Layouts -----
      layout_kind <- input$layout %||% "cover"

      hero <- div(class = "studio-hero",
        div(class = "studio-hero-meta",
            tags$span(class = "studio-kicker", toupper(task)),
            tags$span(class = "studio-dot", "\u2022"),
            tags$span(class = "studio-kicker", toupper(model_id)),
            tags$span(class = "studio-dot", "\u2022"),
            tags$span(class = "studio-grade",
                      style = sprintf("color:%s;border-color:%s;",
                                      grade_color(grade), grade_color(grade)),
                      grade)),
        tags$h1(class = "studio-headline", headline),
        tags$p(class = "studio-deck", caption),
        div(class = "studio-rule")
      )

      # Render plotly via output binding so it's interactive
      output$hero_plot <- plotly::renderPlotly({ studio_plot(payload, height = 360) })

      pull <- if (nzchar(pull_quote))
        tags$blockquote(class = "studio-pull", pull_quote) else NULL

      strip <- div(class = "studio-metric-grid", metric_tiles)

      footer <- div(class = "studio-footer",
        tags$span(class = "studio-byline", "Author \u2014 Malik Hebbat"),
        tags$span(class = "studio-byline-sep", "|"),
        tags$span(class = "studio-byline", sprintf("Run #%s", payload$run_id)),
        tags$span(class = "studio-byline-sep", "|"),
        tags$span(class = "studio-byline",
                  sprintf("AI: %s", analysis$provider %||% "\u2014"))
      )

      switch(layout_kind,
        "cover" = div(class = "studio-page studio-cover",
                      hero,
                      div(class = "studio-cover-grid",
                          div(class = "studio-cover-plot",
                              plotly::plotlyOutput(ns("hero_plot"), height = "360px")),
                          div(class = "studio-cover-side",
                              strip,
                              pull)),
                      footer),
        "spread" = div(class = "studio-page studio-spread",
                       hero,
                       plotly::plotlyOutput(ns("hero_plot"), height = "460px"),
                       strip,
                       pull,
                       footer),
        "minimal" = div(class = "studio-page studio-minimal",
                        tags$h2(class = "studio-min-title", model_id),
                        tags$h1(class = "studio-headline", headline),
                        plotly::plotlyOutput(ns("hero_plot"), height = "320px"),
                        strip,
                        footer)
      )
    })

    # ---- Exports -----------------------------------------------------
    output$export_csv <- downloadHandler(
      filename = function()
        sprintf("studio_run%s_%s.csv",
                state$last_run_id %||% "0",
                format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        d <- tryCatch(db_get_predictions(state$last_run_id),
                       error = function(e) data.frame())
        if (is.null(d) || nrow(d) == 0)
          d <- data.frame(message = "No predictions available")
        utils::write.csv(d, file, row.names = FALSE)
      }
    )

    output$export_html <- downloadHandler(
      filename = function()
        sprintf("studio_run%s_%s.html",
                state$last_run_id %||% "0",
                format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        payload <- studio_payload()
        # Build a self-contained HTML keepsake
        css_path <- file.path(.app_root(), "www", "studio.css")
        css <- if (file.exists(css_path)) readLines(css_path, warn = FALSE) else ""
        css <- paste(css, collapse = "\n")
        run_row  <- payload$run_row
        analysis <- payload$analysis
        metrics  <- payload$metrics
        meta     <- payload$meta
        m_keys <- head(intersect(c("RMSE","MAE","R2","MAPE","Accuracy","F1"),
                                  names(metrics)), 3)
        if (length(m_keys) == 0) m_keys <- head(names(metrics), 3)
        metric_html <- paste(vapply(m_keys, function(k) sprintf(
          '<div class="studio-metric"><div class="studio-metric-label">%s</div><div class="studio-metric-value">%s</div></div>',
          toupper(k), fmt_num(metrics[[k]], 4)), character(1)), collapse = "")
        headline <- if (!is.null(analysis) && nzchar(analysis$verdict %||% ""))
          analysis$verdict else sprintf("%s on %s", run_row$model_id[1],
                                        run_row$task_type[1] %||% "")
        body_html <- sprintf('
<!DOCTYPE html><html><head><meta charset="utf-8">
<title>The Forecast \u2014 Run %s</title>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Playfair+Display:wght@500;700;900&family=Inter:wght@300;400;500;600&display=swap">
<style>%s body{background:#0d0d0f;}</style>
</head><body><div class="studio-shell"><div class="studio-page studio-cover">
<div class="studio-hero">
  <div class="studio-hero-meta">
    <span class="studio-kicker">%s</span><span class="studio-dot">\u2022</span>
    <span class="studio-kicker">%s</span>
  </div>
  <h1 class="studio-headline">%s</h1>
  <p class="studio-deck">%s issued on %s. Model %s.</p>
  <div class="studio-rule"></div>
</div>
<div class="studio-metric-grid">%s</div>
<div class="studio-footer">
  <span class="studio-byline">Author \u2014 Malik Hebbat</span>
  <span class="studio-byline-sep">|</span>
  <span class="studio-byline">Run #%s</span>
</div>
</div></div></body></html>',
          payload$run_id, css,
          toupper(run_row$task_type[1] %||% ""),
          toupper(run_row$model_id[1] %||% ""),
          htmltools::htmlEscape(headline),
          htmltools::htmlEscape(meta$target %||% "Target"),
          format(Sys.time(), "%B %d, %Y"),
          htmltools::htmlEscape(run_row$model_id[1] %||% ""),
          metric_html,
          payload$run_id)
        writeLines(body_html, file)
      }
    )
  })
}
