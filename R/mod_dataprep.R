# =====================================================================
# Module: Editorial Data Preparation
#   Auto-profile, automated best-practice recipe, optional AI assist.
#   Produces state$prepped + state$prep_log for downstream tabs.
# =====================================================================

dataprep_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Premium hero card
    tags$div(class = "studio-shell", style = "padding:0 0 18px;",
      tags$div(class = "studio-page",
        tags$div(class = "studio-hero",
          tags$div(class = "studio-hero-meta",
            tags$span(class = "studio-kicker", "DATA PREP"),
            tags$span(class = "studio-dot", "•"),
            tags$span(class = "studio-kicker", "EDITORIAL CLEANING")
          ),
          tags$h1(class = "studio-headline", "Clean it up. On the record."),
          tags$p(class = "studio-deck",
            "Apply a reproducible recipe: drop bad columns, impute, transform, encode. ",
            "The cleaned dataset is what feeds ", tags$b("Model Lab"), ". ",
            "Want to look around first? Open ", tags$b("Explore"), "."
          ),
          tags$div(class = "studio-rule"),
          tags$div(style = "margin-top:14px; display:flex; gap:8px; flex-wrap:wrap;",
            actionButton(ns("goto_explore"),
                         tagList(HTML("← "), "Open Explore"),
                         class = "btn-outline-warning"),
            actionButton(ns("goto_modellab"),
                         tagList("Continue to Model Lab", HTML(" →")),
                         class = "btn-primary")
          )
        )
      )
    ),
    layout_columns(
    col_widths = c(4, 8),
    card(
      card_header(tagList(icon("wand-magic-sparkles"), "Auto-prepare")),
      tags$p(class = "text-muted",
             "Best-practice recipe: drop dead cols, impute, lump rare levels, ",
             "encode, transform, scale. Toggle steps below."),
      div(class = "prep-toggles",
        checkboxInput(ns("opt_drop_zero_var"),  "Drop zero-variance",   TRUE),
        checkboxInput(ns("opt_drop_id_like"),   "Drop ID-like cols",    TRUE),
        checkboxInput(ns("opt_drop_high_missing"), "Drop >60% missing", TRUE),
        checkboxInput(ns("opt_impute"),         "Impute missing",       TRUE),
        checkboxInput(ns("opt_lump_rare"),      "Lump rare cat levels", TRUE),
        checkboxInput(ns("opt_add_date_feat"),  "Expand date features", TRUE),
        checkboxInput(ns("opt_yeo_johnson"),    "Power transform skewed", TRUE),
        checkboxInput(ns("opt_winsorize"),      "Winsorize 1%/99%",     FALSE),
        checkboxInput(ns("opt_scale"),          "Standardize numerics", TRUE),
        checkboxInput(ns("opt_encode_cats"),    "One-hot encode cats",  TRUE)
      ),
      hr(),
      div(class = "ai-prep-block",
          tags$h6(tagList(icon("robot"), "AI co-pilot (optional)")),
          tags$p(class = "text-muted",
                 "Sends only the column profile (no raw rows). ",
                 "Disabled if Privacy Audit egress is OFF."),
          selectInput(ns("ai_provider"), "Provider",
                      choices = c("auto","openai","anthropic","perplexity","heuristic"),
                      selected = "auto"),
          actionButton(ns("ai_run"), "Suggest with AI",
                       class = "btn-warning w-100", icon = icon("robot"))
      ),
      hr(),
      actionButton(ns("apply"), "Apply pipeline",
                   class = "btn-primary w-100", icon = icon("play")),
      tags$small(class = "text-muted", style = "display:block; margin-top:8px;",
        "Resulting clean data is sent to Model Lab as state$prepped.")
    ),
    div(
      uiOutput(ns("editorial_report")),
      hr(),
      navset_card_underline(
        title = "Detail",
        nav_panel("Profile", DT::DTOutput(ns("profile_table"))),
        nav_panel("Leakage scan", DT::DTOutput(ns("leakage_table"))),
        nav_panel("AI suggestions", DT::DTOutput(ns("ai_table"))),
        nav_panel("Applied steps", verbatimTextOutput(ns("apply_log"))),
        nav_panel("Before / After preview",
                  fluidRow(
                    column(6, h6("Before"), DT::DTOutput(ns("before_tbl"))),
                    column(6, h6("After"),  DT::DTOutput(ns("after_tbl")))
                  ))
      )
    )
  )
  )
}

dataprep_server <- function(id, state, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Cross-links to other top-level tabs
    observeEvent(input$goto_explore, {
      if (!is.null(parent_session))
        bslib::nav_select("main_nav", "2 · Explore",
                          session = parent_session)
    })
    observeEvent(input$goto_modellab, {
      if (!is.null(parent_session))
        bslib::nav_select("main_nav", "6 · Model Lab",
                          session = parent_session)
    })

    profile <- reactive({
      req(state$raw_data)
      profile_columns(state$raw_data,
                      target = state$meta$target,
                      time_col = state$meta$time_col)
    })

    leakage <- reactive({
      req(state$raw_data, state$meta$target)
      leakage_scan(state$raw_data, state$meta$target)
    })

    output$profile_table <- DT::renderDT({
      DT::datatable(profile(),
        filter = "top",
        options = list(pageLength = 15, dom = "tip", scrollX = TRUE),
        rownames = FALSE, class = "compact stripe") |>
        DT::formatStyle("severity",
          backgroundColor = DT::styleEqual(
            c("CRITICAL","WARN","OK"),
            c("#3a1d1d","#3a311d","#1d3a26")))
    })

    output$leakage_table <- DT::renderDT({
      d <- leakage()
      if (nrow(d) == 0) d <- data.frame(message = "No leakage suspicion.")
      DT::datatable(d, options = list(dom = "t"),
                    rownames = FALSE, class = "compact stripe")
    })

    # ---- Editorial health report -----------------------------------
    output$editorial_report <- renderUI({
      p <- profile()
      n_crit <- sum(p$severity == "CRITICAL")
      n_warn <- sum(p$severity == "WARN")
      n_ok   <- sum(p$severity == "OK")
      grade <- if (n_crit > 0) "FAIL" else if (n_warn > n_ok) "WEAK" else "PASS"
      grade_col <- switch(grade, "PASS"="#3fb950","WEAK"="#d4af37","FAIL"="#d9534f")
      div(class = "studio-shell", style = "padding:12px 0 0;",
        div(class = "studio-page",
          div(class = "studio-hero",
            div(class = "studio-hero-meta",
              tags$span(class = "studio-kicker", "DATA HEALTH REPORT"),
              tags$span(class = "studio-dot", "\u2022"),
              tags$span(class = "studio-grade",
                style = sprintf("color:%s;border-color:%s;", grade_col, grade_col),
                grade)),
            tags$h1(class = "studio-headline",
                    sprintf("%d columns reviewed", nrow(p))),
            tags$p(class = "studio-deck",
              sprintf("%d critical \u00b7 %d warning \u00b7 %d healthy",
                      n_crit, n_warn, n_ok)),
            div(class = "studio-rule")),
          div(class = "studio-metric-grid",
            div(class = "studio-metric",
              div(class = "studio-metric-label", "ROWS"),
              div(class = "studio-metric-value",
                  formatC(nrow(state$raw_data), format="d", big.mark=","))),
            div(class = "studio-metric",
              div(class = "studio-metric-label", "COLUMNS"),
              div(class = "studio-metric-value", ncol(state$raw_data))),
            div(class = "studio-metric",
              div(class = "studio-metric-label", "MISSING %"),
              div(class = "studio-metric-value",
                  sprintf("%.1f", 100 * sum(is.na(state$raw_data)) /
                                    (nrow(state$raw_data) * ncol(state$raw_data))))),
            div(class = "studio-metric",
              div(class = "studio-metric-label", "GRADE"),
              div(class = "studio-metric-value",
                  style = sprintf("color:%s;", grade_col), grade))
          )
        )
      )
    })

    # ---- AI suggestions --------------------------------------------
    ai_state <- reactiveVal(list(provider = "-", suggestions = list(),
                                  notes = "Click 'Suggest with AI' to run."))

    observeEvent(input$ai_run, {
      allow <- isTRUE(state$privacy_allow_ai %||% TRUE)
      if (!allow) {
        flash("AI egress is OFF in Privacy Audit. Enable to use AI prep.", "warning")
        return()
      }
      withProgress(message = "Asking AI for prep suggestions\u2026", {
        out <- tryCatch(
          ai_prep_suggestions(profile(),
                               target = state$meta$target,
                               provider = input$ai_provider,
                               allow_ai = allow),
          error = function(e) list(provider = "error",
                                    suggestions = list(),
                                    notes = conditionMessage(e)))
        ai_state(out)
        # Log into privacy audit log if available
        if (exists("privacy_log_event"))
          tryCatch(privacy_log_event(state, action = "ai_prep_suggest",
            scope = sprintf("provider=%s; cols=%d", out$provider, nrow(profile()))),
            error = function(e) NULL)
        flash(sprintf("AI prep ready (%s)", out$provider), "message")
      })
    })

    output$ai_table <- DT::renderDT({
      s <- ai_state()$suggestions
      if (length(s) == 0)
        return(DT::datatable(data.frame(note = ai_state()$notes),
                              options = list(dom = "t"), rownames = FALSE))
      df <- do.call(rbind, lapply(s, function(r)
        data.frame(column = r$column %||% NA,
                    action = r$action %||% NA,
                    reason = r$reason %||% NA,
                    stringsAsFactors = FALSE)))
      DT::datatable(df, filter = "top",
        options = list(pageLength = 15, dom = "tip"),
        rownames = FALSE, class = "compact stripe")
    })

    # ---- Apply pipeline --------------------------------------------
    apply_log <- reactiveVal(character(0))
    after_data <- reactiveVal(NULL)

    observeEvent(input$apply, {
      req(state$raw_data, profile())
      opts <- list(
        drop_zero_var     = isTRUE(input$opt_drop_zero_var),
        drop_id_like      = isTRUE(input$opt_drop_id_like),
        drop_high_missing = isTRUE(input$opt_drop_high_missing),
        impute            = isTRUE(input$opt_impute),
        lump_rare         = isTRUE(input$opt_lump_rare),
        add_date_feat     = isTRUE(input$opt_add_date_feat),
        yeo_johnson       = isTRUE(input$opt_yeo_johnson),
        winsorize         = isTRUE(input$opt_winsorize),
        scale             = isTRUE(input$opt_scale),
        encode_cats       = isTRUE(input$opt_encode_cats)
      )
      steps <- default_steps(profile(), target = state$meta$target,
                              options = opts)
      res <- tryCatch(apply_recipe(state$raw_data, steps,
                                    target = state$meta$target),
                       error = function(e) {
                         flash(conditionMessage(e), "error"); NULL
                       })
      req(res)
      state$prepped <- res$df
      state$prep_log <- res$log
      apply_log(unlist(res$log))
      after_data(res$df)
      flash(sprintf("Pipeline applied: %d \u2192 %d cols",
                    ncol(state$raw_data), ncol(res$df)), "message")
      # Persist for audit
      tryCatch(db_save_prep_report(state$dataset_id, profile(), res$log),
               error = function(e) NULL)
    })

    output$apply_log <- renderText({
      al <- apply_log()
      if (length(al) == 0) "No pipeline applied yet." else paste(al, collapse = "\n")
    })

    output$before_tbl <- DT::renderDT({
      req(state$raw_data)
      DT::datatable(utils::head(state$raw_data, 50),
        options = list(scrollX = TRUE, pageLength = 8, dom = "tp"),
        rownames = FALSE, class = "compact stripe")
    })

    output$after_tbl <- DT::renderDT({
      d <- after_data()
      if (is.null(d)) return(DT::datatable(
        data.frame(message = "Apply pipeline to see preview"),
        options = list(dom = "t"), rownames = FALSE))
      DT::datatable(utils::head(d, 50),
        options = list(scrollX = TRUE, pageLength = 8, dom = "tp"),
        rownames = FALSE, class = "compact stripe")
    })
  })
}
