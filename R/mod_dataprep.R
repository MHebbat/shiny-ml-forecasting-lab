# =====================================================================
# Module: Data Preparation
#
# Builds a reproducible recipe of preprocessing steps and applies it to
# the active dataset before it is passed to Model Lab. UI features:
#
#   * default best-practice toggles (auto-prepare)
#   * full transform vocabulary (log / boxcox / sqrt / inverse / scaling
#     family / quantile-normal / clip / bins / target / freq / ordinal
#     encode / PCA / date features / parse_datetime, etc.)
#   * per-step variable scope (named selectors or specific columns)
#   * top-level scope toggle: apply to all variables vs. chosen variables
#   * time-column protection — numeric transforms never touch the
#     declared time column; only date_features / parse_datetime may
#   * AI co-pilot suggestions are full recipe steps; one-click "Apply"
#     and "Apply all".
# =====================================================================

dataprep_ui <- function(id) {
  ns <- NS(id)
  step_choices <- setNames(names(RECIPE_STEP_VOCAB),
                           vapply(names(RECIPE_STEP_VOCAB), function(k)
                             sprintf("%s — %s", k,
                                       RECIPE_STEP_VOCAB[[k]]$label),
                             character(1)))
  tagList(
    # Compact hero card (scientific register)
    tags$div(class = "studio-intro",
      tags$div(class = "studio-page",
        tags$div(class = "studio-hero",
          tags$div(class = "studio-hero-meta",
            tags$span(class = "studio-kicker", "DATA PREPARATION"),
            tags$span(class = "studio-dot", "•"),
            tags$span(class = "studio-kicker", "REPRODUCIBLE RECIPE")
          ),
          tags$h1(class = "studio-headline",
                  "Feature engineering applied prior to estimation."),
          tags$p(class = "studio-deck",
            "Each step is recorded as a JSON-serialisable recipe entry. ",
            "Numeric transforms never touch the declared time column; only ",
            tags$b("date_features"), " and ", tags$b("parse_datetime"),
            " do. The transformed frame is the input to ",
            tags$b("Model Lab"), "."
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

    # ---- Top scope control ------------------------------------------
    card(
      card_header(tagList(icon("filter"), "Recipe scope")),
      tags$small(class = "text-muted",
        "Restrict the recipe to all variables, or to a chosen subset. ",
        "Steps with explicit column targets always override this scope."),
      radioButtons(ns("scope_mode"), NULL, inline = TRUE,
                    choices = c("All variables" = "all",
                                "Chosen variables" = "chosen"),
                    selected = "all"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'chosen'", ns("scope_mode")),
        selectInput(ns("scope_cols"), "Variables in scope",
                    choices = NULL, multiple = TRUE,
                    width = "100%")
      )
    ),

    layout_columns(
    col_widths = c(4, 8),
    tags$div(
      card(
        card_header(tagList(icon("wand-magic-sparkles"), "Auto-prepare")),
        tags$p(class = "text-muted",
               "Best-practice defaults: drop dead cols, impute, lump rare ",
               "levels, derive date features, transform skewed numerics, ",
               "scale, encode categoricals."),
        div(class = "prep-toggles",
          checkboxInput(ns("opt_drop_zero_var"),  "Drop zero-variance",   TRUE),
          checkboxInput(ns("opt_drop_id_like"),   "Drop ID-like cols",    TRUE),
          checkboxInput(ns("opt_drop_high_missing"), "Drop >60% missing", TRUE),
          checkboxInput(ns("opt_impute"),         "Impute missing",       TRUE),
          checkboxInput(ns("opt_lump_rare"),      "Lump rare cat levels", TRUE),
          checkboxInput(ns("opt_add_date_feat"),  "Expand date features", TRUE),
          checkboxInput(ns("opt_yeo_johnson"),    "Power transform skewed", TRUE),
          checkboxInput(ns("opt_winsorize"),      "Clip 1%/99%",          FALSE),
          checkboxInput(ns("opt_scale"),          "Standardize numerics", TRUE),
          checkboxInput(ns("opt_encode_cats"),    "One-hot encode cats",  TRUE)
        )
      ),
      # ---- Add custom transform step ---------------------------------
      card(
        card_header(tagList(icon("plus"), "Add a transform")),
        tags$small(class = "text-muted",
          "Adds a step to the active recipe. Picks a transform, scope, ",
          "and (where relevant) parameters."),
        selectInput(ns("custom_step_id"), "Transform",
                    choices = step_choices,
                    selected = "log"),
        uiOutput(ns("custom_step_desc")),
        selectInput(ns("custom_step_scope"), "Apply to",
                    choices = c("All numeric" = "all_numeric",
                                "All categorical" = "all_categorical",
                                "All predictors" = "all_predictors",
                                "All columns" = "all",
                                "Chosen columns" = "__chosen"),
                    selected = "all_numeric"),
        conditionalPanel(
          condition = sprintf("input['%s'] == '__chosen'",
                              ns("custom_step_scope")),
          selectInput(ns("custom_step_cols"), "Columns",
                      choices = NULL, multiple = TRUE)
        ),
        uiOutput(ns("custom_step_params")),
        actionButton(ns("custom_step_add"), "Add to recipe",
                     class = "btn-outline-primary w-100",
                     icon = icon("plus"))
      ),
      # ---- AI co-pilot ----------------------------------------------
      card(
        card_header(tagList(icon("robot"),
                              "AI co-pilot (column profile only)")),
        tags$small(class = "text-muted",
          "Sends only the column profile (no raw rows). Disabled if ",
          "Privacy Audit egress is OFF."),
        selectInput(ns("ai_provider"), "Provider",
                    choices = c("auto","openai","anthropic",
                                  "perplexity","heuristic"),
                    selected = "auto"),
        actionButton(ns("ai_run"), "Suggest with AI",
                     class = "btn-warning w-100", icon = icon("robot"))
      ),
      # ---- Apply ----------------------------------------------------
      card(
        card_header(tagList(icon("play"), "Apply pipeline")),
        actionButton(ns("apply"), "Apply pipeline",
                     class = "btn-primary w-100", icon = icon("play")),
        tags$small(class = "text-muted", style = "display:block; margin-top:8px;",
          "The transformed frame becomes state$prepped, the model-ready input.")
      )
    ),
    div(
      uiOutput(ns("editorial_report")),
      hr(),
      navset_card_underline(
        title = "Detail",
        nav_panel("Active recipe",
                  tags$small(class = "text-muted", style = "display:block; margin:6px 0;",
                    "These are the steps that will run when you click 'Apply pipeline'. ",
                    "Custom steps and applied AI suggestions accumulate here."),
                  uiOutput(ns("recipe_steps"))),
        nav_panel("Profile", DT::DTOutput(ns("profile_table"))),
        nav_panel("Leakage scan", DT::DTOutput(ns("leakage_table"))),
        nav_panel("AI suggestions", uiOutput(ns("ai_suggestions_ui"))),
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

    # --- Active recipe (custom steps + AI-applied steps) -------------
    custom_steps <- reactiveVal(list())

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

    # Keep scope-column choices in sync with the data, excluding the time col.
    observe({
      df <- state$raw_data
      if (is.null(df)) return()
      tcol <- state$meta$time_col
      cols <- setdiff(names(df), tcol)
      updateSelectInput(session, "scope_cols",
                        choices = cols, selected = cols)
      updateSelectInput(session, "custom_step_cols", choices = cols)
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
      div(class = "studio-intro", style = "margin-top:12px;",
        div(class = "studio-page",
          div(class = "studio-hero",
            div(class = "studio-hero-meta",
              tags$span(class = "studio-kicker", "DATA HEALTH REPORT"),
              tags$span(class = "studio-dot", "•"),
              tags$span(class = "studio-grade",
                style = sprintf("color:%s;border-color:%s;", grade_col, grade_col),
                grade)),
            tags$h1(class = "studio-headline",
                    sprintf("%d columns reviewed", nrow(p))),
            tags$p(class = "studio-deck",
              sprintf("%d critical · %d warning · %d healthy",
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

    # ---- Custom step UI --------------------------------------------
    output$custom_step_desc <- renderUI({
      sid <- input$custom_step_id
      spec <- RECIPE_STEP_VOCAB[[sid]]
      if (is.null(spec)) return(NULL)
      tags$small(class = "text-muted", style = "display:block; margin:-6px 0 8px;",
                 spec$desc)
    })

    output$custom_step_params <- renderUI({
      sid <- input$custom_step_id
      spec <- RECIPE_STEP_VOCAB[[sid]]
      if (is.null(spec) || length(spec$params %||% list()) == 0) return(NULL)
      controls <- lapply(names(spec$params), function(pname) {
        val <- spec$params[[pname]]
        cid <- ns(paste0("p_", sid, "_", pname))
        if (is.numeric(val) || is.na(val))
          numericInput(cid, pname,
                        value = if (is.na(val)) NA else val)
        else if (is.logical(val))
          checkboxInput(cid, pname, isTRUE(val))
        else
          textInput(cid, pname, value = as.character(val))
      })
      tagList(tags$h6("Parameters", style = "margin-top:6px;"), controls)
    })

    observeEvent(input$custom_step_add, {
      sid <- input$custom_step_id
      spec <- RECIPE_STEP_VOCAB[[sid]]
      req(spec)
      scope <- input$custom_step_scope
      tgts <- if (scope == "__chosen")
        as.character(input$custom_step_cols %||% character(0))
      else scope
      if (scope == "__chosen" && length(tgts) == 0) {
        flash("Pick at least one column for the chosen scope.", "warning")
        return()
      }
      params <- list()
      for (pname in names(spec$params %||% list())) {
        cid <- paste0("p_", sid, "_", pname)
        v <- input[[cid]]
        if (!is.null(v)) params[[pname]] <- v
      }
      step <- recipe_step(sid, targets = tgts, params = params)
      cs <- custom_steps()
      cs[[length(cs) + 1]] <- step
      custom_steps(cs)
      flash(sprintf("Added '%s' to the active recipe.", spec$label), "message")
    })

    output$recipe_steps <- renderUI({
      cs <- custom_steps()
      if (length(cs) == 0)
        return(tags$div(class = "text-muted",
          "No custom steps yet. Auto-prepare toggles and AI suggestions can ",
          "supply steps; you can also add them manually."))
      tags$ul(class = "list-unstyled", style = "margin: 6px 0;",
        lapply(seq_along(cs), function(i) {
          s <- cs[[i]]
          tgt_disp <- if (length(s$targets) == 1 && is.character(s$targets) &&
                          s$targets %in% c("all","all_numeric",
                                            "all_categorical","all_predictors"))
            s$targets else paste(s$targets, collapse = ", ")
          tags$li(style = "padding:6px 0; border-bottom:1px solid #30363d;",
            tags$div(style = "display:flex; justify-content:space-between; gap:8px;",
              tags$span(tags$b(s$label %||% s$id),
                         tags$small(class = "text-muted",
                                     sprintf(" · scope: %s", tgt_disp))),
              actionButton(ns(paste0("rm_step_", i)), "Remove",
                            class = "btn-outline-danger btn-sm")))
        })
      )
    })

    observe({
      cs <- custom_steps()
      lapply(seq_along(cs), function(i) {
        local({
          ii <- i
          observeEvent(input[[paste0("rm_step_", ii)]], {
            cur <- custom_steps()
            if (ii <= length(cur)) {
              cur <- cur[-ii]; custom_steps(cur)
            }
          }, ignoreInit = TRUE, once = TRUE)
        })
      })
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
      withProgress(message = "Asking AI for prep suggestions…", {
        out <- tryCatch(
          ai_prep_suggestions(profile(),
                               target = state$meta$target,
                               provider = input$ai_provider,
                               allow_ai = allow),
          error = function(e) list(provider = "error",
                                    suggestions = list(),
                                    notes = conditionMessage(e)))
        ai_state(out)
        if (exists("privacy_log_event"))
          tryCatch(privacy_log_event(state, action = "ai_prep_suggest",
            scope = sprintf("provider=%s; cols=%d", out$provider, nrow(profile()))),
            error = function(e) NULL)
        flash(sprintf("AI prep ready (%s)", out$provider), "message")
      })
    })

    output$ai_suggestions_ui <- renderUI({
      st <- ai_state()
      s <- st$suggestions
      if (length(s) == 0)
        return(tags$div(class = "alert alert-secondary",
          st$notes %||% "No suggestions yet."))
      tagList(
        tags$div(style = "margin-bottom:10px;",
          tags$small(class = "text-muted",
            sprintf("Provider: %s. ", st$provider)),
          actionButton(ns("ai_apply_all"), "Apply all",
                        class = "btn-primary btn-sm",
                        icon = icon("circle-plus"))),
        tags$ul(class = "list-unstyled",
          lapply(seq_along(s), function(i) {
            it <- s[[i]]
            tags$li(style = "padding:8px 0; border-bottom:1px solid #30363d;",
              tags$div(style = "display:flex; justify-content:space-between; gap:8px;",
                tags$span(tags$b(it$action),
                            tags$small(class = "text-muted",
                                        sprintf(" · %s · %s",
                                          paste(it$column, collapse = ", "),
                                          it$reason %||% ""))),
                actionButton(ns(paste0("ai_apply_", i)), "Apply",
                              class = "btn-outline-primary btn-sm")))
          })))
    })

    observeEvent(input$ai_apply_all, {
      s <- ai_state()$suggestions
      if (length(s) == 0) return()
      cs <- custom_steps()
      for (it in s) cs[[length(cs)+1]] <- it$step
      custom_steps(cs)
      flash(sprintf("Applied %d AI suggestions to the recipe.", length(s)),
            "message")
    })
    observe({
      s <- ai_state()$suggestions
      lapply(seq_along(s), function(i) {
        local({
          ii <- i
          observeEvent(input[[paste0("ai_apply_", ii)]], {
            cur <- custom_steps()
            cur[[length(cur)+1]] <- ai_state()$suggestions[[ii]]$step
            custom_steps(cur)
            flash("Suggestion added to recipe.", "message")
          }, ignoreInit = TRUE, once = TRUE)
        })
      })
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
      auto_steps <- default_steps(profile(), target = state$meta$target,
                                   options = opts)
      steps <- c(auto_steps, custom_steps())
      scope_columns <- if (isTRUE(input$scope_mode == "chosen"))
        as.character(input$scope_cols %||% character(0)) else NULL
      res <- tryCatch(apply_recipe(state$raw_data, steps,
                                    target = state$meta$target,
                                    time_col = state$meta$time_col,
                                    scope_columns = scope_columns),
                       error = function(e) {
                         flash(conditionMessage(e), "error"); NULL
                       })
      req(res)
      state$prepped <- res$df
      state$prep_log <- res$log
      apply_log(unlist(res$log))
      after_data(res$df)
      flash(sprintf("Pipeline applied: %d → %d cols",
                    ncol(state$raw_data), ncol(res$df)), "message")
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
