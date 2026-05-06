# =====================================================================
# Module: Training Studio (was Model Lab)
#   Rich, configurable, well-explained training experience built on top
#   of the existing model registry. Editorial styling (studio-intro) for
#   the hero card; grouped hyperparameters with helper text; explicit
#   training-configuration panel; deep-learning specific block; live
#   training summary + diagnostics; collapsible "About this model"
#   documentation drawer.
#
# UI <-> server output IDs are kept 1:1. Every new control is consumed
# by collect_params() / collect_train_config() / collect_dl_config().
# =====================================================================

modellab_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # ----- Editorial hero (compact intro variant — does NOT take 100vh) -----
    tags$div(class = "studio-intro",
      tags$div(class = "studio-page",
        tags$div(class = "studio-hero",
          tags$div(class = "studio-hero-meta",
            tags$span(class = "studio-kicker", "MODEL LAB"),
            tags$span(class = "studio-dot", "•"),
            tags$span(class = "studio-kicker", "TRAINING STUDIO")
          ),
          tags$h1(class = "studio-headline",
                  tagList("Model Estimation.",
                            doc_chip("modellab", "Model Lab"))),
          tags$p(class = "studio-deck",
            "Specification, hyperparameter configuration, validation strategy, ",
            "and reproducibility controls for every model in the registry. ",
            "Every model carries a methodological description, a ",
            tags$b("best-for"), " / ", tags$b("avoid-when"),
            " card, and a deep-dive documentation drawer. Hyperparameters ",
            "are grouped by role; the resolved JSON is shown before training."
          ),
          tags$div(class = "studio-rule"),
          tags$div(style = "margin-top:10px; display:flex; gap:8px; flex-wrap:wrap;",
            actionButton(ns("save_project"), "Save Project",
                          class = "btn-outline-warning btn-sm",
                          icon = icon("floppy-disk")),
            downloadButton(ns("export_brief_html"), "Brief Report (HTML)",
                            class = "btn-outline-info btn-sm"),
            downloadButton(ns("export_brief_pdf"), "Brief Report (PDF)",
                            class = "btn-outline-info btn-sm"),
            actionButton(ns("show_manifest"), "Show Manifest",
                          class = "btn-outline-secondary btn-sm",
                          icon = icon("file-code"))
          )
        )
      )
    ),

    layout_columns(
      col_widths = c(4, 8),

      # ===== Left: configuration =========================================
      tags$div(
        # ---- Model selector + meta ------------------------------------
        card(
          card_header(tagList(icon("microscope"), "Model")),
          conditionalPanel(
            condition = sprintf("output['%s']", ns("ready")),
            uiOutput(ns("task_badge")),
            selectInput(ns("model_id"), "Model", choices = NULL),
            uiOutput(ns("model_engine")),
            uiOutput(ns("model_meta_card")),
            uiOutput(ns("availability_card")),
            tags$div(style = "margin-top:8px;",
              actionButton(ns("toggle_doc"),
                tagList(icon("book-open"), " About this model"),
                class = "btn-outline-warning btn-sm")
            ),
            conditionalPanel(
              condition = sprintf("output['%s']", ns("doc_open")),
              uiOutput(ns("doc_drawer"))
            )
          ),
          conditionalPanel(
            condition = sprintf("!output['%s']", ns("ready")),
            tags$div(class = "alert alert-warning",
                     "Save a dataset (Ingest) and apply a pipeline (Explore / Data Prep) first.")
          )
        ),

        # ---- Hyperparameter panel (grouped + presets) ------------------
        conditionalPanel(
          condition = sprintf("output['%s']", ns("ready")),
          card(
            card_header(tagList(icon("sliders"), "Hyperparameters")),
            tags$div(class = "studio-kicker", style = "margin-bottom:6px;",
                     "PRESETS"),
            tags$div(style = "display:flex; gap:6px; flex-wrap:wrap; margin-bottom:10px;",
              actionButton(ns("preset_default"),
                tagList(icon("circle-dot"), " Suggested defaults"),
                class = "btn-outline-secondary btn-sm"),
              actionButton(ns("preset_aggressive"),
                tagList(icon("fire-flame-curved"), " Aggressive"),
                class = "btn-outline-warning btn-sm"),
              actionButton(ns("preset_conservative"),
                tagList(icon("shield-halved"), " Conservative"),
                class = "btn-outline-info btn-sm")
            ),
            tags$hr(class = "studio-rule",
                    style = "margin-top:0; margin-bottom:10px;"),
            uiOutput(ns("param_ui"))
          )
        ),

        # ---- Deep-learning block (only when applicable) ----------------
        conditionalPanel(
          condition = sprintf("output['%s']", ns("is_dl")),
          card(
            card_header(tagList(icon("brain"), "Deep-learning configuration")),
            uiOutput(ns("dl_block_ui"))
          )
        ),

        # ---- Training configuration -----------------------------------
        conditionalPanel(
          condition = sprintf("output['%s']", ns("ready")),
          card(
            card_header(tagList(icon("gears"), "Training configuration")),
            tags$div(class = "studio-kicker", style = "margin-bottom:6px;",
                     "VALIDATION STRATEGY"),
            radioButtons(ns("val_scheme"), NULL,
              choices = c("Holdout (train / test split)" = "holdout",
                          "k-fold cross-validation"      = "kfold",
                          "Repeated k-fold CV"           = "repeated_cv",
                          "Rolling-origin CV (time series)" = "rolling"),
              selected = "holdout"),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'holdout'", ns("val_scheme")),
              sliderInput(ns("holdout_pct"), "Train split (%)",
                          min = 50, max = 95, value = 80, step = 1)),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'kfold'", ns("val_scheme")),
              numericInput(ns("kfolds"), "Folds (k)",
                           value = 5, min = 2, max = 20)),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'repeated_cv'", ns("val_scheme")),
              numericInput(ns("kfolds_rep"), "Folds (k)",
                           value = 5, min = 2, max = 20),
              numericInput(ns("cv_repeats"), "Repeats",
                           value = 3, min = 1, max = 20)),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'rolling'", ns("val_scheme")),
              numericInput(ns("ro_initial"), "Initial window",
                           value = 50, min = 10),
              numericInput(ns("ro_h"), "Forecast horizon (h)",
                           value = 12, min = 1),
              numericInput(ns("ro_step"), "Step",
                           value = 1, min = 1)),

            tags$hr(class = "studio-rule"),
            tags$div(class = "studio-kicker", style = "margin-bottom:6px;",
                     "RESAMPLING"),
            numericInput(ns("bootstrap_iters"), "Bootstrap iterations",
                         value = 0, min = 0, max = 1000),
            tags$small(class = "text-muted",
                       "0 = no bootstrap. Otherwise re-train this many times on ",
                       "resampled data and average metrics."),

            tags$hr(class = "studio-rule"),
            tags$div(class = "studio-kicker", style = "margin-bottom:6px;",
                     "CLASS WEIGHTING"),
            radioButtons(ns("class_weighting"), NULL,
              choices = c("None"                 = "none",
                          "Inverse frequency"    = "inverse",
                          "SMOTE (requires themis)" = "smote"),
              selected = "none", inline = TRUE),
            uiOutput(ns("class_weight_note")),

            tags$hr(class = "studio-rule"),
            tags$div(class = "studio-kicker", style = "margin-bottom:6px;",
                     "STOPPING & RUNTIME"),
            numericInput(ns("early_stop_general"),
                         "Early stopping rounds (where applicable)",
                         value = 0, min = 0, max = 500),
            tags$small(class = "text-muted",
                       "Used by xgboost / LightGBM / Keras when supported. ",
                       "Per-model fields take precedence."),
            numericInput(ns("seed"), "Random seed",
                         value = 42, min = 0, max = .Machine$integer.max),
            checkboxInput(ns("seed_lock"), "Lock seed (reproducible runs)",
                          value = TRUE),
            checkboxInput(ns("parallel"),
                          "Parallel backend (future::plan(multisession))",
                          value = FALSE),
            uiOutput(ns("parallel_note")),
            numericInput(ns("compute_budget_min"),
                         "Compute budget (minutes, 0 = no cap)",
                         value = 0, min = 0, max = 600, step = 0.5),

            tags$hr(class = "studio-rule"),

            actionButton(ns("train"),
                         tagList(icon("play"), " Train & Evaluate"),
                         class = "btn-primary w-100"),

            tags$div(style = "margin-top:10px;",
              tags$div(class = "studio-kicker", "RESOLVED RUN PARAMETERS"),
              verbatimTextOutput(ns("summary_json"))
            )
          )
        )
      ),

      # ===== Right: results ==============================================
      navset_card_underline(
        title = "Results",
        nav_panel("Metrics",
          withSpinner(verbatimTextOutput(ns("metrics_box")))),
        nav_panel("Predictions",
          withSpinner(plotlyOutput(ns("pred_plot"), height = "400px"))),
        nav_panel("Residuals",
          withSpinner(plotlyOutput(ns("resid_plot"), height = "400px"))),
        nav_panel("Feature importance",
          withSpinner(plotlyOutput(ns("imp_plot"), height = "400px"))),
        nav_panel("Diagnostics",
          uiOutput(ns("diagnostics_card"))),
        nav_panel("Console log",
          withSpinner(verbatimTextOutput(ns("log_box"))))
      )
    )
  )
}

# ---------------------------------------------------------------------------

modellab_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Logging ---------------------------------------------------------
    log_msgs <- reactiveVal(character(0))
    add_log <- function(...) log_msgs(c(log_msgs(),
      paste0("[", format(Sys.time(), "%H:%M:%S"), "] ",
             paste0(..., collapse = ""))))

    # ---- Documentation drawer state -------------------------------------
    doc_open <- reactiveVal(FALSE)
    output$doc_open <- reactive(isTRUE(doc_open()))
    outputOptions(output, "doc_open", suspendWhenHidden = FALSE)

    observeEvent(input$toggle_doc, {
      doc_open(!isTRUE(doc_open()))
    })
    # Close drawer when model changes so the user is not surprised
    observeEvent(input$model_id, {
      doc_open(FALSE)
    }, ignoreInit = TRUE)

    # ---- Ready / task helpers -------------------------------------------
    output$ready <- reactive({
      !is.null(state$prepped) && !is.null(state$meta)
    })
    outputOptions(output, "ready", suspendWhenHidden = FALSE)

    output$is_dl <- reactive({
      mid <- input$model_id
      !is.null(mid) && mid %in% c("lstm","mlp_reg","mlp_cls")
    })
    outputOptions(output, "is_dl", suspendWhenHidden = FALSE)

    output$task_badge <- renderUI({
      req(state$meta)
      tags$div(class = "alert alert-info",
               style = "padding:6px 10px;",
               icon("crosshairs"),
               " Task: ",     tags$b(state$meta$task_type),
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

    # ---- Model description card -----------------------------------------
    output$model_engine <- renderUI({
      req(input$model_id)
      m <- MODELS[[input$model_id]]
      if (is.null(m)) return(NULL)
      tagList(
        tags$div(style = "margin-top:-6px; margin-bottom:4px;",
          engine_badge(m$engine),
          if (isTRUE(model_is_beta(input$model_id)))
            tags$span(class = "badge",
                      style = "background:#d4af37; color:#0d0d0f; margin-left:6px;",
                      "BETA"),
          if (m$engine == "Python")
            tags$small(class = "text-muted",
              " · requires Python via reticulate")
        ),
        if (!is.null(m$description) && nzchar(m$description))
          tags$div(
            class = "alert alert-secondary",
            style = "font-size:0.85em; padding:8px 10px; margin-top:6px; line-height:1.45;",
            icon("circle-info"), tags$b(" About this model"), br(),
            m$description
          )
      )
    })

    # ---- Best-for / Avoid-when card -------------------------------------
    output$model_meta_card <- renderUI({
      req(input$model_id)
      bf <- model_best_for(input$model_id)
      aw <- model_avoid_when(input$model_id)
      ref <- model_reference_url(input$model_id)
      deps <- model_dependencies(input$model_id)
      if (length(bf) == 0 && length(aw) == 0 &&
          length(deps) == 0 && !nzchar(ref)) return(NULL)
      tags$div(class = "studio-intro", style = "margin-top:8px;",
        tags$div(class = "studio-page",
          if (length(bf) > 0)
            tagList(
              tags$div(class = "studio-kicker", "BEST FOR"),
              tags$ul(style = "margin-top:4px; margin-bottom:8px;",
                lapply(bf, function(x) tags$li(x)))
            ),
          if (length(aw) > 0)
            tagList(
              tags$div(class = "studio-kicker", "AVOID WHEN"),
              tags$ul(style = "margin-top:4px; margin-bottom:8px;",
                lapply(aw, function(x) tags$li(x)))
            ),
          if (length(deps) > 0)
            tagList(
              tags$div(class = "studio-kicker", "DEPENDENCIES"),
              tags$ul(style = "margin-top:4px; margin-bottom:8px;",
                lapply(deps, function(x) tags$li(tags$code(x))))
            ),
          if (nzchar(ref))
            tags$div(style = "margin-top:6px;",
              tags$a(href = ref, target = "_blank",
                     icon("up-right-from-square"), " Reference"))
        )
      )
    })

    # ---- Availability badge / warning -----------------------------------
    # Soft inline variant. The prominent variant (modal with copy-to-
    # clipboard install command) only appears when the user clicks Train.
    output$availability_card <- renderUI({
      req(input$model_id)
      av <- model_availability(input$model_id)
      if (isTRUE(av$ok) && is.null(av$msg)) {
        return(tags$div(class = "dep-notice dep-ready",
          tags$span(class = "dep-dot"),
          tags$span(class = "dep-text", "Runtime available.")))
      }
      msg <- av$msg %||% "This model's runtime is not available."
      tagList(
        tags$div(class = "dep-notice",
          tags$span(class = "dep-dot"),
          tags$span(class = "dep-text",
            if (isTRUE(av$ok)) tagList(tags$b("Note: "), msg)
            else tagList(tags$b("Optional dependency: "), msg)),
          if (!isTRUE(av$ok))
            actionButton(ns("show_install_modal"),
                          tagList(icon("download"), " Install now"),
                          class = "btn-outline-warning btn-sm dep-action")
        )
      )
    })

    # Modal: shows the install command + a copy button. We never
    # auto-execute installs from inside the running Shiny session.
    show_install_modal <- function(model_id, av) {
      m <- MODELS[[model_id]]
      label <- m$label %||% model_id
      cmd <- model_install_command(model_id) %||%
        paste(m$dependencies %||% character(0), collapse = "; ")
      copy_id <- ns("copy_install_cmd")
      js <- sprintf(
        "navigator.clipboard.writeText(%s).then(function(){",
        jsonlite::toJSON(cmd, auto_unbox = TRUE))
      js <- paste0(js,
        "  Shiny.setInputValue('", copy_id, "', Date.now(),",
        "    {priority:'event'});",
        "  var b=document.getElementById('", copy_id, "_btn');",
        "  if(b){b.innerText='Copied';setTimeout(function(){",
        "    b.innerText='Copy command';},1500);}",
        "});")
      showModal(modalDialog(
        title = sprintf("Install %s dependencies", label),
        size = "m", easyClose = TRUE,
        tags$p(av$msg %||% "This model's runtime is not available."),
        tags$p("Run the command below in your R console (not from inside ",
               "this app), then restart the app."),
        tags$code(class = "dep-modal-cmd", cmd),
        tags$div(style = "display:flex; gap:8px;",
          tags$button(id = paste0(copy_id, "_btn"),
                       class = "btn btn-outline-warning btn-sm",
                       onclick = js, "Copy command")),
        footer = tagList(modalButton("Close"))
      ))
    }
    observeEvent(input$show_install_modal, {
      req(input$model_id)
      av <- model_availability(input$model_id)
      if (isTRUE(av$ok)) {
        flash("This model's runtime is already available.", "message")
        return()
      }
      show_install_modal(input$model_id, av)
    })

    # ---- Documentation drawer -------------------------------------------
    output$doc_drawer <- renderUI({
      req(input$model_id)
      doc <- model_long_doc(input$model_id)
      if (!nzchar(doc)) doc <- "No extended documentation provided yet."
      tags$div(class = "studio-intro", style = "margin-top:8px;",
        tags$div(class = "studio-page",
          tags$div(class = "studio-kicker", "EXTENDED DOCUMENTATION"),
          tags$div(style = "margin-top:8px; line-height:1.55; font-size:14px;",
            HTML(.markdown_to_html_basic(doc)))
        )
      )
    })

    # ---- Hyperparameter UI (grouped) ------------------------------------
    output$param_ui <- renderUI({
      req(input$model_id)
      m <- MODELS[[input$model_id]]
      if (is.null(m) || length(m$params) == 0)
        return(tags$em(class = "text-muted",
                       "No tunable hyperparameters."))
      groups <- model_groups(input$model_id)
      param_names <- vapply(m$params, function(p) p$name, character(1))

      # Build group -> params; uncovered params go in a final "General" group.
      if (length(groups) > 0) {
        covered <- unlist(groups, use.names = FALSE)
        uncovered <- setdiff(param_names, covered)
        if (length(uncovered) > 0)
          groups[["General"]] <- c(groups[["General"]] %||% character(0),
                                    uncovered)
      } else {
        groups <- list("Hyperparameters" = param_names)
      }

      render_param <- function(p) {
        nm <- ns(paste0("param_", p$name))
        widget <- switch(p$type,
          "numeric" = numericInput(nm, p$label, value = p$default,
                                    min = p$min, max = p$max, step = p$step),
          "integer" = numericInput(nm, p$label, value = p$default,
                                    min = p$min, max = p$max,
                                    step = p$step %||% 1),
          "logical" = checkboxInput(nm, p$label, value = isTRUE(p$default)),
          "select"  = selectInput(nm, p$label, choices = p$choices,
                                   selected = as.character(p$default)),
          "text"    = textInput(nm, p$label, value = p$default),
          textInput(nm, p$label, value = as.character(p$default %||% ""))
        )
        desc <- p$description %||% p$help
        tags$div(class = "param-block", style = "margin-bottom: 8px;",
          widget,
          if (!is.null(desc) && nzchar(desc))
            tags$small(class = "text-muted",
                       style = "display:block; margin-top:-6px; line-height:1.3;",
                       desc)
        )
      }

      tagList(
        lapply(names(groups), function(gn) {
          ps <- groups[[gn]]
          ps <- intersect(ps, param_names)
          if (length(ps) == 0) return(NULL)
          tags$div(class = "param-group",
                   style = "margin-bottom:14px; padding-bottom:8px; border-bottom:1px solid #2a2a2a;",
            tags$div(class = "studio-kicker",
                     style = "margin-bottom:6px;",
                     toupper(gn)),
            lapply(ps, function(pn) {
              p <- m$params[[which(param_names == pn)]]
              render_param(p)
            })
          )
        })
      )
    })

    # ---- Deep-learning block --------------------------------------------
    # These controls are rendered for any DL model. They override the
    # per-model param_* values for the keys they cover, before fitting.
    output$dl_block_ui <- renderUI({
      req(input$model_id)
      mid <- input$model_id
      is_seq <- mid == "lstm"
      tagList(
        tags$div(class = "studio-kicker", style = "margin-bottom:6px;", "OPTIMIZER"),
        selectInput(ns("dl_optimizer"), "Optimizer",
                    choices = c("adam","sgd","rmsprop","adamw"),
                    selected = "adam"),
        numericInput(ns("dl_lr"), "Learning rate",
                     value = 1e-3, min = 1e-6, max = 1, step = 1e-4),
        numericInput(ns("dl_weight_decay"), "Weight decay",
                     value = 0, min = 0, max = 1, step = 1e-5),

        tags$hr(class = "studio-rule"),
        tags$div(class = "studio-kicker", style = "margin-bottom:6px;", "TRAINING SCHEDULE"),
        numericInput(ns("dl_batch_size"), "Batch size",
                     value = 32, min = 1, max = 4096, step = 1),
        numericInput(ns("dl_epochs"), "Epochs",
                     value = 100, min = 1, max = 5000, step = 1),
        sliderInput(ns("dl_validation_split"),
                    "Validation split (fraction of training data)",
                    min = 0, max = 0.5, value = 0.2, step = 0.05),

        tags$hr(class = "studio-rule"),
        tags$div(class = "studio-kicker", style = "margin-bottom:6px;", "EARLY STOPPING & SCHEDULING"),
        numericInput(ns("dl_es_patience"),
                     "Early stopping patience (epochs, 0 = off)",
                     value = 10, min = 0, max = 200),
        checkboxInput(ns("dl_restore_best"),
                      "Restore best weights",
                      value = TRUE),
        selectInput(ns("dl_lr_scheduler"), "LR scheduler",
                    choices = c("none","step","cosine","reduce_on_plateau"),
                    selected = "none"),

        tags$hr(class = "studio-rule"),
        tags$div(class = "studio-kicker", style = "margin-bottom:6px;", "LOSS & PRECISION"),
        selectInput(ns("dl_loss"), "Loss (auto = task-appropriate)",
                    choices = c("auto","mse","mae","huber",
                                "binary_crossentropy",
                                "sparse_categorical_crossentropy"),
                    selected = "auto"),
        checkboxInput(ns("dl_mixed_precision"),
                      "Mixed precision (fp16/bf16 where supported)",
                      value = FALSE),

        tags$hr(class = "studio-rule"),
        tags$div(class = "studio-kicker", style = "margin-bottom:6px;", "GRADIENT CLIPPING"),
        radioButtons(ns("dl_grad_clip_mode"), NULL,
                     choices = c("Off" = "off", "By value" = "value",
                                 "By global norm" = "norm"),
                     selected = "off", inline = TRUE),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'off'", ns("dl_grad_clip_mode")),
          numericInput(ns("dl_grad_clip_value"),
                       "Clip value / norm",
                       value = 1, min = 0, max = 100, step = 0.1)
        ),

        if (is_seq) {
          tagList(
            tags$hr(class = "studio-rule"),
            tags$div(class = "studio-kicker", style = "margin-bottom:6px;",
                     "SEQUENCE MODEL (LSTM)"),
            numericInput(ns("dl_timesteps"),
                         "Timesteps (window length)",
                         value = 12, min = 2, max = 1000, step = 1),
            numericInput(ns("dl_n_features"),
                         "n_features (auto-detected; override if needed)",
                         value = 1, min = 1, max = 1024, step = 1),
            checkboxInput(ns("dl_stateful"), "Stateful", value = FALSE),
            checkboxInput(ns("dl_return_sequences"),
                          "Return sequences", value = FALSE),
            checkboxInput(ns("dl_bidirectional"),
                          "Bidirectional", value = FALSE),
            numericInput(ns("dl_layers"), "Stacked LSTM layers",
                         value = 1, min = 1, max = 4, step = 1),
            numericInput(ns("dl_dropout"), "Dropout",
                         value = 0, min = 0, max = 0.7, step = 0.05),
            numericInput(ns("dl_recurrent_dropout"),
                         "Recurrent dropout",
                         value = 0, min = 0, max = 0.7, step = 0.05)
          )
        }
      )
    })

    # Auto-fill n_features for sequence models from the prepped data
    observe({
      req(input$model_id == "lstm", state$prepped, state$meta)
      feat_cols <- setdiff(names(state$prepped),
                           c(state$meta$target,
                             if (!is.null(state$meta$time_col) &&
                                 state$meta$time_col != "(none)")
                                state$meta$time_col else NULL))
      nf <- max(1L, length(feat_cols))
      tryCatch(updateNumericInput(session, "dl_n_features", value = nf),
               error = function(e) NULL)
    })

    # ---- Class weighting note -------------------------------------------
    output$class_weight_note <- renderUI({
      req(input$class_weighting)
      if (input$class_weighting == "smote") {
        ok <- requireNamespace("themis", quietly = TRUE)
        cls <- if (ok) "alert alert-info" else "alert alert-warning"
        tags$div(class = cls, style = "font-size:0.85em; padding:6px 10px;",
          icon(if (ok) "circle-info" else "triangle-exclamation"),
          if (ok)
            " 'themis' is installed — SMOTE will be applied during fitting."
          else
            " 'themis' is not installed. Run install.packages('themis') to enable SMOTE.")
      } else if (input$class_weighting == "inverse") {
        tags$small(class = "text-muted",
          "Inverse-frequency weights are passed to the fitter when the model supports a 'weights' argument.")
      } else NULL
    })

    output$parallel_note <- renderUI({
      if (isTRUE(input$parallel)) {
        ok <- requireNamespace("future", quietly = TRUE)
        cls <- if (ok) "alert alert-info" else "alert alert-warning"
        tags$div(class = cls, style = "font-size:0.85em; padding:6px 10px;",
          if (ok)
            tagList(icon("circle-info"),
                    " future::plan(multisession) will be set for the duration of this run.")
          else
            tagList(icon("triangle-exclamation"),
                    " 'future' is not installed; will run sequentially. install.packages('future') to enable."))
      } else NULL
    })

    # ---- Presets --------------------------------------------------------
    apply_preset <- function(kind = c("default","aggressive","conservative")) {
      kind <- match.arg(kind)
      mid <- input$model_id; if (is.null(mid)) return()
      m <- MODELS[[mid]]; if (is.null(m)) return()

      bumps <- list()
      if (mid %in% c("xgb_reg","xgb_cls")) {
        bumps$default      <- list(eta = 0.1,  max_depth = 6, subsample = 0.9, colsample_bytree = 0.9, nrounds = 200)
        bumps$aggressive   <- list(eta = 0.05, max_depth = 10, subsample = 1.0, colsample_bytree = 1.0, nrounds = 1500)
        bumps$conservative <- list(eta = 0.05, max_depth = 4, subsample = 0.7, colsample_bytree = 0.7, nrounds = 500,
                                    gamma = 1, lambda = 5, alpha = 1)
      } else if (mid %in% c("ranger_reg","ranger_cls")) {
        bumps$default      <- list(num_trees = 500, min_node_size = if (grepl("cls", mid)) 1 else 5, sample_fraction = 1)
        bumps$aggressive   <- list(num_trees = 1500, min_node_size = 1, sample_fraction = 1)
        bumps$conservative <- list(num_trees = 1000, min_node_size = 10, sample_fraction = 0.7)
      } else if (mid %in% c("lightgbm_reg","lightgbm_cls")) {
        bumps$default      <- list(num_iterations = 300, learning_rate = 0.05, num_leaves = 31)
        bumps$aggressive   <- list(num_iterations = 2000, learning_rate = 0.02, num_leaves = 127)
        bumps$conservative <- list(num_iterations = 500, learning_rate = 0.05, num_leaves = 15, min_data_in_leaf = 50)
      } else if (mid == "lstm") {
        bumps$default      <- list(units = 64, layers = 1, dropout = 0, learning_rate = 1e-3, epochs = 100)
        bumps$aggressive   <- list(units = 256, layers = 3, dropout = 0.2, recurrent_dropout = 0.1, learning_rate = 5e-4, epochs = 300, bidirectional = TRUE)
        bumps$conservative <- list(units = 32, layers = 1, dropout = 0.3, learning_rate = 1e-3, epochs = 50, early_stopping_patience = 5)
      } else if (mid %in% c("mlp_reg","mlp_cls")) {
        bumps$default      <- list(hidden_units = "128,64", dropout = 0.1, learning_rate = 1e-3, epochs = 50)
        bumps$aggressive   <- list(hidden_units = "256,128,64", dropout = 0.2, learning_rate = 5e-4, epochs = 200)
        bumps$conservative <- list(hidden_units = "64,32", dropout = 0.3, learning_rate = 1e-3, epochs = 30)
      } else if (mid == "kan") {
        bumps$default      <- list(grid = 5, k = 3, steps = 100, learning_rate = 1e-2, optimizer = "LBFGS")
        bumps$aggressive   <- list(grid = 10, k = 3, steps = 500, learning_rate = 5e-3, optimizer = "Adam", lamb_l1 = 0.5)
        bumps$conservative <- list(grid = 3, k = 3, steps = 50, learning_rate = 1e-2, optimizer = "LBFGS", lamb_l1 = 1)
      } else {
        # Fall back to defaults for any other model
        bumps$default <- setNames(lapply(m$params, function(p) p$default),
                                  vapply(m$params, function(p) p$name, character(1)))
        bumps$aggressive <- bumps$default
        bumps$conservative <- bumps$default
      }
      vals <- bumps[[kind]] %||% list()
      for (nm in names(vals)) {
        v <- vals[[nm]]
        spec <- m$params[[which(vapply(m$params, function(p) p$name, character(1)) == nm)[1]]]
        if (length(spec) == 0) next
        input_id <- paste0("param_", nm)
        if (spec$type == "logical") {
          updateCheckboxInput(session, input_id, value = isTRUE(v))
        } else if (spec$type == "select") {
          updateSelectInput(session, input_id, selected = as.character(v))
        } else if (spec$type %in% c("numeric","integer")) {
          updateNumericInput(session, input_id, value = v)
        } else {
          updateTextInput(session, input_id, value = as.character(v))
        }
      }
      flash(sprintf("Applied %s preset for %s", kind, m$label), "message")
    }

    observeEvent(input$preset_default,      apply_preset("default"))
    observeEvent(input$preset_aggressive,   apply_preset("aggressive"))
    observeEvent(input$preset_conservative, apply_preset("conservative"))

    # ---- Param collection -----------------------------------------------
    collect_params <- reactive({
      m <- MODELS[[input$model_id]]
      if (is.null(m)) return(list())
      out <- list()
      for (p in m$params) {
        v <- input[[paste0("param_", p$name)]]
        if (is.null(v)) next
        if (p$type == "integer" && is.numeric(v)) v <- as.integer(v)
        if (p$type == "numeric" && is.numeric(v)) v <- as.numeric(v)
        if (p$type == "logical") v <- isTRUE(v)
        out[[p$name]] <- v
      }
      out
    })

    collect_train_config <- reactive({
      list(
        validation = input$val_scheme %||% "holdout",
        holdout_pct = as.numeric(input$holdout_pct %||% 80),
        kfolds = as.integer(if (identical(input$val_scheme,"repeated_cv"))
                              input$kfolds_rep %||% 5
                            else input$kfolds %||% 5),
        cv_repeats = as.integer(input$cv_repeats %||% 1),
        rolling_initial = as.integer(input$ro_initial %||% 50),
        rolling_h = as.integer(input$ro_h %||% 12),
        rolling_step = as.integer(input$ro_step %||% 1),
        bootstrap_iters = as.integer(input$bootstrap_iters %||% 0),
        class_weighting = input$class_weighting %||% "none",
        early_stop_general = as.integer(input$early_stop_general %||% 0),
        seed = as.integer(input$seed %||% 42),
        seed_lock = isTRUE(input$seed_lock %||% TRUE),
        parallel = isTRUE(input$parallel %||% FALSE),
        compute_budget_min = as.numeric(input$compute_budget_min %||% 0)
      )
    })

    collect_dl_config <- reactive({
      mid <- input$model_id
      if (is.null(mid) || !mid %in% c("lstm","mlp_reg","mlp_cls"))
        return(list())
      cfg <- list(
        optimizer = input$dl_optimizer %||% "adam",
        learning_rate = as.numeric(input$dl_lr %||% 1e-3),
        weight_decay = as.numeric(input$dl_weight_decay %||% 0),
        batch_size = as.integer(input$dl_batch_size %||% 32),
        epochs = as.integer(input$dl_epochs %||% 100),
        validation_split = as.numeric(input$dl_validation_split %||% 0.2),
        early_stopping_patience = as.integer(input$dl_es_patience %||% 10),
        restore_best_weights = isTRUE(input$dl_restore_best %||% TRUE),
        lr_scheduler = input$dl_lr_scheduler %||% "none",
        loss = input$dl_loss %||% "auto",
        mixed_precision = isTRUE(input$dl_mixed_precision %||% FALSE),
        grad_clip_mode = input$dl_grad_clip_mode %||% "off",
        grad_clip_value = as.numeric(input$dl_grad_clip_value %||% 1)
      )
      if (mid == "lstm") {
        cfg$timesteps <- as.integer(input$dl_timesteps %||% 12)
        cfg$n_features <- as.integer(input$dl_n_features %||% 1)
        cfg$stateful <- isTRUE(input$dl_stateful %||% FALSE)
        cfg$return_sequences <- isTRUE(input$dl_return_sequences %||% FALSE)
        cfg$bidirectional <- isTRUE(input$dl_bidirectional %||% FALSE)
        cfg$layers <- as.integer(input$dl_layers %||% 1)
        cfg$dropout <- as.numeric(input$dl_dropout %||% 0)
        cfg$recurrent_dropout <- as.numeric(input$dl_recurrent_dropout %||% 0)
      }
      cfg
    })

    # Merge the DL block into the per-model param dict so the fit function
    # actually receives those values.
    resolved_params <- reactive({
      params <- collect_params()
      dl <- collect_dl_config()
      tcfg <- collect_train_config()
      if (length(dl) > 0) {
        for (k in names(dl)) {
          # DL block wins for keys it covers
          params[[k]] <- dl[[k]]
        }
      }
      # Pipe general early stopping into the model's early-stopping field
      # if that model has no DL block (xgboost / lightgbm).
      if (!is.null(tcfg$early_stop_general) && tcfg$early_stop_general > 0) {
        if (input$model_id %in% c("xgb_reg","xgb_cls"))
          params$early_stopping_rounds <- max(
            as.integer(params$early_stopping_rounds %||% 0L),
            tcfg$early_stop_general)
      }
      # Seed
      if (isTRUE(tcfg$seed_lock) && is.null(params$seed))
        params$seed <- tcfg$seed
      params
    })

    # ---- Live training summary card -------------------------------------
    output$summary_json <- renderText({
      req(input$model_id)
      mid <- input$model_id
      m <- MODELS[[mid]]
      payload <- list(
        model = list(id = mid, label = m$label %||% mid, engine = m$engine %||% "?"),
        hyperparameters = resolved_params(),
        training_config = collect_train_config()
      )
      jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, null = "null")
    })

    # ---- Train ----------------------------------------------------------
    observeEvent(input$train, {
      req(state$prepped, state$meta, input$model_id)

      av <- model_availability(input$model_id)
      if (!isTRUE(av$ok)) {
        # Surface the prominent install-instructions modal so the user
        # has the exact command at hand. Skip training.
        show_install_modal(input$model_id, av)
        add_log("SKIP: ", av$msg %||% "model not available")
        return()
      }

      df <- state$prepped
      target <- state$meta$target
      tcol <- state$meta$time_col
      tcol <- if (!is.null(tcol) && !is.na(tcol) && tcol != "(none)") tcol else NULL
      task <- state$meta$task_type
      m <- MODELS[[input$model_id]]
      tcfg <- collect_train_config()
      params <- resolved_params()

      # Honor seed lock
      if (isTRUE(tcfg$seed_lock)) {
        try(set.seed(tcfg$seed), silent = TRUE)
      }

      # Optional parallel backend
      old_plan <- NULL
      if (isTRUE(tcfg$parallel) && requireNamespace("future", quietly = TRUE)) {
        old_plan <- tryCatch(future::plan(future::multisession),
                             error = function(e) NULL)
      }
      on.exit({
        if (!is.null(old_plan))
          try(future::plan(old_plan), silent = TRUE)
      }, add = TRUE)

      add_log("Training ", m$label, " (", m$engine, ")")

      # ---- Survey sample weights (from Survey -> Model Lab bridge) ----
      # If state$meta$sample_weights names a column in the prepped frame,
      # pass it via params$weights to fitters that support it. Models that
      # don't natively support weights will surface a notice in the log.
      sw_col <- state$meta$sample_weights
      sw_supported <- c("lm_reg","glm_logit","glmnet_reg","glmnet_cls",
                         "gam_reg","ranger_reg","ranger_cls","xgb_reg","xgb_cls",
                         "poisson_glm","negbin_glm","betareg",
                         "survreg")
      if (!is.null(sw_col) && !is.na(sw_col) && nzchar(sw_col) &&
          sw_col %in% names(df)) {
        w_vec <- suppressWarnings(as.numeric(df[[sw_col]]))
        if (any(is.finite(w_vec) & w_vec > 0)) {
          params$weights <- w_vec
          if (input$model_id %in% sw_supported) {
            add_log(sprintf("Applied survey sample weights from `%s`", sw_col))
          } else {
            add_log(sprintf(
              "Survey sample weights `%s` available but ignored by this model",
              sw_col))
          }
        }
      }

      # Class weighting (best-effort: pass weights via params for fitters
      # that accept a 'weights' element).
      if (identical(tcfg$class_weighting, "inverse") &&
          task %in% c("binary_classification","multiclass_classification")) {
        y <- df[[target]]
        tab <- table(y)
        w_per_class <- 1 / as.numeric(tab); names(w_per_class) <- names(tab)
        params$weights <- as.numeric(w_per_class[as.character(y)])
        add_log("Applied inverse-frequency class weights (overrides survey weights for this run)")
      } else if (identical(tcfg$class_weighting, "smote") &&
                 task %in% c("binary_classification","multiclass_classification")) {
        if (requireNamespace("themis", quietly = TRUE)) {
          add_log("Note: SMOTE toggle visible — wire 'themis::step_smote' into the recipe to apply")
        } else {
          add_log("SMOTE toggled but 'themis' not installed; ignored")
        }
      }

      # Prepare modelling frame: drop time column from features
      mdf <- df
      if (!is.null(tcol)) mdf[[tcol]] <- NULL
      # If a sample-weights column was passed, drop it from predictors so
      # it doesn't double as a feature. params$weights still carries the
      # numeric vector aligned with mdf rows.
      if (!is.null(sw_col) && !is.na(sw_col) && nzchar(sw_col) &&
          sw_col %in% names(mdf) && sw_col != target) {
        mdf[[sw_col]] <- NULL
      }
      # Also drop replicate-weight columns from predictors when present.
      rep_w <- state$meta$replicate_weights
      if (!is.null(rep_w) && length(rep_w) > 0) {
        rep_w_in <- intersect(rep_w, names(mdf))
        if (length(rep_w_in) > 0) mdf[, rep_w_in] <- NULL
      }

      # Decide split scheme
      n <- nrow(mdf)
      pct <- pmin(pmax(tcfg$holdout_pct / 100, 0.5), 0.95)
      if (task == "time_series") {
        cut <- floor(n * pct)
        train_idx <- seq_len(cut)
        test_idx  <- setdiff(seq_len(n), train_idx)
      } else {
        train_idx <- sample(seq_len(n), size = floor(n * pct))
        test_idx  <- setdiff(seq_len(n), train_idx)
      }
      train_df <- mdf[train_idx, , drop = FALSE]
      test_df  <- mdf[test_idx, , drop = FALSE]
      # Align weights with the training rows
      if (!is.null(params$weights) && length(params$weights) == n)
        params$weights <- params$weights[train_idx]

      # Compute budget cap
      budget_secs <- as.numeric(tcfg$compute_budget_min) * 60
      t0 <- Sys.time()
      out <- tryCatch({
        # Honor compute budget by wrapping fit + predict in setTimeLimit
        if (is.finite(budget_secs) && budget_secs > 0) {
          setTimeLimit(elapsed = budget_secs, transient = TRUE)
          on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
        }
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
      out$model_id <- m$id
      out$train_config <- tcfg
      state$last_model <- out
      state$last_params <- params
      state$n_train <- nrow(train_df)
      add_log(sprintf("Done in %.2fs · %s", duration,
                      fmt_metrics(out$metrics)))

      run_id <- db_save_run(state$dataset_id, m$id, task, params, out$metrics,
                            horizon = if (task == "time_series")
                                        length(out$predicted) else NA,
                            duration = duration)
      preds_df <- data.frame(
        actual    = as.numeric(out$actual),
        predicted = as.numeric(out$predicted),
        lower     = if (length(out$lower) == length(out$predicted))
                      out$lower else NA_real_,
        upper     = if (length(out$upper) == length(out$predicted))
                      out$upper else NA_real_,
        ts        = NA_character_
      )
      db_save_predictions(run_id, preds_df)
      state$last_run_id <- run_id
    })

    # ---- Output panels --------------------------------------------------
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
        add_lines(y = ~actual, name = "Actual",
                  line = list(color = "#c9d1d9")) |>
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
               shapes = list(list(type = "line",
                                  x0 = min(p, na.rm = TRUE),
                                  x1 = max(p, na.rm = TRUE),
                                  y0 = 0, y1 = 0,
                                  line = list(dash = "dash",
                                              color = "#888"))))
    })

    output$imp_plot <- renderPlotly({
      req(state$last_model)
      imp <- state$last_model$fit$feat_imp
      if (is.null(imp) || length(imp) == 0)
        return(plotly_empty(type = "scatter") |>
                 layout(title = "Not available for this model",
                        paper_bgcolor = "#0d1117",
                        plot_bgcolor = "#0d1117",
                        font = list(color = "#c9d1d9")))
      imp <- sort(imp, decreasing = TRUE)
      d <- data.frame(feature = names(imp), importance = as.numeric(imp))
      d <- utils::head(d, 25)
      plot_ly(d, x = ~importance, y = ~reorder(feature, importance),
              type = "bar", orientation = "h",
              marker = list(color = "#3fb950")) |>
        layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117",
               font = list(color = "#c9d1d9"),
               xaxis = list(title = "Importance"),
               yaxis = list(title = ""))
    })

    output$diagnostics_card <- renderUI({
      req(state$last_model)
      diag <- state$last_model$fit$diagnostics
      if (is.null(diag) || length(diag) == 0)
        return(tags$div(class = "alert alert-secondary",
                        "No model-fit diagnostics published by this model."))
      best_iter <- state$last_model$fit$model$best_iteration %||% NULL
      tags$div(class = "studio-intro",
        tags$div(class = "studio-page",
          tags$div(class = "studio-kicker", "DIAGNOSTICS"),
          tags$ul(style = "margin-top:8px; line-height:1.55;",
            if (!is.null(diag$backend))
              tags$li(tags$b("Backend: "), tags$code(diag$backend)),
            if (!is.null(diag$timesteps))
              tags$li(tags$b("Timesteps: "), diag$timesteps),
            if (!is.null(diag$epochs_run))
              tags$li(tags$b("Epochs run: "), diag$epochs_run),
            if (!is.null(diag$final_loss))
              tags$li(tags$b("Final training loss: "),
                      sprintf("%.6f", diag$final_loss)),
            if (!is.null(best_iter))
              tags$li(tags$b("Best iteration: "), best_iter),
            if (!is.null(diag$width))
              tags$li(tags$b("KAN width: "),
                      paste(diag$width, collapse = " - "))
          ),
          if (!is.null(diag$history) &&
              !is.null(diag$history$loss) &&
              length(diag$history$loss) > 1) {
            ph <- diag$history$loss
            tags$div(style = "margin-top:8px;",
              tags$div(class = "studio-kicker", "TRAINING LOSS CURVE"),
              plot_ly(
                x = seq_along(ph), y = unlist(ph),
                type = "scatter", mode = "lines",
                line = list(color = "#3fb950")
              ) |> layout(
                paper_bgcolor = "#0d0d0f",
                plot_bgcolor  = "#0d0d0f",
                font = list(color = "#f5f3ee"),
                xaxis = list(title = "Epoch"),
                yaxis = list(title = "Loss"),
                height = 240, margin = list(t = 20, b = 30)
              )
            )
          }
        )
      )
    })

    output$log_box <- renderText({
      paste(log_msgs(), collapse = "\n")
    })

    # ---- Save project ----------------------------------------------
    observeEvent(input$save_project, {
      bundle <- tryCatch(project_save(state),
                          error = function(e) {
                            flash(paste("Save failed:",
                                          conditionMessage(e)), "error"); NULL })
      if (!is.null(bundle))
        flash(sprintf("Project saved to %s", bundle), "message")
    })

    # ---- Brief report ---------------------------------------------
    output$export_brief_html <- downloadHandler(
      filename = function()
        sprintf("brief_report_run%s_%s.html",
                state$last_run_id %||% "0",
                format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        r <- tryCatch(render_brief_report(state, format = "html",
                                            file_path = file,
                                            theme = state$chrome_theme %||% "bundesbank"),
                       error = function(e) NULL)
        if (is.null(r) || !file.exists(file))
          writeLines("<p>Report failed.</p>", file)
      }
    )
    output$export_brief_pdf <- downloadHandler(
      filename = function()
        sprintf("brief_report_run%s_%s.pdf",
                state$last_run_id %||% "0",
                format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        r <- tryCatch(render_brief_report(state, format = "pdf",
                                            file_path = file,
                                            theme = state$chrome_theme %||% "bundesbank"),
                       error = function(e) NULL)
        if (is.null(r) || !file.exists(file)) {
          tryCatch(render_brief_report(state, format = "html",
                                         file_path = file,
                                         theme = state$chrome_theme %||% "bundesbank"),
                    error = function(e) writeLines("Report failed.", file))
        }
      }
    )

    # ---- Manifest modal -------------------------------------------
    observeEvent(input$show_manifest, {
      m <- tryCatch(make_manifest(state), error = function(e) list(error = conditionMessage(e)))
      txt <- jsonlite::toJSON(m, auto_unbox = TRUE, pretty = TRUE,
                                na = "null", null = "null")
      showModal(modalDialog(
        title = "Reproducibility Manifest",
        size = "l", easyClose = TRUE,
        tags$pre(style = "max-height:60vh;overflow:auto;",
                  as.character(txt)),
        footer = tagList(modalButton("Close"))
      ))
    })
  })
}

# Tiny markdown -> HTML renderer for the documentation drawer.
# Handles **bold**, *italic*, `code`, paragraphs, line breaks. Avoids a
# heavy dependency for what is essentially body copy.
.markdown_to_html_basic <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return("")
  s <- as.character(txt)
  # Escape HTML
  s <- gsub("&",  "&amp;",  s, fixed = TRUE)
  s <- gsub("<",  "&lt;",   s, fixed = TRUE)
  s <- gsub(">",  "&gt;",   s, fixed = TRUE)
  # Inline code
  s <- gsub("`([^`]+)`", "<code>\\1</code>", s)
  # Bold + italic (bold first)
  s <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", s, perl = TRUE)
  s <- gsub("\\*([^*]+)\\*", "<em>\\1</em>", s, perl = TRUE)
  # Paragraphs (blank line) and soft breaks
  paras <- strsplit(s, "\n\n+", perl = TRUE)[[1]]
  paras <- vapply(paras, function(p) {
    p <- gsub("\n", "<br>", p, fixed = TRUE)
    paste0("<p>", p, "</p>")
  }, character(1))
  paste(paras, collapse = "\n")
}
