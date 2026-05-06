# =====================================================================
# Module 1: Data Ingest
#
# Workspace model
# ---------------
# state$datasets is a named list keyed by `ds_id` (string). Each entry is
#   list(name=, data=, meta=, labels=, survey_hints=, format=, loaded_at=,
#        n_rows=, n_cols=)
# state$active_id is the currently active dataset id (or NULL).
# Backward-compat shortcuts (state$raw_data, state$dataset_name,
# state$labels, state$survey_hints, state$meta) are mirrored from the
# active slot whenever active_id changes — this means downstream modules
# (Explore / Data Prep / Privacy / Survey / Model Lab / Predict /
# Dashboard / Studio / Runs) keep working without modification.
#
# Precedence rules
# ----------------
# - File upload ALWAYS appends a new entry and switches active to it,
#   even if a sample is currently loaded.
# - "Load sample X": if the workspace is empty, append + activate.
#   Otherwise append but DO NOT auto-activate (the user picks it via the
#   Workspace card).
# =====================================================================

# Internal: build a fresh dataset entry from a raw data.frame
.ds_entry <- function(name, df, format = "table") {
  labels <- tryCatch(extract_labels(df), error = function(e) list())
  df2    <- tryCatch(harmonize_labelled(df), error = function(e) df)
  hints  <- tryCatch(detect_survey_columns(df2, labels),
                      error = function(e) list())
  list(
    name         = name,
    data         = df2,
    labels       = labels,
    survey_hints = hints,
    meta         = NULL,
    format       = format,
    loaded_at    = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    n_rows       = nrow(df2),
    n_cols       = ncol(df2)
  )
}

# Internal: append entry to state$datasets and optionally activate it.
# Returns the new id.
.workspace_add <- function(state, entry, activate = TRUE) {
  ds <- state$datasets %||% list()
  # Generate a stable, unique id
  i <- length(ds) + 1L
  ds_id <- sprintf("ds_%d", i)
  while (ds_id %in% names(ds)) {
    i <- i + 1L
    ds_id <- sprintf("ds_%d", i)
  }
  ds[[ds_id]] <- entry
  state$datasets <- ds
  if (activate) state$active_id <- ds_id
  ds_id
}

ingest_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    div(
      card(
        card_header("Upload data"),
        fileInput(ns("file"),
          "CSV / TSV / TXT / XLSX / Parquet / JSON / RDS / Stata (.dta) / SPSS (.sav .por) / SAS (.sas7bdat .xpt)",
          accept = c(".csv",".tsv",".txt",".xlsx",".xls",".parquet",".json",".rds",
                     ".dta",".sav",".por",".sas7bdat",".xpt"),
          width = "100%"),
        hr(),
        strong("Or load a sample dataset"),
        div(style = "display:flex; gap:6px; flex-wrap:wrap; margin-top:6px;",
          actionButton(ns("sample_air"),  "AirPassengers (TS)", class="btn-sm btn-outline-primary"),
          actionButton(ns("sample_iris"), "Iris (Classif.)",    class="btn-sm btn-outline-primary"),
          actionButton(ns("sample_mt"),   "Mtcars (Reg.)",      class="btn-sm btn-outline-primary"),
          actionButton(ns("sample_ins"),  "Insurance (Reg.)",   class="btn-sm btn-outline-primary"),
          actionButton(ns("sample_survey"), "Survey demo",      class="btn-sm btn-outline-warning")
        ),
        tags$small(class = "text-muted", style = "display:block;margin-top:6px;",
          "Samples append to the workspace. They auto-activate only when the workspace is empty."),
        hr(),
        uiOutput(ns("schema_ui")),
        uiOutput(ns("save_ui"))
      ),
      card(
        card_header(tagList(icon("layer-group"), "Workspace")),
        uiOutput(ns("workspace_ui"))
      )
    ),
    card(
      card_header(textOutput(ns("preview_title"))),
      withSpinner(DT::DTOutput(ns("preview")), color = "#3fb950")
    )
  )
}

ingest_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Mirror active slot into legacy shortcuts ---------------------
    # Whenever active_id changes, refresh state$raw_data / labels / etc.
    observeEvent(state$active_id, ignoreNULL = FALSE, ignoreInit = TRUE, {
      aid <- state$active_id
      if (is.null(aid) || is.null(state$datasets[[aid]])) {
        state$raw_data     <- NULL
        state$dataset_name <- NULL
        state$labels       <- list()
        state$survey_hints <- list()
        state$meta         <- NULL
        state$dataset_id   <- NULL
        state$prepped      <- NULL
        state$recipe       <- NULL
        state$survey_design <- NULL
        return(invisible())
      }
      e <- state$datasets[[aid]]
      state$raw_data     <- e$data
      state$dataset_name <- e$name
      state$labels       <- e$labels %||% list()
      state$survey_hints <- e$survey_hints %||% list()
      state$meta         <- e$meta
      state$dataset_id   <- e$dataset_id
      # Reset prep / model artifacts when switching datasets — they're
      # tied to the previous one and would mis-render otherwise.
      state$prepped      <- NULL
      state$recipe       <- NULL
      state$survey_design <- NULL
    })

    # ---- File upload (always wins, always activates) -----------------
    observeEvent(input$file, {
      req(input$file)
      df <- tryCatch(read_uploaded(input$file$datapath, input$file$name),
                     error = function(e) { flash(conditionMessage(e), "error"); NULL })
      req(df)
      ext <- tolower(tools::file_ext(input$file$name))
      e <- .ds_entry(input$file$name, df, format = ext)
      ds_id <- .workspace_add(state, e, activate = TRUE)
      flash(sprintf("Loaded %s (%s rows × %s cols)",
                    e$name, e$n_rows, e$n_cols), "message")
    })

    # ---- Sample datasets: append; activate iff workspace empty -------
    .add_sample <- function(name, df, format = "sample") {
      empty <- length(state$datasets %||% list()) == 0
      e <- .ds_entry(name, df, format = format)
      .workspace_add(state, e, activate = empty)
      if (empty) {
        flash(sprintf("Loaded sample '%s' as active dataset", name), "message")
      } else {
        flash(sprintf("Sample '%s' added to workspace — pick it active to use", name), "message")
      }
    }

    observeEvent(input$sample_air, {
      d <- data.frame(month = seq.Date(as.Date("1949-01-01"),
                                       by = "month", length.out = 144),
                      passengers = as.numeric(AirPassengers))
      .add_sample("AirPassengers", d)
    })
    observeEvent(input$sample_iris, { .add_sample("iris", iris) })
    observeEvent(input$sample_mt, {
      d <- mtcars; d$model <- rownames(mtcars); rownames(d) <- NULL
      .add_sample("mtcars", d)
    })
    observeEvent(input$sample_ins, {
      set.seed(42); n <- 1000
      d <- data.frame(
        age = sample(18:65, n, TRUE),
        bmi = round(rnorm(n, 27, 5), 1),
        children = sample(0:5, n, TRUE),
        smoker = sample(c("yes","no"), n, TRUE, prob = c(0.2, 0.8)),
        region = sample(c("NW","NE","SW","SE"), n, TRUE)
      )
      d$charges <- 2500 + d$age*120 + d$bmi*150 + d$children*500 +
                   ifelse(d$smoker=="yes",18000,0) + rnorm(n, 0, 2000)
      .add_sample("synthetic_insurance", d)
    })
    observeEvent(input$sample_survey, {
      d <- tryCatch(make_survey_demo(),
                    error = function(e) { flash(conditionMessage(e), "error"); NULL })
      req(d)
      .add_sample("survey_demo", d, format = "labelled")
    })

    # ---- Workspace card UI -------------------------------------------
    output$workspace_ui <- renderUI({
      ds <- state$datasets %||% list()
      if (length(ds) == 0)
        return(tags$div(class = "text-muted",
                        style = "padding:6px 0; font-size:0.9em;",
                        "No datasets loaded yet. Upload a file or click a sample."))
      aid <- state$active_id %||% ""
      rows <- lapply(names(ds), function(k) {
        e <- ds[[k]]
        is_active <- isTRUE(k == aid)
        chip_cls <- if (is_active) "badge bg-success" else "badge bg-secondary"
        chip_txt <- if (is_active) "ACTIVE" else "available"
        actions <- tagList(
          if (!is_active)
            actionButton(ns(paste0("activate_", k)), "Make active",
                          class = "btn-sm btn-outline-primary"),
          actionButton(ns(paste0("remove_", k)), "Remove",
                        class = "btn-sm btn-outline-danger")
        )
        tags$div(
          style = paste0("display:flex; align-items:center; justify-content:space-between;",
                          "gap:8px; padding:8px 6px; border-bottom:1px solid #30363d;"),
          tags$div(
            tags$div(tags$strong(e$name),
                     " ", tags$span(class = chip_cls, chip_txt)),
            tags$small(class = "text-muted",
              sprintf("%d rows × %d cols · %s · loaded %s",
                       e$n_rows, e$n_cols, e$format %||% "?",
                       e$loaded_at %||% "?"))
          ),
          tags$div(style = "display:flex; gap:6px;", actions)
        )
      })
      tags$div(rows)
    })

    # ---- Wire up dynamic activate / remove buttons -------------------
    # We register a single observer per dataset id the first time we see it.
    seen_ids <- reactiveVal(character(0))
    observe({
      ds_names <- names(state$datasets %||% list())
      seen <- seen_ids()
      new_ids <- setdiff(ds_names, seen)
      for (k in new_ids) local({
        kk <- k
        observeEvent(input[[paste0("activate_", kk)]], {
          if (!is.null(state$datasets[[kk]])) {
            state$active_id <- kk
            flash(sprintf("Active dataset → %s",
                           state$datasets[[kk]]$name), "message")
          }
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("remove_", kk)]], {
          ds <- state$datasets
          if (is.null(ds[[kk]])) return()
          rm_name <- ds[[kk]]$name
          ds[[kk]] <- NULL
          state$datasets <- ds
          if (isTRUE(state$active_id == kk)) {
            state$active_id <- if (length(ds) > 0) names(ds)[1] else NULL
          }
          flash(sprintf("Removed '%s' from workspace", rm_name), "warning")
        }, ignoreInit = TRUE)
      })
      if (length(new_ids) > 0) seen_ids(c(seen, new_ids))
    })

    # ---- Preview ------------------------------------------------------
    output$preview_title <- renderText({
      if (is.null(state$raw_data)) "Preview" else
        sprintf("Preview · %s · %d rows × %d cols",
                state$dataset_name, nrow(state$raw_data), ncol(state$raw_data))
    })

    output$preview <- DT::renderDT({
      req(state$raw_data)
      DT::datatable(head(state$raw_data, 200),
                    options = list(scrollX = TRUE, pageLength = 10,
                                   dom = "tip", searchHighlight = TRUE),
                    rownames = FALSE, class = "compact stripe")
    })

    # ---- Schema configuration (for active dataset) -------------------
    output$schema_ui <- renderUI({
      req(state$raw_data)
      df <- state$raw_data
      cols <- names(df)
      time_candidates <- c("(none)", cols[sapply(df, function(c)
        inherits(c, c("Date","POSIXt")) ||
        (is.character(c) && !any(is.na(suppressWarnings(as.Date(c))))) )])
      if (length(time_candidates) == 1) time_candidates <- c("(none)", cols)
      # Restore previous selection if the active dataset already has meta
      m <- state$meta %||% list()
      tagList(
        h6("Schema"),
        selectInput(ns("target"), "Target column",
                    choices = cols,
                    selected = m$target %||% utils::tail(cols, 1)),
        selectInput(ns("time_col"), "Time column (optional)",
                    choices = time_candidates,
                    selected = m$time_col %||% "(none)"),
        selectInput(ns("frequency"), "Data frequency",
                    choices = c("daily","weekly","monthly","quarterly","yearly","none"),
                    selected = m$frequency %||% "monthly"),
        uiOutput(ns("task_hint"))
      )
    })

    auto_task <- reactive({
      req(state$raw_data, input$target)
      tcol <- input$time_col
      tcp <- !is.null(tcol) && tcol != "(none)"
      detect_task_type(state$raw_data[[input$target]], time_col_present = tcp)
    })

    output$task_hint <- renderUI({
      tt <- auto_task()
      tags$div(class = "alert alert-secondary",
               style = "padding:6px 10px; margin-top:4px;",
               icon("wand-magic-sparkles"),
               "Detected task: ", tags$b(tt))
    })

    output$save_ui <- renderUI({
      req(state$raw_data, input$target)
      actionButton(ns("save"), "Save dataset & continue ➜",
                   class = "btn-primary w-100", style = "margin-top:8px;")
    })

    observeEvent(input$save, {
      req(state$raw_data, input$target)
      meta <- list(
        frequency = input$frequency,
        target    = input$target,
        time_col  = if (input$time_col == "(none)") NA_character_ else input$time_col,
        task_type = auto_task()
      )
      state$meta <- meta
      ds_id_db <- db_save_dataset(state$dataset_name %||% "dataset",
                                   state$raw_data, meta)
      state$dataset_id <- ds_id_db
      # Persist meta + db id back into the workspace entry so it survives
      # active switching.
      aid <- state$active_id
      if (!is.null(aid) && !is.null(state$datasets[[aid]])) {
        ds <- state$datasets
        ds[[aid]]$meta       <- meta
        ds[[aid]]$dataset_id <- ds_id_db
        state$datasets <- ds
      }
      flash(sprintf("Saved as dataset #%s. Move to 'Explore & Prep'.", ds_id_db),
            "message")
    })
  })
}
