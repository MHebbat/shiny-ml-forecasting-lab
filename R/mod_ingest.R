# =====================================================================
# Module 1: Data Ingest
# =====================================================================

ingest_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
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
        actionButton(ns("sample_ins"),  "Insurance (Reg.)",   class="btn-sm btn-outline-primary")
      ),
      hr(),
      uiOutput(ns("schema_ui")),
      uiOutput(ns("save_ui"))
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

    # ---- load uploaded file ------------------------------------------
    observeEvent(input$file, {
      req(input$file)
      df <- tryCatch(read_uploaded(input$file$datapath, input$file$name),
                     error = function(e) { flash(conditionMessage(e), "error"); NULL })
      req(df)
      # Capture variable + value labels (Stata / SPSS / SAS); harmonise haven_labelled cols
      state$labels <- tryCatch(extract_labels(df), error = function(e) list())
      df <- tryCatch(harmonize_labelled(df), error = function(e) df)
      state$raw_data <- df
      state$dataset_name <- input$file$name
      # Auto-detect possible survey design columns
      state$survey_hints <- tryCatch(
        detect_survey_columns(df, state$labels),
        error = function(e) list())
      flash(sprintf("Loaded %s (%s rows × %s cols)",
                    input$file$name, nrow(df), ncol(df)), "message")
    })

    # Helper: reset labels/hints when loading a sample dataset
    .reset_aux <- function() { state$labels <- list(); state$survey_hints <- list() }

    # ---- sample datasets ---------------------------------------------
    observeEvent(input$sample_air, {
      .reset_aux()
      d <- data.frame(month = seq.Date(as.Date("1949-01-01"),
                                       by = "month", length.out = 144),
                      passengers = as.numeric(AirPassengers))
      state$raw_data <- d; state$dataset_name <- "AirPassengers"
      flash("Loaded AirPassengers sample","message")
    })
    observeEvent(input$sample_iris, {
      .reset_aux()
      state$raw_data <- iris; state$dataset_name <- "iris"
      flash("Loaded Iris sample","message")
    })
    observeEvent(input$sample_mt, {
      .reset_aux()
      d <- mtcars; d$model <- rownames(mtcars); rownames(d) <- NULL
      state$raw_data <- d; state$dataset_name <- "mtcars"
      flash("Loaded mtcars sample","message")
    })
    observeEvent(input$sample_ins, {
      .reset_aux()
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
      state$raw_data <- d; state$dataset_name <- "synthetic_insurance"
      flash("Loaded synthetic insurance sample","message")
    })

    # ---- preview ------------------------------------------------------
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

    # ---- schema configuration ----------------------------------------
    output$schema_ui <- renderUI({
      req(state$raw_data)
      df <- state$raw_data
      cols <- names(df)
      time_candidates <- c("(none)", cols[sapply(df, function(c)
        inherits(c, c("Date","POSIXt")) ||
        (is.character(c) && !any(is.na(suppressWarnings(as.Date(c))))) )])
      if (length(time_candidates) == 1) time_candidates <- c("(none)", cols)
      tagList(
        h6("Schema"),
        selectInput(ns("target"), "Target column",
                    choices = cols, selected = utils::tail(cols, 1)),
        selectInput(ns("time_col"), "Time column (optional)",
                    choices = time_candidates, selected = "(none)"),
        selectInput(ns("frequency"), "Data frequency",
                    choices = c("daily","weekly","monthly","quarterly","yearly","none"),
                    selected = "monthly"),
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
      ds_id <- db_save_dataset(state$dataset_name %||% "dataset",
                               state$raw_data, meta)
      state$dataset_id <- ds_id
      flash(sprintf("Saved as dataset #%s. Move to 'Explore & Prep'.", ds_id),
            "message")
    })
  })
}
