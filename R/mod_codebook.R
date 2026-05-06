# =====================================================================
# Codebook viewer module
#
# A label-aware, searchable codebook powered by haven labels.
# Builds a row per variable with: name, type, label, value labels
# (count + expandable list), missing %, n unique, example values,
# numeric range / quantile summary.
#
# Public API:
#   build_codebook_rich(df, labels = list())
#       Returns a tidy data.frame suitable for DT::datatable().
#   codebook_to_markdown(rows)
#       Returns a markdown table string.
#   codebook_ui(id, ns)
#       Card with search box, type filter, copy/download buttons,
#       expandable value-label dictionary.
#   codebook_server(id, state)
#       Wires the module to state$raw_data + state$labels.
#
# Author: Malik Hebbat
# =====================================================================

# ---- Builder --------------------------------------------------------
# Returns a data.frame with columns:
#   name, type, label, n_value_labels, value_labels_str, value_labels_list,
#   pct_missing, n_unique, sample, summary
build_codebook_rich <- function(df, labels = list()) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0)
    return(data.frame(name = character(0), type = character(0),
                       label = character(0), n_value_labels = integer(0),
                       value_labels_str = character(0),
                       value_labels_list = I(list()),
                       pct_missing = numeric(0), n_unique = integer(0),
                       sample = character(0), summary = character(0),
                       stringsAsFactors = FALSE))

  if (is.null(labels)) labels <- list()
  vlabs  <- labels$var_labels   %||% list()
  vvlabs <- labels$value_labels %||% list()

  rows <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    n <- length(x)
    n_miss <- sum(is.na(x))
    pct_missing <- if (n > 0) round(100 * n_miss / n, 2) else NA_real_

    # Variable label
    lab <- vlabs[[nm]]
    if (is.null(lab) || !nzchar(as.character(lab))) {
      a <- attr(x, "label", exact = TRUE)
      lab <- if (!is.null(a)) as.character(a) else ""
    }

    # Value labels: prefer state$labels$value_labels, else attr(x,'labels')
    valab <- vvlabs[[nm]]
    if (is.null(valab) && inherits(x, "haven_labelled")) {
      vl <- attr(x, "labels", exact = TRUE)
      if (!is.null(vl) && length(vl) > 0)
        valab <- setNames(as.character(names(vl)), as.character(unname(vl)))
    }

    val_str <- if (!is.null(valab) && length(valab) > 0)
      paste(sprintf("%s = %s", names(valab), unname(valab)),
            collapse = "; ") else ""

    # Type classification (broad bucket for the filter)
    cls <- class(x)
    type <- if ("haven_labelled" %in% cls) "labelled"
            else if (is.factor(x))            "factor"
            else if (is.numeric(x))           "numeric"
            else if (is.logical(x))           "logical"
            else if (inherits(x, "Date"))     "date"
            else if (inherits(x, "POSIXt"))   "datetime"
            else if (is.character(x))         "character"
            else                                  paste(cls, collapse = "/")

    n_unique <- length(unique(stats::na.omit(x)))

    # Example values: first 3 unique non-NA values
    sample_vals <- utils::head(unique(stats::na.omit(x)), 3L)
    sample_str <- paste(format(sample_vals), collapse = " | ")
    if (nchar(sample_str) > 80) sample_str <- paste0(substr(sample_str, 1, 77), "...")

    # Summary block: numeric range/quantiles, or top-level frequencies
    summ <- ""
    xn <- x
    if (inherits(xn, "haven_labelled")) xn <- as.numeric(unclass(xn))
    if (is.numeric(xn) && any(is.finite(xn))) {
      qq <- tryCatch(stats::quantile(xn, c(0.0, 0.25, 0.5, 0.75, 1.0),
                                      na.rm = TRUE),
                      error = function(e) NULL)
      if (!is.null(qq))
        summ <- sprintf("min=%g · Q25=%g · med=%g · Q75=%g · max=%g",
                         qq[1], qq[2], qq[3], qq[4], qq[5])
    } else if (is.factor(x) || is.character(x)) {
      tab <- sort(table(x, useNA = "no"), decreasing = TRUE)
      tk  <- utils::head(names(tab), 3)
      summ <- paste(sprintf("%s=%d", tk, as.integer(tab[tk])), collapse = "; ")
    }

    data.frame(
      name              = nm,
      type              = type,
      label             = as.character(lab),
      n_value_labels    = if (is.null(valab)) 0L else as.integer(length(valab)),
      value_labels_str  = val_str,
      value_labels_list = I(list(valab %||% NULL)),
      pct_missing       = pct_missing,
      n_unique          = as.integer(n_unique),
      sample            = sample_str,
      summary           = summ,
      stringsAsFactors  = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# ---- Markdown export -------------------------------------------------
codebook_to_markdown <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0)
    return("(empty codebook)")
  cols <- c("name","type","label","pct_missing","n_unique",
            "n_value_labels","value_labels_str","summary")
  cols <- intersect(cols, names(rows))
  hdr  <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep  <- paste0("|", paste(rep("---", length(cols)), collapse = "|"), "|")
  body <- vapply(seq_len(nrow(rows)), function(i) {
    paste0("| ",
           paste(vapply(cols, function(cn) {
             v <- rows[[cn]][i]
             if (is.list(v)) v <- paste(unlist(v), collapse = ", ")
             v <- as.character(v)
             v <- gsub("\\|", "\\\\|", v)
             v <- gsub("\n", " ", v, fixed = TRUE)
             if (is.na(v) || !nzchar(v)) "—" else v
           }, character(1)), collapse = " | "),
           " |")
  }, character(1))
  paste(c(hdr, sep, body), collapse = "\n")
}

# ---- UI --------------------------------------------------------------
codebook_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "studio-kicker", "CODEBOOK"),
    tags$div(style = "display:flex; gap:8px; flex-wrap:wrap; margin:6px 0 10px;",
      selectInput(ns("type_filter"), NULL,
                   choices = c("All types" = "",
                                "Numeric" = "numeric",
                                "Labelled" = "labelled",
                                "Factor" = "factor",
                                "Character" = "character",
                                "Logical" = "logical",
                                "Date" = "date",
                                "Datetime" = "datetime"),
                   selected = "", width = "180px"),
      textInput(ns("search"), NULL, placeholder = "Search name/label…",
                 width = "260px"),
      actionButton(ns("copy_md"), "Copy as markdown",
                    class = "btn-sm btn-outline-secondary",
                    icon = icon("clipboard")),
      downloadButton(ns("download_md"), "Download .md",
                      class = "btn-sm btn-outline-info")
    ),
    DT::DTOutput(ns("table")),
    tags$hr(class = "studio-rule"),
    tags$div(class = "studio-kicker", "VALUE-LABEL DICTIONARY"),
    tags$small(class = "text-muted", style = "display:block;",
      "Click a labelled row above to inspect its value→label mapping."),
    uiOutput(ns("value_labels"))
  )
}

# ---- Server ----------------------------------------------------------
codebook_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rows_r <- reactive({
      df  <- state$raw_data
      lab <- state$labels %||% list()
      build_codebook_rich(df, lab)
    })

    filtered_rows <- reactive({
      r <- rows_r()
      if (is.null(r) || nrow(r) == 0) return(r)
      if (!is.null(input$type_filter) && nzchar(input$type_filter))
        r <- r[r$type == input$type_filter, , drop = FALSE]
      if (!is.null(input$search) && nzchar(input$search)) {
        s <- tolower(input$search)
        keep <- grepl(s, tolower(r$name),  fixed = TRUE) |
                grepl(s, tolower(r$label), fixed = TRUE)
        r <- r[keep, , drop = FALSE]
      }
      r
    })

    output$table <- DT::renderDT({
      r <- filtered_rows()
      if (is.null(r) || nrow(r) == 0)
        return(DT::datatable(data.frame(message = "No variables match.")))
      # The list-column 'value_labels_list' is not displayable; drop it.
      view <- r[, setdiff(names(r), "value_labels_list"), drop = FALSE]
      DT::datatable(
        view,
        filter = "top",
        selection = "single",
        options = list(pageLength = 15, scrollX = TRUE, dom = "tip"),
        rownames = FALSE, class = "compact stripe"
      )
    })

    output$value_labels <- renderUI({
      r <- filtered_rows()
      sel <- input$table_rows_selected
      if (is.null(r) || nrow(r) == 0 || is.null(sel) || length(sel) == 0)
        return(tags$div(class = "text-muted",
                        style = "padding:8px 0; font-size:0.9em;",
                        "Select a row to view its value labels."))
      row <- r[sel[1], , drop = FALSE]
      vl <- row$value_labels_list[[1]]
      if (is.null(vl) || length(vl) == 0)
        return(tags$div(class = "text-muted",
                        style = "padding:8px 0; font-size:0.9em;",
                        sprintf("`%s` has no value labels.", row$name)))
      pairs <- data.frame(value = names(vl),
                          label = unname(vl),
                          stringsAsFactors = FALSE)
      tagList(
        tags$h6(sprintf("`%s` — %s", row$name, row$label)),
        DT::renderDT(DT::datatable(pairs,
          options = list(pageLength = 25, dom = "tip"),
          rownames = FALSE, class = "compact stripe"))()
      )
    })

    output$download_md <- downloadHandler(
      filename = function()
        sprintf("codebook_%s.md", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        writeLines(codebook_to_markdown(filtered_rows()), file)
      }
    )

    observeEvent(input$copy_md, {
      md <- codebook_to_markdown(filtered_rows())
      session$sendCustomMessage("shinyml_copy_clipboard",
                                 list(text = md))
      flash("Codebook copied as markdown.", "message")
    })
  })
}
