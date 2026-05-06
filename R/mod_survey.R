# =====================================================================
# Module: Survey & Panel Analysis
#   Built on `survey` + `srvyr` (loaded lazily). Falls back to
#   unweighted base-R when those packages are unavailable.
#   Includes: design declaration, weighted descriptives, weighted
#   crosstabs (chi-square design-corrected), Likert renderer, codebook,
#   panel handlers (attrition, transitions), AI interpretation hook.
# =====================================================================

# ---- Internal: strip haven_labelled and zap labels before svy* calls
.svy_zap <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      lab <- attr(col, "labels", exact = TRUE)
      if (!is.null(lab) && length(lab) > 0 && length(lab) <= 50) {
        df[[nm]] <- factor(unclass(col), levels = unname(lab),
                            labels = names(lab))
      } else {
        df[[nm]] <- as.numeric(unclass(col))
      }
    }
  }
  df
}

# ---- Survey design helper -------------------------------------------
# Returns list(design=, kind=, message=, df=). df is the (possibly
# row-filtered) frame that backs the design — useful for fallbacks.
make_design <- function(df, weight_col = NULL, strata_col = NULL,
                         psu_col = NULL) {
  norm <- function(x) {
    if (is.null(x)) return(NULL)
    if (length(x) == 0) return(NULL)
    x <- x[1]
    if (is.na(x) || !nzchar(x) || x == "(none)") return(NULL)
    if (!x %in% names(df)) return(NULL)
    x
  }
  weight_col <- norm(weight_col)
  strata_col <- norm(strata_col)
  psu_col    <- norm(psu_col)

  df2 <- .svy_zap(df)

  # If a weight column is given, drop rows with NA / non-positive weight
  if (!is.null(weight_col)) {
    w <- suppressWarnings(as.numeric(df2[[weight_col]]))
    keep <- !is.na(w) & is.finite(w) & w > 0
    df2 <- df2[keep, , drop = FALSE]
    df2[[weight_col]] <- w[keep]
  }

  if (!requireNamespace("survey", quietly = TRUE)) {
    return(list(design = NULL, kind = "unweighted",
                 message = "Install 'survey' for weighted analysis",
                 df = df2))
  }

  ids     <- if (!is.null(psu_col)) stats::as.formula(paste0("~", psu_col)) else stats::as.formula("~1")
  strata  <- if (!is.null(strata_col)) stats::as.formula(paste0("~", strata_col)) else NULL
  weights <- if (!is.null(weight_col)) stats::as.formula(paste0("~", weight_col)) else NULL

  des <- tryCatch(
    survey::svydesign(ids = ids, strata = strata, weights = weights,
                       data = df2, nest = TRUE),
    error = function(e) {
      attr(des, "err") <- conditionMessage(e); NULL
    })
  if (is.null(des)) {
    msg <- tryCatch(
      survey::svydesign(ids = ids, strata = strata, weights = weights,
                         data = df2),  # retry without nest
      error = function(e) conditionMessage(e))
    if (inherits(msg, "survey.design")) {
      return(list(design = msg, kind = "weighted",
                   message = "Declared (without nesting)", df = df2))
    }
    return(list(design = NULL, kind = "error",
                 message = paste("Could not declare design:", as.character(msg)),
                 df = df2))
  }
  kind <- if (is.null(weights)) "unweighted" else "weighted"
  list(design = des, kind = kind,
        message = sprintf("Declared with %d rows", nrow(df2)),
        df = df2)
}

# ---- Weighted descriptives ------------------------------------------
weighted_summary <- function(design, var) {
  unweighted_summary <- function(x) {
    if (inherits(x, "haven_labelled")) x <- as.numeric(unclass(x))
    if (is.numeric(x)) {
      data.frame(stat = c("mean","sd","median","min","max"),
                  value = c(mean(x, na.rm=TRUE), stats::sd(x, na.rm=TRUE),
                            stats::median(x, na.rm=TRUE),
                            suppressWarnings(min(x, na.rm=TRUE)),
                            suppressWarnings(max(x, na.rm=TRUE))),
                  stringsAsFactors = FALSE)
    } else {
      tab <- prop.table(table(x, useNA = "no"))
      data.frame(level = names(tab),
                  proportion = as.numeric(tab),
                  stringsAsFactors = FALSE)
    }
  }

  if (is.null(design$design)) {
    x <- design$df[[var]]
    if (is.null(x)) return(data.frame(error = sprintf("Variable '%s' not in data", var)))
    return(unweighted_summary(x))
  }
  des <- design$design
  x <- des$variables[[var]]
  if (is.null(x)) return(data.frame(error = sprintf("Variable '%s' not in design", var)))
  if (inherits(x, "haven_labelled")) x <- as.numeric(unclass(x))
  if (is.numeric(x)) {
    m <- tryCatch(survey::svymean(stats::as.formula(paste0("~", var)),
                                    des, na.rm = TRUE),
                   error = function(e) NULL)
    if (is.null(m)) return(unweighted_summary(des$variables[[var]]))
    q <- tryCatch(survey::svyquantile(stats::as.formula(paste0("~", var)),
                                       des, c(0.25, 0.5, 0.75), na.rm = TRUE),
                   error = function(e) NULL)
    out <- data.frame(stat = c("mean","SE"),
                       value = c(as.numeric(coef(m)),
                                 sqrt(diag(as.matrix(vcov(m))))),
                       stringsAsFactors = FALSE)
    if (!is.null(q)) {
      qx <- q[[var]]
      qd <- if (is.matrix(qx)) as.numeric(qx[, 1]) else as.numeric(qx)
      if (length(qd) == 3) {
        out <- rbind(out, data.frame(stat = c("Q25","Median","Q75"), value = qd))
      }
    }
    out
  } else {
    p <- tryCatch(survey::svymean(stats::as.formula(paste0("~", var)),
                                    des, na.rm = TRUE),
                   error = function(e) NULL)
    if (is.null(p)) return(unweighted_summary(des$variables[[var]]))
    cf <- coef(p)
    se <- sqrt(diag(as.matrix(vcov(p))))
    data.frame(level = gsub(paste0("^", var), "", names(cf)),
                proportion = as.numeric(cf),
                SE = as.numeric(se),
                stringsAsFactors = FALSE)
  }
}

# ---- Likert detection -----------------------------------------------
detect_likert <- function(x, labels = NULL) {
  likert_kw <- paste0("agree|disagree|neutral|nie|selten|manchmal|oft|immer|",
                      "strongly|always|never|trifft\\s*zu|stimme")
  # If haven_labelled with a small label set, examine the label names
  if (inherits(x, "haven_labelled")) {
    val_lab <- attr(x, "labels", exact = TRUE)
    if (!is.null(val_lab)) {
      n <- length(val_lab)
      if (n >= 3 && n <= 7) {
        if (any(grepl(likert_kw, names(val_lab), ignore.case = TRUE))) return(TRUE)
        if (n %in% c(5L, 7L)) return(TRUE)
      }
    }
    x <- as.numeric(unclass(x))
  }
  if (is.factor(x) || is.character(x)) {
    lv <- if (is.factor(x)) levels(x) else unique(stats::na.omit(x))
    n <- length(lv)
    if (n >= 3 && n <= 7) {
      if (any(grepl(likert_kw, lv, ignore.case = TRUE))) return(TRUE)
      if (n %in% c(5L, 7L)) return(TRUE)
    }
  }
  if (is.numeric(x)) {
    u <- unique(stats::na.omit(x))
    if (length(u) >= 3 && length(u) <= 7 &&
        all(u == round(u))) return(TRUE)
  }
  if (!is.null(labels) && length(labels) >= 3 && length(labels) <= 7 &&
      any(grepl(likert_kw, names(labels) %||% labels, ignore.case = TRUE)))
    return(TRUE)
  FALSE
}

# ---- Panel data inspector -------------------------------------------
panel_summary <- function(df, id_col, wave_col) {
  if (is.null(id_col) || is.null(wave_col) ||
      !id_col %in% names(df) || !wave_col %in% names(df))
    return(list(message = "Set ID and Wave columns first"))
  ids   <- df[[id_col]]
  waves <- df[[wave_col]]
  n_units <- length(unique(ids))
  n_waves <- length(unique(waves))
  obs_per_unit <- table(ids)
  full_balance <- mean(obs_per_unit == n_waves)
  attrition <- 1 - full_balance
  # First/last wave size
  wsizes <- table(waves)
  list(
    n_units = n_units,
    n_waves = n_waves,
    n_observations = nrow(df),
    pct_balanced = round(100 * full_balance, 2),
    pct_attrition = round(100 * attrition, 2),
    waves = data.frame(wave = names(wsizes),
                        n = as.integer(wsizes),
                        stringsAsFactors = FALSE),
    avg_obs_per_unit = round(mean(obs_per_unit), 2)
  )
}

# ---- Codebook -------------------------------------------------------
build_codebook <- function(df, labels = list()) {
  if (is.null(labels)) labels <- list()
  vlabs <- labels$var_labels %||% list()
  vvlabs <- labels$value_labels %||% list()
  # Also pull labels straight off column attributes if `labels` is empty —
  # haven_labelled keeps them on the column even after harmonize_labelled.
  rows <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    vlab <- vlabs[[nm]]
    if (is.null(vlab) || !nzchar(as.character(vlab))) {
      a <- attr(x, "label", exact = TRUE)
      vlab <- if (!is.null(a)) as.character(a) else ""
    }
    valab <- vvlabs[[nm]]
    if (is.null(valab) && inherits(x, "haven_labelled")) {
      vl <- attr(x, "labels", exact = TRUE)
      if (!is.null(vl) && length(vl) > 0)
        valab <- setNames(as.character(names(vl)), as.character(unname(vl)))
    }
    val_str <- if (!is.null(valab) && length(valab) > 0)
      paste(sprintf("%s=%s", names(valab), valab), collapse = "; ") else ""
    data.frame(
      variable = nm,
      label = vlab,
      class = paste(class(x), collapse = "/"),
      n_missing = sum(is.na(x)),
      pct_missing = round(100 * sum(is.na(x)) / max(1, length(x)), 2),
      n_unique = length(unique(stats::na.omit(x))),
      value_labels = val_str,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# ---- Synthetic survey demo dataset ----------------------------------
# Builds a small data.frame with weight, strata, psu, id, wave, plus a
# Likert variable and a categorical with value labels via haven::labelled
# (or a plain factor fallback if haven is not installed).
make_survey_demo <- function(n = 400, seed = 42) {
  set.seed(seed)
  region <- sample(c("North","South","East","West"), n, TRUE)
  strata <- sample(c("urban","rural"), n, TRUE, prob = c(0.6, 0.4))
  psu    <- paste0("PSU_", sample(1:20, n, TRUE))
  id     <- seq_len(n)
  wave   <- sample(2018:2022, n, TRUE)
  weight <- round(runif(n, 0.4, 2.5), 3)
  age    <- sample(18:80, n, TRUE)
  income <- round(rlnorm(n, meanlog = 10.2, sdlog = 0.4))
  # Likert satisfaction (1=strongly disagree ... 5=strongly agree)
  sat    <- sample(1:5, n, TRUE, prob = c(0.10, 0.18, 0.24, 0.28, 0.20))
  edu    <- sample(1:4, n, TRUE, prob = c(0.20, 0.35, 0.30, 0.15))

  df <- data.frame(
    id = id, wave = wave, weight = weight, strata = strata, psu = psu,
    age = age, income = income, region = region,
    satisfaction = sat, education = edu,
    stringsAsFactors = FALSE
  )

  if (requireNamespace("haven", quietly = TRUE)) {
    df$satisfaction <- haven::labelled(
      sat,
      labels = c("Strongly disagree" = 1, "Disagree" = 2, "Neutral" = 3,
                  "Agree" = 4, "Strongly agree" = 5),
      label = "Overall life satisfaction (Likert 1-5)")
    df$education <- haven::labelled(
      edu,
      labels = c("None/Primary" = 1, "Secondary" = 2,
                  "Tertiary" = 3, "Postgrad" = 4),
      label = "Highest education attained")
  } else {
    df$satisfaction <- factor(sat, levels = 1:5,
      labels = c("Strongly disagree","Disagree","Neutral",
                  "Agree","Strongly agree"))
    attr(df$satisfaction, "label") <- "Overall life satisfaction (Likert 1-5)"
    df$education <- factor(edu, levels = 1:4,
      labels = c("None/Primary","Secondary","Tertiary","Postgrad"))
    attr(df$education, "label") <- "Highest education attained"
  }
  # Variable labels (work for both haven and fallback paths)
  attr(df$age, "label")    <- "Age in years"
  attr(df$income, "label") <- "Annual income (local currency)"
  attr(df$weight, "label") <- "Survey weight"
  attr(df$strata, "label") <- "Sampling stratum"
  attr(df$psu, "label")    <- "Primary sampling unit"
  attr(df$wave, "label")   <- "Survey wave (year)"
  df
}

# ---- UI / Server -----------------------------------------------------
survey_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header(tagList(icon("clipboard-question"), "Design declaration")),
      div(style = "display:flex; gap:6px; flex-wrap:wrap; margin-bottom:8px;",
        actionButton(ns("load_demo"), "Load survey demo",
                      class = "btn-sm btn-outline-warning",
                      icon = icon("flask"))),
      tags$small(class = "text-muted", style = "display:block;margin-bottom:8px;",
        "Demo creates a synthetic dataset with weight / strata / PSU / id / wave plus a labelled Likert item."),
      uiOutput(ns("design_inputs")),
      hr(),
      actionButton(ns("declare"), "Declare design",
                    class = "btn-primary w-100", icon = icon("check")),
      hr(),
      uiOutput(ns("design_status"))
    ),
    div(
      uiOutput(ns("hero_panel")),
      hr(),
      navset_card_underline(
        title = "Survey workspace",
        nav_panel("Codebook",     DT::DTOutput(ns("codebook"))),
        nav_panel("Descriptives", uiOutput(ns("descr_ui"))),
        nav_panel("Crosstab",     uiOutput(ns("xtab_ui"))),
        nav_panel("Likert grid",  uiOutput(ns("likert_ui"))),
        nav_panel("Panel report", verbatimTextOutput(ns("panel_out")))
      )
    )
  )
}

survey_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    design_state <- reactiveVal(list(design = NULL, kind = "none",
                                      message = "Not declared yet"))

    # ---- inputs auto-suggested from heuristics ---------------------
    output$design_inputs <- renderUI({
      req(state$raw_data)
      cols <- c("(none)", names(state$raw_data))
      h <- state$survey_hints %||% list()
      tagList(
        selectInput(ns("weight"), "Weight column",  cols,
                    selected = (h$weight  %||% "(none)")[1] %||% "(none)"),
        selectInput(ns("strata"), "Strata column",  cols,
                    selected = (h$strata  %||% "(none)")[1] %||% "(none)"),
        selectInput(ns("psu"),    "PSU / cluster",  cols,
                    selected = (h$psu     %||% "(none)")[1] %||% "(none)"),
        hr(),
        tags$h6("Panel"),
        selectInput(ns("id_col"),   "Respondent ID", cols,
                    selected = (h$id      %||% "(none)")[1] %||% "(none)"),
        selectInput(ns("wave_col"), "Wave / period", cols,
                    selected = (h$wave    %||% "(none)")[1] %||% "(none)")
      )
    })

    # ---- "Load survey demo" — adds a synthetic survey dataset to the
    # workspace (always activates so the user can immediately exercise it).
    observeEvent(input$load_demo, {
      d <- tryCatch(make_survey_demo(),
                    error = function(e) { flash(conditionMessage(e), "error"); NULL })
      req(d)
      ds <- state$datasets %||% list()
      i <- length(ds) + 1L
      ds_id <- sprintf("ds_%d", i)
      while (ds_id %in% names(ds)) { i <- i + 1L; ds_id <- sprintf("ds_%d", i) }
      labels <- tryCatch(extract_labels(d), error = function(e) list())
      d2 <- tryCatch(harmonize_labelled(d), error = function(e) d)
      hints <- tryCatch(detect_survey_columns(d2, labels),
                        error = function(e) list())
      ds[[ds_id]] <- list(
        name = "survey_demo", data = d2, labels = labels,
        survey_hints = hints, meta = NULL, format = "labelled",
        loaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        n_rows = nrow(d2), n_cols = ncol(d2)
      )
      state$datasets  <- ds
      state$active_id <- ds_id
      flash("Loaded survey_demo and made it active", "message")
    })

    observeEvent(input$declare, {
      req(state$raw_data)
      d <- make_design(state$raw_data,
                        weight_col = input$weight,
                        strata_col = input$strata,
                        psu_col    = input$psu)
      d$df <- state$raw_data
      design_state(d)
      state$survey_design <- list(
        weight = input$weight, strata = input$strata, psu = input$psu,
        id = input$id_col, wave = input$wave_col, kind = d$kind)
      tryCatch(db_save_survey_design(state$dataset_id,
                                       state$survey_design),
                error = function(e) NULL)
      flash(sprintf("Design declared (%s): %s", d$kind, d$message), "message")
    })

    output$design_status <- renderUI({
      d <- design_state()
      icon_name <- if (d$kind == "weighted") "circle-check"
                    else if (d$kind == "error") "triangle-exclamation"
                    else "circle-info"
      bg <- if (d$kind == "weighted") "alert-success"
            else if (d$kind == "error") "alert-danger"
            else "alert-secondary"
      div(class = sprintf("alert %s", bg),
        icon(icon_name), tags$b(toupper(d$kind)), " - ", d$message)
    })

    # ---- Hero header ----------------------------------------------
    output$hero_panel <- renderUI({
      req(state$raw_data)
      d <- design_state()
      grade_col <- if (d$kind == "weighted") "#3fb950" else "#8b949e"
      div(class = "studio-intro", style = "margin-top:12px;",
        div(class = "studio-page",
          div(class = "studio-hero",
            div(class = "studio-hero-meta",
              tags$span(class = "studio-kicker", "SURVEY \u0026 PANEL"),
              tags$span(class = "studio-dot", "\u2022"),
              tags$span(class = "studio-grade",
                style = sprintf("color:%s;border-color:%s;", grade_col, grade_col),
                toupper(d$kind))),
            tags$h1(class = "studio-headline",
              ifelse(is.null(state$dataset_name),
                      "Untitled survey", state$dataset_name)),
            tags$p(class = "studio-deck",
              sprintf("%s rows \u00B7 %s columns \u00B7 %s",
                       formatC(nrow(state$raw_data), format = "d", big.mark = ","),
                       ncol(state$raw_data),
                       d$message)),
            div(class = "studio-rule"))))
    })

    # ---- Codebook --------------------------------------------------
    output$codebook <- DT::renderDT({
      req(state$raw_data)
      cb <- build_codebook(state$raw_data, state$labels %||% list())
      DT::datatable(cb, filter = "top",
        options = list(pageLength = 15, dom = "tip", scrollX = TRUE),
        rownames = FALSE, class = "compact stripe")
    })

    # ---- Descriptives ----------------------------------------------
    output$descr_ui <- renderUI({
      req(state$raw_data)
      tagList(
        selectInput(ns("descr_var"), "Variable", names(state$raw_data)),
        DT::DTOutput(ns("descr_table"))
      )
    })
    output$descr_table <- DT::renderDT({
      req(input$descr_var)
      d <- design_state(); d$df <- state$raw_data
      tab <- tryCatch(weighted_summary(d, input$descr_var),
                       error = function(e) data.frame(error = conditionMessage(e)))
      DT::datatable(tab, options = list(dom = "tip", pageLength = 15),
                     rownames = FALSE, class = "compact stripe")
    })

    # ---- Crosstab --------------------------------------------------
    output$xtab_ui <- renderUI({
      req(state$raw_data)
      cols <- names(state$raw_data)
      tagList(
        fluidRow(
          column(6, selectInput(ns("xt_row"), "Row variable", cols,
                                selected = cols[1])),
          column(6, selectInput(ns("xt_col"), "Column variable", cols,
                                selected = utils::tail(cols, 1)))),
        verbatimTextOutput(ns("xtab_out"))
      )
    })
    output$xtab_out <- renderText({
      req(input$xt_row, input$xt_col)
      d <- design_state()
      df <- state$raw_data
      r <- df[[input$xt_row]]; c <- df[[input$xt_col]]
      if (!is.null(d$design) && requireNamespace("survey", quietly = TRUE)) {
        f <- stats::as.formula(paste("~", input$xt_row, "+", input$xt_col))
        tab <- tryCatch(survey::svytable(f, d$design),
                         error = function(e) NULL)
        chi <- tryCatch(survey::svychisq(f, d$design),
                         error = function(e) NULL)
        if (is.null(tab)) return("Could not compute weighted xtab")
        out <- capture.output(print(tab))
        if (!is.null(chi)) out <- c(out, "", capture.output(print(chi)))
        paste(out, collapse = "\n")
      } else {
        out <- capture.output(print(table(r, c)))
        chi <- tryCatch(suppressWarnings(stats::chisq.test(table(r, c))),
                         error = function(e) NULL)
        if (!is.null(chi))
          out <- c(out, "", capture.output(print(chi)))
        paste(out, collapse = "\n")
      }
    })

    # ---- Likert grid ----------------------------------------------
    output$likert_ui <- renderUI({
      req(state$raw_data)
      candidates <- names(state$raw_data)[
        vapply(state$raw_data, detect_likert, logical(1))]
      if (length(candidates) == 0)
        return(tags$div(class = "alert alert-secondary",
                        "No Likert-style items detected (3-7 ordered levels)."))
      tagList(
        selectInput(ns("lik_var"), "Likert item", candidates,
                    selected = candidates[1]),
        plotly::plotlyOutput(ns("lik_plot"), height = "300px")
      )
    })
    output$lik_plot <- plotly::renderPlotly({
      req(input$lik_var, state$raw_data)
      x <- state$raw_data[[input$lik_var]]
      lab <- state$labels$value_labels[[input$lik_var]]
      if (!is.null(lab) && is.numeric(x))
        x <- factor(x, levels = unname(lab), labels = names(lab))
      tab <- table(x, useNA = "ifany")
      pct <- prop.table(tab) * 100
      df <- data.frame(level = names(pct), pct = as.numeric(pct))
      plotly::plot_ly(df, x = ~level, y = ~pct, type = "bar",
                       marker = list(color = "#d4af37")) |>
        plotly::layout(paper_bgcolor = "#0d0d0f",
                        plot_bgcolor  = "#0d0d0f",
                        font = list(color = "#e8e6e0"),
                        yaxis = list(title = "Percent"),
                        xaxis = list(title = ""))
    })

    # ---- Panel report ---------------------------------------------
    output$panel_out <- renderText({
      req(state$raw_data)
      id <- input$id_col; wv <- input$wave_col
      if (is.null(id) || is.null(wv) || id == "(none)" || wv == "(none)")
        return("Select Respondent ID and Wave to compute panel summary.")
      ps <- panel_summary(state$raw_data, id, wv)
      if (!is.null(ps$message)) return(ps$message)
      paste0(
        "Units (respondents): ", ps$n_units, "\n",
        "Waves: ", ps$n_waves, "\n",
        "Total observations: ", ps$n_observations, "\n",
        "Avg obs / unit: ", ps$avg_obs_per_unit, "\n",
        "Balanced: ", ps$pct_balanced, "% \u00B7 Attrition: ", ps$pct_attrition, "%\n\n",
        "Wave sizes:\n",
        paste(sprintf("  %s : %d", ps$waves$wave, ps$waves$n), collapse = "\n")
      )
    })
  })
}
