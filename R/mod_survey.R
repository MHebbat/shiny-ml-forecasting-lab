# =====================================================================
# Module: Survey & Panel Analysis
#   Built on `survey` + `srvyr` (loaded lazily). Falls back to
#   unweighted base-R when those packages are unavailable.
#   Includes: design declaration (Standard Taylor or Replicate-weights),
#   complex-survey templates (PHF, HFCN), multi-implicate (MI) detection,
#   weighted descriptives + crosstabs, Likert renderer, codebook,
#   panel handlers (attrition, transitions), and a "Send to Model Lab"
#   bridge that materialises a survey-prepped dataset slot and switches
#   the navbar to Model Lab.
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

# ---- Multi-implicate detection --------------------------------------
# Detects either a long-format `implicate` column or wide-format
# imputed value sets like `var_imp1`, `var_imp2`. Returns
# list(kind = "none" | "long" | "wide", ...).
detect_implicates <- function(df) {
  long_cols <- intersect(names(df),
                          c("implicate", "imputation", "imp", "imp_id"))
  if (length(long_cols) > 0) {
    implicate_col <- long_cols[1]
    n_imp <- length(unique(stats::na.omit(df[[implicate_col]])))
    return(list(kind = "long", column = implicate_col, n = n_imp))
  }
  # Wide: "imp1"/"imp2"/... or "_i1"/"_i2"/... markers
  pat <- "(_imp|_i)([0-9]+)$"
  mset <- grepl(pat, names(df))
  if (any(mset)) {
    nums <- as.integer(sub(paste0(".*", pat), "\\2", names(df)[mset]))
    n_imp <- length(unique(nums))
    if (n_imp >= 2)
      return(list(kind = "wide",
                  pattern = pat,
                  n = n_imp))
  }
  list(kind = "none", n = 0)
}

# ---- Survey design helper -------------------------------------------
# Builds either a Taylor-linearisation svydesign or a replicate-weights
# svrepdesign, plus optional implicates. Returns a list with $design,
# $kind ("standard" | "replicate" | "unweighted" | "error"),
# $message, $df, $implicates (NULL when not detected).
make_design <- function(df,
                         method = c("standard", "replicate"),
                         weight_col = NULL, strata_col = NULL,
                         psu_col = NULL,
                         repweights_pattern = NULL,
                         repdesign_type = "BRR",
                         combined_weights = TRUE,
                         use_mse = TRUE,
                         implicates = NULL) {
  method <- match.arg(method)

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

  # Drop rows with non-positive / NA weight (when given)
  if (!is.null(weight_col)) {
    w <- suppressWarnings(as.numeric(df2[[weight_col]]))
    keep <- !is.na(w) & is.finite(w) & w > 0
    df2 <- df2[keep, , drop = FALSE]
    df2[[weight_col]] <- w[keep]
  }

  if (!requireNamespace("survey", quietly = TRUE)) {
    return(list(design = NULL, kind = "unweighted",
                 message = "Install 'survey' for weighted analysis",
                 df = df2, implicates = implicates))
  }

  if (method == "replicate") {
    if (is.null(repweights_pattern) || !nzchar(repweights_pattern))
      return(list(design = NULL, kind = "error",
                   message = "Replicate-weights pattern is required.",
                   df = df2, implicates = implicates))
    rep_cols <- grep(repweights_pattern, names(df2), value = TRUE)
    if (length(rep_cols) == 0)
      return(list(design = NULL, kind = "error",
                   message = sprintf("No columns match pattern '%s'.",
                                       repweights_pattern),
                   df = df2, implicates = implicates))
    repdesign_type <- toupper(as.character(repdesign_type %||% "BRR"))
    rep_args <- list(
      data        = df2,
      repweights  = df2[, rep_cols, drop = FALSE],
      type        = repdesign_type,
      combined.weights = isTRUE(combined_weights),
      mse         = isTRUE(use_mse)
    )
    if (!is.null(weight_col)) rep_args$weights <- df2[[weight_col]]
    des <- tryCatch(do.call(survey::svrepdesign, rep_args),
                     error = function(e) {
                       attr(e, "err") <- conditionMessage(e); NULL
                     })
    if (is.null(des))
      return(list(design = NULL, kind = "error",
                   message = "Could not declare replicate-weight design.",
                   df = df2, implicates = implicates))
    return(list(design = des, kind = "replicate",
                 message = sprintf(
                   "Replicate design (%s, %d replicates) on %d rows",
                   repdesign_type, length(rep_cols), nrow(df2)),
                 df = df2, implicates = implicates,
                 rep_cols = rep_cols))
  }

  # Standard Taylor linearisation
  ids     <- if (!is.null(psu_col)) stats::as.formula(paste0("~", psu_col)) else stats::as.formula("~1")
  strata  <- if (!is.null(strata_col)) stats::as.formula(paste0("~", strata_col)) else NULL
  weights <- if (!is.null(weight_col)) stats::as.formula(paste0("~", weight_col)) else NULL

  des <- tryCatch(
    survey::svydesign(ids = ids, strata = strata, weights = weights,
                       data = df2, nest = TRUE),
    error = function(e) {
      attr(e, "err") <- conditionMessage(e); NULL
    })
  if (is.null(des)) {
    msg <- tryCatch(
      survey::svydesign(ids = ids, strata = strata, weights = weights,
                         data = df2),
      error = function(e) conditionMessage(e))
    if (inherits(msg, "survey.design"))
      return(list(design = msg, kind = "standard",
                   message = "Declared (without nesting)",
                   df = df2, implicates = implicates))
    return(list(design = NULL, kind = "error",
                 message = paste("Could not declare design:",
                                  as.character(msg)),
                 df = df2, implicates = implicates))
  }
  kind <- if (is.null(weights)) "unweighted" else "standard"
  list(design = des, kind = kind,
        message = sprintf("Standard design declared with %d rows", nrow(df2)),
        df = df2, implicates = implicates)
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
  attr(df$age, "label")    <- "Age in years"
  attr(df$income, "label") <- "Annual income (local currency)"
  attr(df$weight, "label") <- "Survey weight"
  attr(df$strata, "label") <- "Sampling stratum"
  attr(df$psu, "label")    <- "Primary sampling unit"
  attr(df$wave, "label")   <- "Survey wave (year)"
  df
}

# ---- Survey templates -----------------------------------------------
# Each template returns a list of suggested column / parameter mappings
# to populate the design declaration form.
SURVEY_TEMPLATES <- list(
  phf = list(
    label = "Bundesbank Panel on Household Finances (PHF)",
    description = paste(
      "Bundesbank PHF: weight = `wgt`, replicate weights = `wr_*`",
      "(BRR / Fay), strata = `wsr` / sampling stratum, PSU = `wsr` / `psu`,",
      "household id = `hid`, person id = `pid`, multi-implicate = 5 (long",
      "format with `implicate` column).", sep = " "),
    method = "replicate",
    weight = c("wgt", "weight", "phf_wgt"),
    repweights_pattern = "^wr_|^wgt_rep_|^repwgt_",
    repdesign_type = "Fay",
    combined_weights = TRUE,
    use_mse = TRUE,
    strata = c("wsr", "stratum", "schicht"),
    psu = c("psu", "cluster", "wsr"),
    id  = c("hid", "pid", "household_id"),
    wave = c("wave", "welle"),
    implicate = c("implicate", "imp", "imp_id")
  ),
  hfcn = list(
    label = "ECB Household Finance and Consumption Network (HFCN)",
    description = paste(
      "ECB HFCN: weight = `hw0001` (household), replicate weights",
      "`hw0002`-`hw1000`, strata = `sa0100`, PSU = `sa0010`,",
      "household id = `id`, multi-implicate = 5 (wide `*_imp1..5`).",
      sep = " "),
    method = "replicate",
    weight = c("hw0001", "weight", "hfcn_weight"),
    repweights_pattern = "^hw[0-9]{4}$|^hw_rep_|^wgt_rep_",
    repdesign_type = "bootstrap",
    combined_weights = TRUE,
    use_mse = TRUE,
    strata = c("sa0100", "stratum"),
    psu = c("sa0010", "psu"),
    id  = c("id", "hid"),
    wave = c("wave"),
    implicate = c("implicate", "imp")
  )
)

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
      tags$div(class = "studio-kicker", style = "margin-bottom:6px;", "TEMPLATES"),
      div(style = "display:flex; gap:6px; flex-wrap:wrap; margin-bottom:8px;",
        actionButton(ns("tpl_phf"),  "Apply PHF",
                      class = "btn-sm btn-outline-primary"),
        actionButton(ns("tpl_hfcn"), "Apply HFCN",
                      class = "btn-sm btn-outline-primary")),
      tags$small(class = "text-muted", style = "display:block;margin-bottom:8px;",
        "Templates auto-fill the form with PHF / HFCN column conventions."),
      tags$hr(class = "studio-rule", style = "margin:6px 0;"),
      tags$div(class = "studio-kicker", style = "margin-bottom:6px;", "METHOD"),
      radioButtons(ns("design_method"), NULL, inline = TRUE,
                    choices = c("Standard (Taylor)" = "standard",
                                "Replicate weights" = "replicate"),
                    selected = "standard"),
      uiOutput(ns("design_inputs")),
      uiOutput(ns("repweight_inputs")),
      uiOutput(ns("implicate_badge")),
      tags$hr(class = "studio-rule"),
      tags$div(class = "studio-kicker", "VARIANCE ESTIMATION"),
      selectInput(ns("variance_method"), NULL,
                   choices = c(
                     "Taylor linearization (default)" = "taylor",
                     "Bootstrap"                       = "bootstrap",
                     "JK1 (jackknife)"                 = "JK1",
                     "JKn (stratified jackknife)"      = "JKn",
                     "BRR (balanced repeated rep)"     = "BRR",
                     "Fay's BRR"                       = "Fay",
                     "ACS-style replicate weights"     = "ACS"
                   ),
                   selected = "taylor", width = "100%"),
      conditionalPanel(
        condition = sprintf(
          "['bootstrap','JK1','JKn','BRR','Fay'].indexOf(input['%s']) >= 0",
          ns("variance_method")),
        sliderInput(ns("var_replicates"), "Replicate count",
                     min = 50, max = 2000, value = 200, step = 50)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Fay'", ns("variance_method")),
        sliderInput(ns("brr_rho"), "Fay rho", min = 0, max = 0.5,
                     value = 0, step = 0.05)
      ),
      tags$small(class = "text-muted", style = "display:block;",
        "Method governs every weighted descriptive, weighted xtab, and svyglm SE."),
      hr(),
      actionButton(ns("declare"), "Declare design",
                    class = "btn-primary w-100", icon = icon("check")),
      hr(),
      uiOutput(ns("design_status")),
      hr(),
      actionButton(ns("send_modellab"), "Send to Model Lab →",
                    class = "btn-warning w-100",
                    icon = icon("paper-plane")),
      tags$small(class = "text-muted", style = "display:block;margin-top:6px;",
        "Materialises a survey-prepped slot in the workspace and switches to Model Lab. ",
        "Models that support 'weights' will use the declared survey weights.")
    ),
    div(
      uiOutput(ns("hero_panel")),
      hr(),
      navset_card_underline(
        title = "Survey workspace",
        nav_panel("Codebook",     codebook_ui(ns("cb"))),
        nav_panel("Descriptives", uiOutput(ns("descr_ui"))),
        nav_panel("Crosstab",     uiOutput(ns("xtab_ui"))),
        nav_panel("MI Pooled",    uiOutput(ns("mi_ui"))),
        nav_panel("Likert grid",  uiOutput(ns("likert_ui"))),
        nav_panel("Panel report", verbatimTextOutput(ns("panel_out")))
      )
    )
  )
}

survey_server <- function(id, state, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    design_state <- reactiveVal(list(design = NULL, kind = "none",
                                      message = "Not declared yet",
                                      df = NULL, implicates = NULL))

    .first_match <- function(candidates, names_in_df) {
      hit <- intersect(candidates, names_in_df)
      if (length(hit) > 0) return(hit[1])
      # case-insensitive fallback
      lower <- tolower(names_in_df)
      cand_lower <- tolower(candidates)
      i <- which(lower %in% cand_lower)
      if (length(i) > 0) return(names_in_df[i[1]])
      NA_character_
    }

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

    # Replicate-weight inputs (visible when method == 'replicate')
    output$repweight_inputs <- renderUI({
      req(input$design_method)
      if (input$design_method != "replicate") return(NULL)
      tagList(
        tags$hr(class = "studio-rule"),
        tags$div(class = "studio-kicker", "REPLICATE-WEIGHT CONFIG"),
        textInput(ns("rep_pattern"), "Replicate-weight column regex",
                   value = "^wgt_rep_|^repwgt_|^wr_"),
        selectInput(ns("rep_type"), "Replicate type",
                     choices = c("BRR", "Fay", "JK1", "JK2", "JKn",
                                 "bootstrap", "ACS"),
                     selected = "BRR"),
        checkboxInput(ns("rep_combined"),
                       "Replicate weights are combined (final, not multipliers)",
                       value = TRUE),
        checkboxInput(ns("rep_mse"),
                       "Use MSE estimator (recommended)",
                       value = TRUE),
        tags$small(class = "text-muted", style = "display:block;",
          "Pattern matches column names. PHF uses `^wr_`; HFCN uses `^hw[0-9]{4}$`. ",
          "Apply a template to auto-fill.")
      )
    })

    # Multi-implicate badge
    implicate_info <- reactive({
      req(state$raw_data)
      tryCatch(detect_implicates(state$raw_data),
                error = function(e) list(kind = "none", n = 0))
    })

    output$implicate_badge <- renderUI({
      info <- implicate_info()
      if (is.null(info) || info$kind == "none" || info$n < 2) return(NULL)
      msg <- if (info$kind == "long")
        sprintf("MI detected: %d implicates in `%s` (long)", info$n, info$column)
      else sprintf("MI detected: %d implicates (wide pattern)", info$n)
      tags$div(class = "alert alert-info",
               style = "padding:6px 10px; margin-top:8px;",
               icon("layer-group"), tags$b(" "), msg,
               tags$br(),
               tags$small("Single-implicate descriptives appear on the Descriptives tab. ",
                           "For pooled estimates with FMI / lambda, see the ",
                           tags$b("MI Pooled"), " tab."))
    })

    # ---- Templates ------------------------------------------------
    # Holder for deferred replicate-weight settings; consumed by the
    # observer below once the replicate-weight renderUI has populated.
    pending_tpl_apply <- reactiveVal(NULL)
    .apply_template <- function(tpl_id) {
      tpl <- SURVEY_TEMPLATES[[tpl_id]]
      req(tpl)
      df <- state$raw_data
      if (is.null(df)) {
        flash("Load a dataset before applying a template.", "warning")
        return()
      }
      cn <- names(df)
      pick <- function(cands) .first_match(cands, cn)
      updateRadioButtons(session, "design_method",
                         selected = tpl$method %||% "standard")
      # delay slightly so dependent UI renders before update
      shiny::isolate({
        upd_select <- function(id, val) {
          if (!is.na(val) && nzchar(val))
            updateSelectInput(session, id, selected = val)
          else
            updateSelectInput(session, id, selected = "(none)")
        }
        upd_select("weight",  pick(tpl$weight))
        upd_select("strata",  pick(tpl$strata))
        upd_select("psu",     pick(tpl$psu))
        upd_select("id_col",  pick(tpl$id))
        upd_select("wave_col",pick(tpl$wave))
      })
      # Defer replicate-weight controls until renderUI rebuilds them.
      # Use a one-shot observer that fires after the next input flush.
      pending_tpl_apply(list(tpl = tpl, ts = Sys.time()))
      flash(sprintf("Template applied: %s", tpl$label), "message")
    }
    # Observer to apply pending replicate-weight settings once renderUI
    # has built the rep_pattern / rep_type controls.
    observe({
      pending <- pending_tpl_apply()
      if (is.null(pending)) return()
      tpl <- pending$tpl
      if (isTRUE(input$design_method == "replicate")) {
        tryCatch({
          updateTextInput(session, "rep_pattern",
                           value = tpl$repweights_pattern %||% "")
          updateSelectInput(session, "rep_type",
                             selected = tpl$repdesign_type %||% "BRR")
          updateCheckboxInput(session, "rep_combined",
                               value = isTRUE(tpl$combined_weights))
          updateCheckboxInput(session, "rep_mse",
                               value = isTRUE(tpl$use_mse))
        }, error = function(e) NULL)
        # Clear pending so we don't re-apply
        pending_tpl_apply(NULL)
      }
    })

    observeEvent(input$tpl_phf,  .apply_template("phf"))
    observeEvent(input$tpl_hfcn, .apply_template("hfcn"))

    # ---- Demo loader ----------------------------------------------
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

    # ---- Declare design ------------------------------------------
    observeEvent(input$declare, {
      req(state$raw_data)
      method <- input$design_method %||% "standard"
      d <- make_design(state$raw_data,
                        method = method,
                        weight_col = input$weight,
                        strata_col = input$strata,
                        psu_col    = input$psu,
                        repweights_pattern = if (method == "replicate")
                          input$rep_pattern else NULL,
                        repdesign_type = input$rep_type %||% "BRR",
                        combined_weights = isTRUE(input$rep_combined %||% TRUE),
                        use_mse = isTRUE(input$rep_mse %||% TRUE),
                        implicates = implicate_info())
      d$df <- state$raw_data

      # Apply selectable variance method on top of the base design (when
      # the user picked a non-Taylor method on a Standard design).
      vmethod <- input$variance_method %||% "taylor"
      reps    <- as.integer(input$var_replicates %||% 200L)
      brr_rho <- as.numeric(input$brr_rho %||% 0)
      if (!identical(vmethod, "taylor") && method == "standard" &&
          !is.null(d$design) &&
          requireNamespace("survey", quietly = TRUE)) {
        new_des <- tryCatch({
          args <- list(design = d$design, type = vmethod, replicates = reps)
          if (vmethod == "Fay") args$fay.rho <- brr_rho
          do.call(survey::as.svrepdesign, args)
        }, error = function(e) NULL)
        if (!is.null(new_des)) {
          d$design <- new_des
          d$kind <- "replicate"
          d$message <- sprintf(
            "%s · variance: %s (%d reps%s)", d$message, vmethod, reps,
            if (vmethod == "Fay") sprintf(", rho=%.2f", brr_rho) else "")
        }
      }

      design_state(d)
      state$survey_design <- list(
        method = method,
        weight = input$weight, strata = input$strata, psu = input$psu,
        id = input$id_col, wave = input$wave_col, kind = d$kind,
        repweights_pattern = if (method == "replicate")
          input$rep_pattern else NULL,
        repdesign_type = input$rep_type %||% NA_character_,
        combined_weights = isTRUE(input$rep_combined %||% TRUE),
        use_mse = isTRUE(input$rep_mse %||% TRUE),
        implicates = d$implicates,
        variance = list(method = vmethod, replicates = reps,
                         brr_rho = if (vmethod == "Fay") brr_rho else NA_real_)
      )
      tryCatch(db_save_survey_design(state$dataset_id,
                                       state$survey_design),
                error = function(e) NULL)
      flash(sprintf("Design declared (%s): %s", d$kind, d$message), "message")
    })

    output$design_status <- renderUI({
      d <- design_state()
      icon_name <- switch(d$kind,
        "standard"   = "circle-check",
        "replicate"  = "circle-check",
        "error"      = "triangle-exclamation",
        "circle-info")
      bg <- switch(d$kind,
        "standard"  = "alert-success",
        "replicate" = "alert-success",
        "error"     = "alert-danger",
        "alert-secondary")
      div(class = sprintf("alert %s", bg),
        icon(icon_name), tags$b(toupper(d$kind)), " - ", d$message)
    })

    # ---- Send to Model Lab ---------------------------------------
    observeEvent(input$send_modellab, {
      if (is.null(state$raw_data)) {
        flash("Load a dataset first.", "warning")
        return()
      }
      sd <- state$survey_design
      if (is.null(sd)) {
        flash("Declare a survey design first.", "warning")
        return()
      }
      df <- state$raw_data
      base_name <- state$dataset_name %||% "dataset"
      slot_name <- sprintf("%s (survey-prepped)", base_name)

      ds <- state$datasets %||% list()
      i <- length(ds) + 1L
      ds_id <- sprintf("ds_%d", i)
      while (ds_id %in% names(ds)) { i <- i + 1L; ds_id <- sprintf("ds_%d", i) }

      # Build a meta block carrying survey-related hints into Model Lab
      cur_meta <- state$meta %||% list()
      sample_w  <- if (!is.null(sd$weight) && sd$weight != "(none)")
                      sd$weight else NA_character_
      strata_v  <- if (!is.null(sd$strata) && sd$strata != "(none)")
                      sd$strata else NA_character_
      psu_v     <- if (!is.null(sd$psu)    && sd$psu    != "(none)")
                      sd$psu    else NA_character_
      rep_cols  <- if (!is.null(sd$repweights_pattern) &&
                       nzchar(sd$repweights_pattern))
                      grep(sd$repweights_pattern, names(df), value = TRUE)
                    else character(0)

      new_meta <- modifyList(cur_meta, list(
        sample_weights    = sample_w,
        strata            = strata_v,
        psu               = psu_v,
        replicate_weights = rep_cols,
        survey_method     = sd$method %||% "standard"
      ))

      labels <- tryCatch(extract_labels(df), error = function(e) list())
      hints  <- tryCatch(detect_survey_columns(df, labels),
                          error = function(e) list())
      ds[[ds_id]] <- list(
        name         = slot_name,
        data         = df,
        labels       = labels,
        survey_hints = hints,
        meta         = new_meta,
        format       = "survey-prepped",
        loaded_at    = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        n_rows       = nrow(df),
        n_cols       = ncol(df)
      )
      state$datasets  <- ds
      state$active_id <- ds_id
      state$meta      <- new_meta
      # Make a starter prepped frame so Model Lab is "ready" — the user
      # can re-run Data Prep to refine.
      state$prepped <- df

      if (!is.null(parent_session))
        bslib::nav_select("main_nav", "6 · Model Lab",
                          session = parent_session)

      flash(sprintf("Sent '%s' to Model Lab. Weights: %s",
                    slot_name,
                    if (is.na(sample_w)) "(none)" else sample_w),
            "message")
    })

    # ---- Hero header ----------------------------------------------
    output$hero_panel <- renderUI({
      req(state$raw_data)
      d <- design_state()
      grade_col <- if (d$kind %in% c("standard","replicate"))
                     "#3fb950" else "#8b949e"
      div(class = "studio-intro", style = "margin-top:12px;",
        div(class = "studio-page",
          div(class = "studio-hero",
            div(class = "studio-hero-meta",
              tags$span(class = "studio-kicker", "SURVEY & PANEL"),
              tags$span(class = "studio-dot", "•"),
              tags$span(class = "studio-grade",
                style = sprintf("color:%s;border-color:%s;", grade_col, grade_col),
                toupper(d$kind))),
            tags$h1(class = "studio-headline",
              tagList(ifelse(is.null(state$dataset_name),
                                "Untitled survey", state$dataset_name),
                       doc_chip("survey", "Survey & Panel"))),
            tags$p(class = "studio-deck",
              sprintf("%s rows · %s columns · %s",
                       formatC(nrow(state$raw_data), format = "d", big.mark = ","),
                       ncol(state$raw_data),
                       d$message)),
            div(class = "studio-rule"),
            tags$div(style = "margin-top:10px; display:flex; gap:8px; flex-wrap:wrap;",
              actionButton(ns("save_project"), "Save Project",
                            class = "btn-outline-warning btn-sm",
                            icon = icon("floppy-disk")),
              downloadButton(ns("export_brief_html"), "Brief Report (HTML)",
                              class = "btn-outline-info btn-sm")
            ))))
    })

    # ---- Save project ----------------------------------------------
    observeEvent(input$save_project, {
      bundle <- tryCatch(project_save(state),
                          error = function(e) {
                            flash(paste("Save failed:",
                                          conditionMessage(e)), "error"); NULL })
      if (!is.null(bundle)) {
        state$current_project   <- basename(bundle)
        state$last_project_save <- bundle
        flash(sprintf("Project saved to %s", bundle), "message")
      }
    })
    output$export_brief_html <- downloadHandler(
      filename = function()
        sprintf("survey_report_%s.html",
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

    # ---- Codebook (rich, searchable, label-aware) ------------------
    codebook_server("cb", state)

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
        # For replicate designs survey::svytable / svychisq still work
        # because svrepdesign carries the same dispatch.
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

    # ---- MI Pooled (Rubin's rules) --------------------------------
    output$mi_ui <- renderUI({
      req(state$raw_data)
      info <- implicate_info()
      if (is.null(info) || info$kind == "none" || info$n < 2) {
        return(tags$div(class = "alert alert-secondary",
          icon("circle-info"), tags$b(" "),
          "No multi-implicate structure detected. ",
          "Pooling via Rubin's rules requires >= 2 implicates."))
      }
      if (!requireNamespace("mitools", quietly = TRUE) ||
          !requireNamespace("survey", quietly = TRUE)) {
        return(tags$div(class = "alert alert-warning",
          icon("triangle-exclamation"),
          " Install 'mitools' and 'survey' for pooled estimation: ",
          tags$code("install.packages(c('mitools','survey'))")))
      }
      cols <- names(state$raw_data)
      tagList(
        tags$div(class = "studio-kicker", "POOLED ESTIMATION"),
        tags$small(class = "text-muted", style = "display:block;margin-bottom:8px;",
          sprintf("MI detected: %d implicates (%s).", info$n, info$kind),
          " Estimates are combined via mitools::MIcombine."),
        radioButtons(ns("mi_estimator"), "Estimator", inline = TRUE,
                      choices = c("Mean (svymean)" = "svymean",
                                  "Quantile (svyquantile)" = "svyquantile",
                                  "Ratio (svyratio)" = "svyratio",
                                  "Linear regression (svyglm)" = "svyglm_lm",
                                  "Logistic regression (svyglm)" = "svyglm_logit"),
                      selected = "svymean"),
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == 'svymean' || input['%s'] == 'svyquantile'",
            ns("mi_estimator"), ns("mi_estimator")),
          selectInput(ns("mi_var"), "Variable", cols, selected = cols[1])
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'svyratio'", ns("mi_estimator")),
          selectInput(ns("mi_num"),  "Numerator",   cols, selected = cols[1]),
          selectInput(ns("mi_den"),  "Denominator", cols,
                       selected = utils::tail(cols, 1))
        ),
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == 'svyglm_lm' || input['%s'] == 'svyglm_logit'",
            ns("mi_estimator"), ns("mi_estimator")),
          textInput(ns("mi_formula"), "Formula (R syntax)",
                     value = "", placeholder = "y ~ x1 + x2"),
          tags$small(class = "text-muted", style = "display:block;",
            "Two-sided formula. Logistic expects a 0/1 outcome.")
        ),
        actionButton(ns("mi_run"), "Pool estimate",
                      class = "btn-primary", icon = icon("layer-group")),
        tags$hr(class = "studio-rule"),
        tags$div(class = "studio-kicker", "POOLED RESULTS"),
        DT::DTOutput(ns("mi_result")),
        tags$hr(class = "studio-rule"),
        tags$div(class = "studio-kicker", "DIAGNOSTICS (FMI / lambda)"),
        verbatimTextOutput(ns("mi_diag"))
      )
    })

    # Build implicate list using current detection + survey-design hint.
    .build_implicates <- function() {
      info <- implicate_info()
      if (is.null(info) || info$kind == "none") return(list())
      hint <- if (info$kind == "long")
        list(kind = "long", column = info$column)
      else if (info$kind == "wide")
        list(kind = "wide", pattern = info$pattern)
      else NULL
      mi_detect(state$raw_data, hint = hint)
    }

    mi_combined <- reactiveVal(NULL)
    observeEvent(input$mi_run, {
      implicates <- tryCatch(.build_implicates(),
                              error = function(e) list())
      if (length(implicates) < 2) {
        flash("Could not split data into >= 2 implicates.", "warning")
        return()
      }
      sd <- state$survey_design %||% list()
      sd$variance_method <- (sd$variance %||% list())$method
      sd$replicates      <- (sd$variance %||% list())$replicates
      sd$brr_rho         <- (sd$variance %||% list())$brr_rho

      designs <- tryCatch(mi_designs(implicates, sd),
                          error = function(e) {
                            flash(paste("Design build failed:",
                                          conditionMessage(e)), "error")
                            NULL })
      req(designs)

      est <- input$mi_estimator
      out <- tryCatch({
        if (est == "svymean")
          mi_estimate(designs, formula = stats::as.formula(
            paste0("~", input$mi_var)), model = "svymean")
        else if (est == "svyquantile")
          mi_estimate(designs, formula = stats::as.formula(
            paste0("~", input$mi_var)), model = "svyquantile",
            quantile_probs = 0.5)
        else if (est == "svyratio")
          mi_estimate(designs,
            numerator = stats::as.formula(paste0("~", input$mi_num)),
            denominator = stats::as.formula(paste0("~", input$mi_den)),
            model = "svyratio")
        else if (est == "svyglm_lm")
          mi_estimate(designs,
            formula = stats::as.formula(input$mi_formula %||% ""),
            family = stats::gaussian, model = "svyglm")
        else if (est == "svyglm_logit")
          mi_estimate(designs,
            formula = stats::as.formula(input$mi_formula %||% ""),
            family = stats::quasibinomial, model = "svyglm")
      }, error = function(e) {
        flash(paste("Pooling failed:", conditionMessage(e)), "error")
        NULL
      })
      mi_combined(out)
      if (!is.null(out))
        flash(sprintf("Pooled %d implicates (%s).",
                      attr(out, "n_implicates") %||% length(implicates),
                      est), "message")
    })

    output$mi_result <- DT::renderDT({
      r <- mi_combined()
      if (is.null(r)) return(DT::datatable(
        data.frame(message = "Pool an estimate to populate this panel.")))
      view <- r
      view$estimate  <- round(view$estimate,  6)
      view$std.error <- round(view$std.error, 6)
      view$statistic <- round(view$statistic, 4)
      view$p.value   <- round(view$p.value,   6)
      view$df        <- round(view$df, 2)
      view$fmi       <- round(view$fmi, 4)
      view$riv       <- round(view$riv, 4)
      view$lambda    <- round(view$lambda, 4)
      DT::datatable(view, options = list(dom = "tip", pageLength = 15,
                                          scrollX = TRUE),
                     rownames = FALSE, class = "compact stripe")
    })

    output$mi_diag <- renderText({
      r <- mi_combined()
      if (is.null(r)) return("Pool an estimate to populate diagnostics.")
      d <- mi_diagnostics(r)
      paste0(
        sprintf("Implicates pooled: %s\n", d$n_implicates %||% "?"),
        sprintf("Model: %s\n", d$model %||% "?"),
        sprintf("Avg FMI: %s\n",
                format(round(d$avg_fmi, 4), nsmall = 4)),
        sprintf("Max FMI: %s\n",
                format(round(d$max_fmi, 4), nsmall = 4)),
        sprintf("Avg lambda: %s\n",
                format(round(d$avg_lambda, 4), nsmall = 4)),
        "\nFMI = fraction of missing information; ",
        "lambda = (1+1/m)*B / T; ",
        "high values indicate sensitivity to imputation choice."
      )
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
        "Balanced: ", ps$pct_balanced, "% · Attrition: ", ps$pct_attrition, "%\n\n",
        "Wave sizes:\n",
        paste(sprintf("  %s : %d", ps$waves$wave, ps$waves$n), collapse = "\n")
      )
    })
  })
}
