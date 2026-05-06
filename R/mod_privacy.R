# =====================================================================
# Module: Privacy Audit
#   * Scans for PII / quasi-identifiers / sensitive variables
#   * Computes k-anonymity & re-identification risk
#   * Master AI-egress consent gate (state$privacy_allow_ai)
#   * Pseudonymizer / column suppressor
#   * Append-only audit log (privacy_log table)
#   Pure local R - nothing leaves the machine.
# =====================================================================

# ---- Detection rules -------------------------------------------------
PRIVACY_PATTERNS <- list(
  email   = "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
  phone   = "^[+]?[0-9 ()-]{7,}$",
  iban    = "^[A-Z]{2}[0-9]{2}[A-Z0-9]{11,30}$",
  ipv4    = "^([0-9]{1,3}\\.){3}[0-9]{1,3}$",
  postcode_de = "^[0-9]{5}$",
  postcode_us = "^[0-9]{5}(-[0-9]{4})?$",
  credit_card = "^[0-9 -]{13,19}$"
)

# Names of columns that often hold sensitive attributes (EN/DE)
SENSITIVE_KEYWORDS <- c(
  "income","salary","wage","gehalt","einkommen","networth","wealth",
  "religion","ethnicity","race","nationality","staatsang",
  "sexuality","sexual","gender_id","political","wahl","party",
  "health","krankheit","diagnos","disability","behind",
  "criminal","conviction","arrest","strafe",
  "biometric","genetic"
)

NAME_KEYWORDS <- c("first_?name","last_?name","surname","vorname","nachname",
                    "fullname","full_?name","name$")
ID_KEYWORDS   <- c("ssn","tax_?id","passport","license","ausweis",
                    "personal_?id","persnr","caseid","respondent")
ADDR_KEYWORDS <- c("address","street","stra(s|\u00df)e","city","stadt","plz",
                    "zip","postal","postcode")

# Quasi-identifier candidates (combined risk)
QUASI_ID_KEYWORDS <- c("age","sex","gender","geschlecht","plz","zip","postal",
                        "birth","geburt","occupation","beruf","education",
                        "bildung","income_range","ethnicity")

# ---- Scanners --------------------------------------------------------
.detect_regex_hits <- function(x, pattern) {
  if (!is.character(x) && !is.factor(x)) return(0)
  v <- as.character(stats::na.omit(x))
  if (length(v) == 0) return(0)
  v <- utils::head(v, 500)        # sample - keep it cheap
  sum(grepl(pattern, v, perl = TRUE)) / length(v)
}

.matches_any <- function(name, label, kws) {
  hay <- tolower(paste(name, label %||% ""))
  any(vapply(kws, function(k) grepl(k, hay, perl = TRUE), logical(1)))
}

# Returns a data.frame of findings per column with severity + tag.
privacy_scan <- function(df, labels = list()) {
  rows <- list()
  for (nm in names(df)) {
    x <- df[[nm]]
    lab <- labels$var_labels[[nm]]
    tags <- character(0)
    severity <- "OK"

    # Name-based heuristics
    if (.matches_any(nm, lab, NAME_KEYWORDS))
      { tags <- c(tags, "name");        severity <- "CRITICAL" }
    if (.matches_any(nm, lab, ID_KEYWORDS))
      { tags <- c(tags, "id_doc");      severity <- "CRITICAL" }
    if (.matches_any(nm, lab, ADDR_KEYWORDS))
      { tags <- c(tags, "address");     severity <- "WARN" }
    if (.matches_any(nm, lab, SENSITIVE_KEYWORDS))
      { tags <- c(tags, "sensitive");
        severity <- ifelse(severity == "OK", "WARN", severity) }
    if (.matches_any(nm, lab, QUASI_ID_KEYWORDS))
      { tags <- c(tags, "quasi_id");
        severity <- ifelse(severity == "OK", "WARN", severity) }

    # Pattern-based (string contents)
    for (pat_name in names(PRIVACY_PATTERNS)) {
      hit_rate <- .detect_regex_hits(x, PRIVACY_PATTERNS[[pat_name]])
      if (hit_rate > 0.3) {
        tags <- c(tags, pat_name)
        severity <- "CRITICAL"
      }
    }

    if (length(tags) == 0) { tags <- "ok" }
    rows[[length(rows)+1]] <- data.frame(
      column = nm,
      label = lab %||% "",
      severity = severity,
      tags = paste(unique(tags), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

# ---- k-anonymity & re-identification --------------------------------
quasi_identifiers <- function(scan_df) {
  scan_df$column[grepl("quasi_id|address|name", scan_df$tags)]
}

k_anonymity <- function(df, qi_cols) {
  qi_cols <- intersect(qi_cols, names(df))
  if (length(qi_cols) == 0)
    return(list(k = NA_integer_, n_unique_groups = 0,
                pct_unique = NA_real_, pct_at_risk = NA_real_,
                qi_cols = character(0)))
  # Group counts on quasi-identifier combinations
  key <- do.call(paste, c(df[, qi_cols, drop = FALSE], sep = "\u241F"))
  tab <- table(key)
  n <- length(key)
  list(
    k = as.integer(min(tab)),
    n_unique_groups = sum(tab == 1),
    pct_unique = round(100 * sum(tab == 1) / n, 2),
    pct_at_risk = round(100 * sum(tab[tab < 5]) / n, 2),
    qi_cols = qi_cols
  )
}

# ---- Pseudonymizer ---------------------------------------------------
pseudonymize_df <- function(df, scan_df, salt = "shiny-ml") {
  req_pkg("digest")
  out <- df
  to_drop  <- scan_df$column[scan_df$severity == "CRITICAL" &
                              !grepl("id_doc", scan_df$tags)]
  to_hash  <- scan_df$column[grepl("id_doc", scan_df$tags)]
  for (c in intersect(to_hash, names(out))) {
    out[[c]] <- vapply(as.character(out[[c]]),
                        function(v) substr(digest::digest(paste0(salt, v),
                                                          algo = "sha256"), 1, 12),
                        character(1))
  }
  for (c in intersect(to_drop, names(out))) out[[c]] <- NULL
  list(df = out, dropped = to_drop, hashed = to_hash)
}

# ---- Audit log -------------------------------------------------------
privacy_log_event <- function(state, action, scope = "") {
  tryCatch({
    db_log_privacy(state$dataset_id %||% NA_integer_,
                    action, scope)
  }, error = function(e) NULL)
}

# ---- UI / Server -----------------------------------------------------
privacy_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header(tagList(icon("shield-halved"), "Privacy Audit")),
      tags$p(class = "text-muted",
        "Local-only scan. Nothing leaves this machine unless you explicitly ",
        "enable AI egress below."),
      div(class = "alert alert-success", style = "padding:8px 12px;",
        tags$b(textOutput(ns("local_label"), inline = TRUE))),
      hr(),
      tags$h6("AI egress gate"),
      checkboxInput(ns("allow_ai"), "Allow AI calls", TRUE),
      tags$small(class = "text-muted",
        "When OFF: AI verdict, AI prep suggestions, and any future LLM ",
        "feature are blocked. No data sent anywhere."),
      hr(),
      actionButton(ns("scan"), "Run privacy scan", class = "btn-primary w-100",
                    icon = icon("magnifying-glass")),
      hr(),
      tags$h6("Anonymizer"),
      actionButton(ns("anonymize"), "Pseudonymize critical PII",
                    class = "btn-warning w-100", icon = icon("user-secret")),
      tags$small(class = "text-muted", style = "display:block; margin-top:6px;",
        "Hashes ID-like cols, drops names/addresses. Result is sent to ",
        "Model Lab as state$prepped.")
    ),
    div(
      uiOutput(ns("editorial_panel")),
      hr(),
      navset_card_underline(
        title = "Detail",
        nav_panel("Findings",   DT::DTOutput(ns("findings"))),
        nav_panel("k-anonymity", verbatimTextOutput(ns("k_summary"))),
        nav_panel("Audit log",  DT::DTOutput(ns("log_table"))),
        nav_panel("Capabilities", uiOutput(ns("caps_panel")))
      )
    )
  )
}

privacy_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Default privacy gate is ON (allow AI)
    observe({ state$privacy_allow_ai <- isTRUE(input$allow_ai %||% TRUE) })

    output$local_label <- renderText({
      sprintf("Running locally - working dir: %s",
              tryCatch(.app_root(), error = function(e) getwd()))
    })

    scan_df <- reactiveVal(data.frame())

    observeEvent(input$scan, {
      req(state$raw_data)
      withProgress(message = "Scanning columns for PII / quasi-identifiers\u2026",{
        s <- privacy_scan(state$raw_data, state$labels %||% list())
        scan_df(s)
        privacy_log_event(state, "privacy_scan",
                           sprintf("rows=%d cols=%d critical=%d warn=%d",
                                   nrow(state$raw_data), ncol(state$raw_data),
                                   sum(s$severity == "CRITICAL"),
                                   sum(s$severity == "WARN")))
      })
    })

    output$findings <- DT::renderDT({
      d <- scan_df()
      if (nrow(d) == 0)
        return(DT::datatable(data.frame(message = "Click 'Run privacy scan'"),
                              options = list(dom = "t"), rownames = FALSE))
      DT::datatable(d, filter = "top",
        options = list(pageLength = 15, dom = "tip"),
        rownames = FALSE, class = "compact stripe") |>
        DT::formatStyle("severity", backgroundColor = DT::styleEqual(
          c("CRITICAL","WARN","OK"),
          c("#3a1d1d","#3a311d","#1d3a26")))
    })

    output$k_summary <- renderText({
      d <- scan_df()
      req(state$raw_data)
      qi <- if (nrow(d) > 0) quasi_identifiers(d) else character(0)
      if (length(qi) == 0) return(
        "No quasi-identifier columns detected. Run privacy scan first.")
      k <- k_anonymity(state$raw_data, qi)
      paste0(
        "Quasi-identifier columns: ", paste(k$qi_cols, collapse = ", "), "\n",
        "Minimum group size (k): ", k$k, "\n",
        "Unique-row groups (k=1): ", k$n_unique_groups, "\n",
        "Percent of rows uniquely identifiable: ", k$pct_unique, "%\n",
        "Percent of rows in groups smaller than 5: ", k$pct_at_risk, "%"
      )
    })

    output$log_table <- DT::renderDT({
      logs <- tryCatch(db_get_privacy_log(), error = function(e) data.frame())
      if (nrow(logs) == 0)
        return(DT::datatable(data.frame(message = "No events logged"),
                              options = list(dom = "t"), rownames = FALSE))
      DT::datatable(logs, options = list(pageLength = 15, dom = "tip"),
                     rownames = FALSE, class = "compact stripe")
    })

    output$caps_panel <- renderUI({
      tagList(
        tags$h6("Outbound capabilities"),
        tags$ul(
          tags$li(tags$b("AI verdict / AI prep "),
                  tags$span(class = "badge bg-warning",
                            ifelse(isTRUE(state$privacy_allow_ai), "ENABLED", "DISABLED"))),
          tags$li(tags$b("Local DB writes "),
                  tags$span(class = "badge bg-success", "ALWAYS LOCAL")),
          tags$li(tags$b("Telemetry / analytics "),
                  tags$span(class = "badge bg-secondary", "NONE"))
        ),
        tags$small(class = "text-muted",
          "When AI egress is enabled, only summary statistics (column names, ",
          "metric values, residual aggregates) are sent. Raw rows are never ",
          "transmitted.")
      )
    })

    # ---- Anonymize button ---------------------------------------
    observeEvent(input$anonymize, {
      req(state$raw_data)
      d <- scan_df()
      if (nrow(d) == 0) {
        flash("Run the privacy scan first.", "warning"); return()
      }
      out <- pseudonymize_df(state$raw_data, d)
      state$prepped <- out$df
      state$prep_log <- list(
        sprintf("anonymize -> dropped %d, hashed %d",
                length(out$dropped), length(out$hashed)))
      privacy_log_event(state, "anonymize",
                         sprintf("dropped=%d hashed=%d",
                                 length(out$dropped), length(out$hashed)))
      flash(sprintf("Pseudonymized: dropped %d, hashed %d cols",
                     length(out$dropped), length(out$hashed)), "message")
    })

    # ---- Editorial panel ---------------------------------------
    output$editorial_panel <- renderUI({
      d <- scan_df()
      n_crit <- if (nrow(d) > 0) sum(d$severity == "CRITICAL") else 0
      n_warn <- if (nrow(d) > 0) sum(d$severity == "WARN") else 0
      grade <- if (n_crit > 0) "FAIL"
               else if (n_warn > 0) "WEAK"
               else if (nrow(d) == 0) "-"
               else "PASS"
      grade_col <- switch(grade, "PASS"="#3fb950","WEAK"="#d4af37","FAIL"="#d9534f","#8b949e")
      div(class = "studio-intro", style = "margin-top:12px;",
        div(class = "studio-page",
          div(class = "studio-hero",
            div(class = "studio-hero-meta",
              tags$span(class = "studio-kicker", "PRIVACY AUDIT"),
              tags$span(class = "studio-dot", "\u2022"),
              tags$span(class = "studio-grade",
                style = sprintf("color:%s;border-color:%s;", grade_col, grade_col),
                grade)),
            tags$h1(class = "studio-headline",
                    tagList(
                      if (nrow(d) == 0) "No scan run yet"
                      else sprintf("%d findings", nrow(d)),
                      doc_chip("privacy", "Privacy Audit"))),
            tags$p(class = "studio-deck",
              if (nrow(d) == 0) "Click 'Run privacy scan' to evaluate this dataset."
              else sprintf("%d critical \u00b7 %d warning \u00b7 AI egress %s",
                            n_crit, n_warn,
                            ifelse(isTRUE(state$privacy_allow_ai), "ON", "OFF"))),
            div(class = "studio-rule"))))
    })
  })
}
