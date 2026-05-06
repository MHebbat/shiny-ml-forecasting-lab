# =====================================================================
# Brief report renderer
#
# Produces a 2-3 page report in HTML (always) or PDF (best effort) from
# the current Lab state. The PDF route prefers
#   pagedown::chrome_print(); falls back to rmarkdown + pandoc; if
# neither are available, the HTML is the only output and a notice is
# returned.
#
# Author: Malik Hebbat
# =====================================================================

# Minimal markdown -> HTML (paragraphs, bold, italic, code, lists). We
# avoid pulling in `markdown` to keep the report renderer dependency-light.
.report_md_to_html <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return("")
  s <- as.character(txt)
  s <- htmltools::htmlEscape(s)
  s <- gsub("`([^`]+)`", "<code>\\1</code>", s)
  s <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", s, perl = TRUE)
  s <- gsub("\\*([^*]+)\\*", "<em>\\1</em>", s, perl = TRUE)
  paras <- strsplit(s, "\n\n+", perl = TRUE)[[1]]
  paste(vapply(paras, function(p) {
    if (grepl("^\\s*[-*] ", p)) {
      items <- strsplit(p, "\n", fixed = TRUE)[[1]]
      items <- sub("^\\s*[-*] ", "", items)
      paste0("<ul>",
             paste(sprintf("<li>%s</li>", items), collapse = ""),
             "</ul>")
    } else {
      paste0("<p>", gsub("\n", "<br>", p, fixed = TRUE), "</p>")
    }
  }, character(1)), collapse = "\n")
}

# Build the body HTML from state + manifest. theme picks the chrome.
.report_body_html <- function(state, manifest, theme = c("bundesbank","light","dark")) {
  theme <- match.arg(theme)
  ds   <- manifest$dataset      %||% list()
  rec  <- manifest$recipe$log   %||% list()
  sd   <- manifest$survey_design %||% list()
  mod  <- manifest$model        %||% list()
  vald <- manifest$validation   %||% list()
  met  <- manifest$metrics$test %||% list()
  boot <- manifest$metrics$bootstrap_ci_95 %||% list()
  sw   <- manifest$software     %||% list()

  fmt <- function(x, d = 4) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) return("—")
    if (is.numeric(x)) formatC(x, format = "f", digits = d) else as.character(x)
  }

  # Sections
  hdr <- sprintf(
    "<div class='hdr'><div class='hdr-kicker'>BRIEF REPORT</div>
     <h1 class='hdr-title'>%s</h1>
     <p class='hdr-deck'>Generated %s &middot; commit %s</p></div>",
    htmltools::htmlEscape(ds$name %||% "Untitled run"),
    htmltools::htmlEscape(manifest$app$generated_at %||% ""),
    htmltools::htmlEscape(manifest$git_commit %||% "n/a"))

  ds_html <- sprintf(
    "<h2>1. Dataset</h2>
     <ul><li><b>Name:</b> %s</li>
         <li><b>Rows × Cols:</b> %s × %s</li>
         <li><b>SHA-256:</b> <code>%s</code></li>
         <li><b>Source format:</b> %s</li></ul>",
    htmltools::htmlEscape(ds$name %||% "—"),
    fmt(ds$n_rows, 0), fmt(ds$n_cols, 0),
    htmltools::htmlEscape(ds$sha256 %||% "—"),
    htmltools::htmlEscape(ds$source %||% "—"))

  rec_lines <- if (length(rec) == 0) "<li>(no recipe steps applied)</li>"
               else paste(sprintf("<li>%s</li>",
                                    htmltools::htmlEscape(unlist(rec))),
                          collapse = "")
  rec_html <- sprintf("<h2>2. Preprocessing recipe</h2><ul>%s</ul>", rec_lines)

  sd_html <- if (is.null(sd) || length(sd) == 0)
    "<h2>3. Survey design</h2><p>(no survey design declared)</p>"
  else {
    sd_items <- paste0(
      sprintf("<li><b>Method:</b> %s</li>",
              htmltools::htmlEscape(sd$method %||% sd$kind %||% "standard")),
      sprintf("<li><b>Weights:</b> %s</li>",
              htmltools::htmlEscape(sd$weight %||% "(none)")),
      sprintf("<li><b>Strata:</b> %s</li>",
              htmltools::htmlEscape(sd$strata %||% "(none)")),
      sprintf("<li><b>PSU:</b> %s</li>",
              htmltools::htmlEscape(sd$psu %||% "(none)")),
      if (!is.null(sd$repweights_pattern) && nzchar(sd$repweights_pattern))
        sprintf("<li><b>Replicate weights pattern:</b> <code>%s</code> (%s)</li>",
                htmltools::htmlEscape(sd$repweights_pattern),
                htmltools::htmlEscape(sd$repdesign_type %||% "?")) else "",
      if (!is.null(sd$implicates$n) && sd$implicates$n >= 2)
        sprintf("<li><b>Implicates (MI):</b> %d (%s)</li>",
                as.integer(sd$implicates$n),
                htmltools::htmlEscape(sd$implicates$kind %||% "?")) else ""
    )
    sprintf("<h2>3. Survey design</h2><ul>%s</ul>", sd_items)
  }

  mod_html <- sprintf(
    "<h2>4. Model</h2>
     <ul><li><b>Id:</b> %s</li>
         <li><b>Label:</b> %s</li>
         <li><b>Backend:</b> %s</li></ul>
     <p><b>Hyperparameters.</b></p><pre>%s</pre>",
    htmltools::htmlEscape(mod$id %||% "—"),
    htmltools::htmlEscape(mod$label %||% "—"),
    htmltools::htmlEscape(mod$available_backend %||% "—"),
    htmltools::htmlEscape(jsonlite::toJSON(mod$params %||% list(),
                                              auto_unbox = TRUE,
                                              pretty = TRUE)))

  val_html <- sprintf(
    "<h2>5. Validation</h2>
     <ul><li><b>Strategy:</b> %s</li>
         <li><b>Seed:</b> %s</li></ul>",
    htmltools::htmlEscape(vald$strategy %||% "—"),
    fmt(vald$seed, 0))

  met_rows <- if (length(met) == 0) "<tr><td colspan='2'>(no metrics)</td></tr>"
              else paste(vapply(names(met), function(k) sprintf(
                "<tr><td>%s</td><td>%s</td></tr>",
                htmltools::htmlEscape(toupper(k)),
                fmt(met[[k]], 4)), character(1)), collapse = "")
  ci_block <- if (length(boot) == 0) "" else sprintf(
    "<p><b>Bootstrap 95%% CI</b> for %s: [%s, %s] (R=%s, n=%s)</p>",
    htmltools::htmlEscape(boot$metric %||% "metric"),
    fmt(boot$lo, 4), fmt(boot$hi, 4),
    fmt(boot$R, 0), fmt(boot$n, 0))
  met_html <- sprintf(
    "<h2>6. Headline metrics</h2>
     <table class='kv'><tr><th>Metric</th><th>Value</th></tr>%s</table>%s",
    met_rows, ci_block)

  diag_html <- if (is.null(state$last_model$fit$diagnostics)) "" else {
    diag <- state$last_model$fit$diagnostics
    items <- character(0)
    if (!is.null(diag$backend))
      items <- c(items, sprintf("<li>Backend: <code>%s</code></li>",
                                htmltools::htmlEscape(diag$backend)))
    if (!is.null(diag$epochs_run))
      items <- c(items, sprintf("<li>Epochs run: %s</li>",
                                fmt(diag$epochs_run, 0)))
    if (!is.null(diag$final_loss))
      items <- c(items, sprintf("<li>Final loss: %s</li>",
                                fmt(diag$final_loss, 6)))
    if (length(items) == 0) ""
    else sprintf("<h2>7. Residual diagnostics</h2><ul>%s</ul>",
                  paste(items, collapse = ""))
  }

  meth_html <- "<h2>8. Methods</h2><p>The model was fitted using the recipe in section 2 on the training rows; the holdout split is described in section 5. Metrics in section 6 are out-of-sample point estimates with bootstrap CI (where applicable) computed by resampling the holdout predictions. Survey weights, when declared, are passed via the model's native <code>weights</code> argument; replicate weights are honoured for descriptive statistics but not yet pooled at fit time.</p>"
  lim_html  <- "<h2>9. Limitations</h2><p>Metrics describe out-of-sample performance under the named validation strategy. They do not guarantee generalisation to data with materially different sampling design or distribution. Confidence intervals are sensitive to sample size. Time-series forecasts assume stationarity over the forecast horizon.</p>"
  ref_html  <- "<h2>10. References</h2><p>Lumley (2010) <i>Complex Surveys</i>; Rubin (1987) <i>Multiple Imputation for Nonresponse in Surveys</i>; James, Witten, Hastie & Tibshirani (2013) <i>An Introduction to Statistical Learning</i>; Hyndman & Athanasopoulos (2021) <i>Forecasting: Principles and Practice</i>.</p>"

  sw_html <- sprintf(
    "<h2>11. Software</h2><p>%s &middot; %s</p>",
    htmltools::htmlEscape(sw$R %||% R.version.string),
    htmltools::htmlEscape(sw$os %||% "—"))

  # ---- Codebook appendix (only for labelled / haven imports) -------
  cb_html <- ""
  fmt <- {
    aid <- state$active_id
    if (!is.null(aid) && !is.null(state$datasets[[aid]]))
      state$datasets[[aid]]$format
    else NULL
  }
  is_haven_src <- !is.null(fmt) &&
    grepl("^(dta|sav|sas7bdat|labelled)$", as.character(fmt),
          ignore.case = TRUE)
  if (is_haven_src && !is.null(state$raw_data) &&
      exists("build_codebook_rich", mode = "function")) {
    cb_rows <- tryCatch(
      build_codebook_rich(state$raw_data, state$labels %||% list()),
      error = function(e) NULL)
    if (!is.null(cb_rows) && nrow(cb_rows) > 0) {
      header_cols <- c("name","type","label","pct_missing","n_unique",
                        "n_value_labels","summary")
      header_cols <- intersect(header_cols, names(cb_rows))
      hdr <- paste(sprintf("<th>%s</th>",
                            htmltools::htmlEscape(header_cols)),
                    collapse = "")
      body <- paste(vapply(seq_len(nrow(cb_rows)), function(i) {
        cells <- vapply(header_cols, function(cn) {
          v <- cb_rows[[cn]][i]
          if (is.list(v)) v <- paste(unlist(v), collapse = ", ")
          v <- as.character(v)
          if (is.na(v) || !nzchar(v)) "—" else htmltools::htmlEscape(v)
        }, character(1))
        sprintf("<tr>%s</tr>",
                paste(sprintf("<td>%s</td>", cells), collapse = ""))
      }, character(1)), collapse = "")
      cb_html <- sprintf(
        "<h2>12. Codebook (appendix)</h2>
         <p class='muted'>Active dataset is in a labelled format (%s) — the codebook is included as a reproducibility aid.</p>
         <table class='kv'><thead><tr>%s</tr></thead><tbody>%s</tbody></table>",
        htmltools::htmlEscape(as.character(fmt)),
        hdr, body)
    }
  }

  manifest_hash <- if (!is.null(ds$sha256) && nzchar(ds$sha256))
    substr(ds$sha256, 1, 12) else "n/a"
  foot_html <- sprintf(
    "<div class='foot'>Manifest hash: <code>%s</code> &middot; Author: Malik Hebbat &middot; Pure R Shiny, fully local.</div>",
    htmltools::htmlEscape(manifest_hash))

  paste(hdr, ds_html, rec_html, sd_html, mod_html, val_html,
         met_html, diag_html, meth_html, lim_html, ref_html, sw_html,
         cb_html, foot_html, sep = "\n")
}

# Theme-aware skeleton CSS.
.report_skeleton_css <- function(theme) {
  if (theme == "bundesbank") {
    paste0(
      "body{background:#FAFAF7;color:#1A1A1A;font-family:Inter,sans-serif;",
      "max-width:780px;margin:24px auto;padding:0 24px;line-height:1.55;}",
      "h1{font-family:'Source Serif 4','PT Serif',Georgia,serif;color:#003D7C;font-size:30px;margin:.2em 0;}",
      "h2{font-family:'Source Serif 4','PT Serif',Georgia,serif;color:#003D7C;border-bottom:1px solid #D9D9D9;padding-bottom:4px;margin-top:32px;}",
      ".hdr-kicker{font-size:11px;letter-spacing:.22em;color:#5C5C5C;text-transform:uppercase;}",
      ".hdr-deck{color:#5C5C5C;font-size:13px;}",
      "code{background:#F0F0F0;padding:1px 4px;border-radius:2px;}",
      "pre{background:#F4F4F0;border:1px solid #D9D9D9;padding:10px;overflow:auto;}",
      "table.kv{border-collapse:collapse;margin:6px 0;}",
      "table.kv th,table.kv td{border:1px solid #D9D9D9;padding:4px 10px;}",
      "table.kv th{background:#F0F4FA;color:#003D7C;text-align:left;}",
      ".foot{margin-top:36px;padding-top:8px;border-top:1px solid #D9D9D9;color:#5C5C5C;font-size:12px;}")
  } else if (theme == "light") {
    paste0(
      "body{background:#FFFFFF;color:#222;font-family:Inter,sans-serif;",
      "max-width:780px;margin:24px auto;padding:0 24px;line-height:1.55;}",
      "h1,h2{color:#1f77b4;}",
      "code{background:#F4F4F4;padding:1px 4px;border-radius:2px;}",
      ".foot{color:#666;font-size:12px;margin-top:36px;}")
  } else {
    paste0(
      "body{background:#0d0d0f;color:#e8e6e0;font-family:Inter,sans-serif;",
      "max-width:780px;margin:24px auto;padding:0 24px;line-height:1.55;}",
      "h1,h2{color:#d4af37;}",
      "pre{background:#1a1a1a;border:1px solid #333;padding:10px;}",
      ".foot{color:#888;font-size:12px;margin-top:36px;}")
  }
}

# Render a brief report. Returns list(format=, path=, notice=).
render_brief_report <- function(state, format = c("html","pdf"),
                                 file_path = NULL,
                                 theme = "bundesbank") {
  format <- match.arg(format)
  manifest <- tryCatch(make_manifest(state), error = function(e) list())
  body <- .report_body_html(state, manifest, theme = theme)
  css  <- .report_skeleton_css(theme)
  html <- sprintf(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>
    <title>Brief report</title>
    <link rel='stylesheet' href='https://fonts.googleapis.com/css2?family=Source+Serif+4:wght@400;600;700&family=Inter:wght@300;400;500;600;700&display=swap'>
    <style>%s</style></head><body>%s</body></html>", css, body)

  out_path <- file_path %||% tempfile(fileext = paste0(".", format))
  if (format == "html") {
    writeLines(html, out_path)
    return(list(format = "html", path = out_path,
                 notice = "HTML rendered."))
  }
  # PDF route
  html_tmp <- tempfile(fileext = ".html")
  writeLines(html, html_tmp)
  if (requireNamespace("pagedown", quietly = TRUE)) {
    res <- tryCatch(pagedown::chrome_print(html_tmp, output = out_path),
                    error = function(e) NULL)
    if (!is.null(res) && file.exists(out_path))
      return(list(format = "pdf", path = out_path,
                   notice = "PDF rendered via pagedown::chrome_print."))
  }
  if (requireNamespace("rmarkdown", quietly = TRUE) &&
      nzchar(Sys.which("pandoc"))) {
    md_tmp <- tempfile(fileext = ".md")
    writeLines(c("# Brief report", "", body), md_tmp)
    res <- tryCatch(rmarkdown::render(md_tmp, output_file = out_path,
                                        output_format = "pdf_document",
                                        quiet = TRUE),
                    error = function(e) NULL)
    if (!is.null(res) && file.exists(out_path))
      return(list(format = "pdf", path = out_path,
                   notice = "PDF rendered via rmarkdown + pandoc."))
  }
  # Fallback: write HTML and explain
  html_path <- if (grepl("\\.pdf$", out_path)) sub("\\.pdf$", ".html", out_path) else out_path
  writeLines(html, html_path)
  list(format = "html", path = html_path,
        notice = paste("PDF export needs pagedown + Chrome or rmarkdown +",
                        "pandoc. Wrote HTML instead."))
}
