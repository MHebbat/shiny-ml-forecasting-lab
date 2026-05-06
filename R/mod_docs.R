# =====================================================================
# In-app documentation drawer
#
# doc_chip(id, title) renders a small "?" chip next to a heading and an
# associated bslib::offcanvas drawer carrying the documentation content
# from R/docs_content.R. Each chip uses bslib::offcanvas_toggler to open
# its drawer; no Shiny server-side wiring is required.
#
# Author: Malik Hebbat
# =====================================================================

doc_chip <- function(doc_id, title = NULL) {
  entry <- DOCS[[doc_id]]
  ttl <- title %||% entry$title %||% doc_id
  body_md <- .docs_render_body(entry)
  off_id <- paste0("doc_off_", gsub("[^A-Za-z0-9]+", "_", doc_id))

  # bslib's offcanvas API differs by version; we build the markup directly
  # using Bootstrap 5 data attributes so the chip works regardless of
  # which bslib helper is or isn't available.
  body_html <- .docs_render_md(body_md)

  tagList(
    tags$button(
      type = "button",
      class = "doc-chip",
      style = paste0("background:transparent;border:1px solid currentColor;",
                      "color:inherit;border-radius:50%;width:22px;height:22px;",
                      "padding:0;font-weight:700;font-size:12px;line-height:1;",
                      "cursor:pointer;display:inline-flex;align-items:center;",
                      "justify-content:center;margin-left:6px;opacity:.75;"),
      `data-bs-toggle`  = "offcanvas",
      `data-bs-target`  = paste0("#", off_id),
      `aria-controls`   = off_id,
      title = paste("Help:", ttl),
      "?"
    ),
    tags$div(
      class = "offcanvas offcanvas-end",
      tabindex = "-1",
      id = off_id,
      style = "max-width:560px;",
      tags$div(class = "offcanvas-header",
        tags$h5(class = "offcanvas-title", paste("Help —", ttl)),
        tags$button(type = "button", class = "btn-close",
                     `data-bs-dismiss` = "offcanvas",
                     `aria-label` = "Close")
      ),
      tags$div(class = "offcanvas-body",
        HTML(body_html)
      )
    )
  )
}

# Tiny markdown -> HTML for the drawer (bold, italic, code, paragraphs).
.docs_render_md <- function(s) {
  if (is.null(s) || !nzchar(s)) return("")
  s <- htmltools::htmlEscape(s)
  s <- gsub("`([^`]+)`", "<code>\\1</code>", s)
  s <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", s, perl = TRUE)
  s <- gsub("\\*([^*]+)\\*", "<em>\\1</em>", s, perl = TRUE)
  paras <- strsplit(s, "\n\n+", perl = TRUE)[[1]]
  paste(vapply(paras, function(p) paste0("<p>",
                                            gsub("\n", "<br>", p, fixed = TRUE),
                                            "</p>"),
                 character(1)),
         collapse = "\n")
}
