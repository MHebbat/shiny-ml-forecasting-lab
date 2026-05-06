# =====================================================================
# Plot themes: Studio (dark editorial), Light minimal, Bundesbank.
#
# Each theme exposes:
#   <name>_palette(kind)  -> character vector of hex colours
#                            kind in c("qualitative","sequential","diverging")
#   theme_<name>()        -> ggplot2::theme() object
#   plotly_<name>()       -> a list passed to plotly::layout()
#
# A small selector layer (`plot_theme_apply_*`) reads `state$plot_theme`
# (one of "studio" | "light" | "bundesbank") and dispatches to the
# correct helper.
#
# Reference (Bundesbank):
#   The Deutsche Bundesbank Monthly Report and Statistical Series use a
#   sober palette dominated by the corporate dark blue (`#003D7C`) with
#   secondary red, ochre, teal, and grey accents. The theme below
#   reproduces the look — clean white background, very thin grey
#   gridlines, single dark axis line, Inter typography, legend at the
#   bottom. Hex values are an editorial approximation of the published
#   colours; the Bank does not publish a normative web palette.
#
# Author: Malik Hebbat
# =====================================================================

# ---- Bundesbank palette ---------------------------------------------
.bundesbank_colors <- list(
  primary_blue   = "#003D7C",
  secondary_red  = "#B61F29",
  accent_ochre   = "#C8A951",
  accent_teal    = "#3F7D7B",
  neutral_grey   = "#5C5C5C",
  light_grid     = "#D9D9D9",
  background     = "#FFFFFF"
)

bundesbank_palette <- function(kind = c("qualitative", "sequential", "diverging"),
                                n = NULL) {
  kind <- match.arg(kind)
  qual <- c(.bundesbank_colors$primary_blue,
            .bundesbank_colors$secondary_red,
            .bundesbank_colors$accent_ochre,
            .bundesbank_colors$accent_teal,
            .bundesbank_colors$neutral_grey,
            "#7BAFD4",  # softened blue tint
            "#A14B57")  # softened red tint
  seq_pal  <- grDevices::colorRampPalette(c("#E6EFF7",
                                             "#9DBCDC",
                                             .bundesbank_colors$primary_blue))
  div_pal  <- grDevices::colorRampPalette(c(.bundesbank_colors$primary_blue,
                                             "#9DBCDC",
                                             "#F0F0F0",
                                             "#D88B92",
                                             .bundesbank_colors$secondary_red))
  out <- switch(kind,
    "qualitative" = qual,
    "sequential"  = seq_pal(if (is.null(n)) 9 else n),
    "diverging"   = div_pal(if (is.null(n)) 11 else n))
  if (kind == "qualitative" && !is.null(n)) {
    if (n <= length(out)) out <- out[seq_len(n)]
    else out <- grDevices::colorRampPalette(out)(n)
  }
  out
}

# ---- Bundesbank ggplot theme ----------------------------------------
theme_bundesbank <- function(base_size = 11) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("theme_bundesbank requires the ggplot2 package")
  ggplot2::theme_minimal(base_size = base_size, base_family = "Inter") +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(
        fill = .bundesbank_colors$background, colour = NA),
      panel.background = ggplot2::element_rect(
        fill = .bundesbank_colors$background, colour = NA),
      panel.border     = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        colour = .bundesbank_colors$light_grid, linewidth = 0.3),
      axis.line.x = ggplot2::element_line(
        colour = .bundesbank_colors$primary_blue, linewidth = 0.5),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks  = ggplot2::element_line(
        colour = .bundesbank_colors$neutral_grey, linewidth = 0.3),
      axis.text   = ggplot2::element_text(
        colour = .bundesbank_colors$neutral_grey, size = base_size - 1),
      axis.title  = ggplot2::element_text(
        colour = .bundesbank_colors$primary_blue, size = base_size),
      plot.title  = ggplot2::element_text(
        family = "Inter", face = "bold", size = base_size + 3,
        colour = .bundesbank_colors$primary_blue),
      plot.subtitle = ggplot2::element_text(
        family = "Inter", face = "plain", size = base_size,
        colour = .bundesbank_colors$neutral_grey),
      plot.caption  = ggplot2::element_text(
        family = "Inter", size = base_size - 2,
        colour = .bundesbank_colors$neutral_grey),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title    = ggplot2::element_text(
        colour = .bundesbank_colors$primary_blue, size = base_size - 1),
      legend.text     = ggplot2::element_text(
        colour = .bundesbank_colors$neutral_grey, size = base_size - 1),
      legend.background = ggplot2::element_blank(),
      legend.key        = ggplot2::element_blank()
    )
}

# ---- Plotly equivalents ----------------------------------------------
# Each returns a named list of arguments suitable for `plotly::layout(...)`.
plotly_bundesbank <- function() {
  list(
    paper_bgcolor = .bundesbank_colors$background,
    plot_bgcolor  = .bundesbank_colors$background,
    font          = list(family = "Inter", size = 12,
                         color = .bundesbank_colors$neutral_grey),
    title         = list(font = list(family = "Inter", size = 16,
                            color = .bundesbank_colors$primary_blue)),
    legend        = list(orientation = "h", x = 0, y = -0.2,
                         font = list(color = .bundesbank_colors$neutral_grey)),
    xaxis = list(showgrid = FALSE,
                 zeroline = FALSE,
                 showline = TRUE,
                 linecolor = .bundesbank_colors$primary_blue,
                 tickcolor = .bundesbank_colors$neutral_grey,
                 tickfont  = list(color = .bundesbank_colors$neutral_grey)),
    yaxis = list(showgrid = TRUE,
                 gridcolor = .bundesbank_colors$light_grid,
                 zeroline = FALSE,
                 showline = FALSE,
                 separatethousands = TRUE,
                 tickfont = list(color = .bundesbank_colors$neutral_grey))
  )
}

plotly_studio <- function() {
  list(
    paper_bgcolor = "#0d1117",
    plot_bgcolor  = "#0d1117",
    font = list(family = "Inter", color = "#c9d1d9"),
    legend = list(font = list(color = "#c9d1d9"))
  )
}

plotly_light <- function() {
  list(
    paper_bgcolor = "#FFFFFF",
    plot_bgcolor  = "#FFFFFF",
    font = list(family = "Inter", color = "#222"),
    xaxis = list(gridcolor = "#EAEAEA", zerolinecolor = "#EAEAEA"),
    yaxis = list(gridcolor = "#EAEAEA", zerolinecolor = "#EAEAEA"),
    legend = list(font = list(color = "#222"))
  )
}

# ---- Theme dispatch helpers -----------------------------------------
plot_theme_id <- function(state) {
  v <- tryCatch(state$plot_theme, error = function(e) NULL)
  if (is.null(v) || !nzchar(v)) "studio" else as.character(v)
}

plot_theme_layout <- function(state) {
  switch(plot_theme_id(state),
    "bundesbank" = plotly_bundesbank(),
    "light"      = plotly_light(),
    plotly_studio())
}

plot_theme_palette <- function(state,
                                kind = c("qualitative", "sequential", "diverging"),
                                n = NULL) {
  kind <- match.arg(kind)
  switch(plot_theme_id(state),
    "bundesbank" = bundesbank_palette(kind, n),
    "light"      = if (kind == "qualitative")
      c("#1f77b4","#d62728","#2ca02c","#9467bd","#ff7f0e","#17becf","#8c564b")
      else if (kind == "sequential")
        grDevices::colorRampPalette(c("#E6EEF6","#1f77b4"))(if (is.null(n)) 9 else n)
      else
        grDevices::colorRampPalette(c("#1f77b4","#FFFFFF","#d62728"))(if (is.null(n)) 11 else n),
    {
      # studio (dark editorial)
      if (kind == "qualitative")
        c("#3fb950","#d4af37","#58a6ff","#d9534f","#9d7cd8","#f1c40f","#c9d1d9")
      else if (kind == "sequential")
        grDevices::colorRampPalette(c("#0d1117","#3fb950"))(if (is.null(n)) 9 else n)
      else
        grDevices::colorRampPalette(c("#d9534f","#0d1117","#3fb950"))(if (is.null(n)) 11 else n)
    })
}

# Convenience: plotly object piped through layout for the active theme.
plotly_apply_theme <- function(p, state, extra = list()) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(p)
  args <- modifyList(plot_theme_layout(state), extra %||% list())
  do.call(plotly::layout, c(list(p = p), args))
}

# Convenience for a primary chart colour (single-series fallback).
plot_theme_primary <- function(state) {
  plot_theme_palette(state, "qualitative", 1)[1]
}

# Renders a small swatch preview as a plotly bar — used in the picker UI.
plot_theme_preview <- function(theme_id) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  fake_state <- list(plot_theme = theme_id)
  pal <- plot_theme_palette(fake_state, "qualitative", 5)
  plotly::plot_ly(x = seq_along(pal), y = rep(1, length(pal)),
                   type = "bar",
                   marker = list(color = pal),
                   hoverinfo = "skip", showlegend = FALSE) |>
    plotly::layout(
      paper_bgcolor = switch(theme_id,
        "bundesbank" = "#FFFFFF", "light" = "#FFFFFF", "#0d1117"),
      plot_bgcolor = switch(theme_id,
        "bundesbank" = "#FFFFFF", "light" = "#FFFFFF", "#0d1117"),
      margin = list(l = 0, r = 0, t = 0, b = 0),
      xaxis = list(showticklabels = FALSE, showgrid = FALSE,
                   zeroline = FALSE, fixedrange = TRUE),
      yaxis = list(showticklabels = FALSE, showgrid = FALSE,
                   zeroline = FALSE, fixedrange = TRUE)
    ) |>
    plotly::config(displayModeBar = FALSE)
}
