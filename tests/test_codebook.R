# =====================================================================
# Codebook test
#
# Builds a haven::labelled column and asserts the value-label
# dictionary is preserved after the codebook builder runs.
#
# Skipped when 'haven' is not installed (we still test the plain-vector
# path).
# =====================================================================

local({
  here <- if (file.exists("R/utils.R")) "." else
          if (file.exists("../R/utils.R")) ".." else
          stop("test must be run from repo root or tests/")
  source(file.path(here, "R", "utils.R"), local = TRUE)
  source(file.path(here, "R", "mod_codebook.R"), local = TRUE)

  set.seed(1)

  # Plain-vector codebook works regardless of haven availability
  df_plain <- data.frame(
    x = rnorm(20),
    g = sample(c("a","b","c"), 20, TRUE),
    stringsAsFactors = FALSE
  )
  attr(df_plain$x, "label") <- "Random normal draws"
  cb <- build_codebook_rich(df_plain, list())
  stopifnot(is.data.frame(cb), nrow(cb) == 2L)
  stopifnot("name" %in% names(cb), "type" %in% names(cb))
  stopifnot("Random normal draws" %in% cb$label)
  cat("OK plain-vector codebook: ", nrow(cb), " rows, label preserved\n",
      sep = "")

  # Markdown export round-trip
  md <- codebook_to_markdown(cb)
  stopifnot(is.character(md), grepl("\\| name \\|", md))
  cat("OK codebook_to_markdown produces a markdown table\n")

  # haven::labelled path -- preserves value labels
  if (requireNamespace("haven", quietly = TRUE)) {
    sat <- sample(1:5, 30, TRUE)
    sat_lab <- haven::labelled(
      sat,
      labels = c("Strongly disagree" = 1, "Disagree" = 2, "Neutral" = 3,
                  "Agree" = 4, "Strongly agree" = 5),
      label = "Overall life satisfaction (Likert 1-5)")
    df_lab <- data.frame(weight = runif(30, 0.5, 2.0),
                          satisfaction = sat_lab,
                          stringsAsFactors = FALSE)
    df_lab$satisfaction <- sat_lab
    cb2 <- build_codebook_rich(df_lab, list())
    sat_row <- cb2[cb2$name == "satisfaction", , drop = FALSE]
    stopifnot(nrow(sat_row) == 1L)
    stopifnot(sat_row$type == "labelled")
    stopifnot(sat_row$n_value_labels == 5L)
    vl <- sat_row$value_labels_list[[1]]
    stopifnot(!is.null(vl), length(vl) == 5L)
    stopifnot("Strongly disagree" %in% names(vl))
    cat("OK haven::labelled column: type=labelled, ",
        sat_row$n_value_labels, " value labels preserved\n", sep = "")
  } else {
    cat("SKIP haven path (haven not installed)\n")
  }

  cat("codebook: PASS\n")
})
