# Regex audit test — every cleanup pattern used by Studio / report /
# manifest must accept a synthetic markdown sample with #, >, *, -, spaces.
# A single bad character-class previously crashed the Editorial Studio tab.
suppressMessages(source("R/utils.R"))
suppressMessages(source("R/report_render.R"))

sample <- c(
  "# Title",
  "> blockquote",
  "* bullet",
  "- dash bullet",
  "**bold** and *italic* with `code`",
  "    leading spaces",
  "#- mixed",
  "*-#> all the prefixes",
  ""
)

# Patterns used in the codebase — must each compile and run.
patterns <- list(
  "^[-#>* ]+",
  "`([^`]+)`",
  "\\*\\*([^*]+)\\*\\*",
  "\\*([^*]+)\\*",
  "^\\s*[-*] ",
  "^- ",
  "\\.pdf$"
)

for (p in patterns) {
  res <- tryCatch(gsub(p, "", sample, perl = TRUE),
                  error = function(e) stop("Pattern failed: ", p, " — ", e$message))
  stopifnot(length(res) == length(sample))
}

# .safe_regex must NOT crash on a deliberately malformed pattern; it
# returns the input unchanged with a warning. Force TRE (perl=FALSE) so
# the test exercises the engine that originally rejected the pattern.
bad_pattern <- "^[#>*\\- ]+"  # the original offender (in TRE)
out <- suppressWarnings(.safe_regex(bad_pattern, sample, perl = FALSE))
stopifnot(identical(out, sample))

# .safe_regex on a good pattern works as gsub
out2 <- .safe_regex("^[-#>* ]+", "# Heading")
stopifnot(out2 == "Heading")

cat("test_regex.R: OK\n")
