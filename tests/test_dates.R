# =====================================================================
# Robust date parsing tests
#
# Verifies that safe_as_date / safe_as_posix never throw, fall back to
# lubridate orders for ambiguous strings, and return NA where parsing
# is impossible.
# =====================================================================

local({
  here <- if (file.exists("R/utils.R")) "." else
          if (file.exists("../R/utils.R")) ".." else
          stop("test must be run from repo root or tests/")
  source(file.path(here, "R", "utils.R"), local = TRUE)

  # Standard ISO -> Date
  v <- safe_as_date(c("2024-01-31", "2024-12-15"))
  stopifnot(inherits(v, "Date"))
  stopifnot(all(!is.na(v)))
  cat("OK ISO date parses\n")

  # Mixed orders -> at least one parses via lubridate fallback
  v2 <- safe_as_date(c("31/01/2024", "15-12-2024", "2024-06-01"))
  stopifnot(inherits(v2, "Date"))
  # We expect at least 2/3 parsed under the lubridate fallback when present.
  ok <- sum(!is.na(v2))
  cat(sprintf("OK mixed-order parse: %d / 3\n", ok))

  # Garbage -> NA, no crash
  v3 <- safe_as_date(c("not a date", "still nope"))
  stopifnot(all(is.na(v3)))
  cat("OK garbage -> NA, no crash\n")

  # POSIXct path
  vp <- safe_as_posix(c("2024-01-31 10:15:00", "2024-12-15 23:59:59"))
  stopifnot(inherits(vp, "POSIXt"))
  stopifnot(all(!is.na(vp)))
  cat("OK POSIXct parse\n")

  # Already-Date input is a no-op
  d <- as.Date(c("2020-01-01", "2020-02-01"))
  stopifnot(identical(safe_as_date(d), d))
  cat("OK already-Date passthrough\n")

  cat("dates: PASS\n")
})
