# =====================================================================
# Training shape-guard test
#
# Exercises the guards that surface clear errors when upstream Data Prep
# or validation-splitting hands a fit_* function an empty or misshapen
# training set. The previous failure mode was a cryptic backend error
# many frames deep; the guards now stop early with a readable message.
#
# Run with:  Rscript tests/test_train_shapes.R
# =====================================================================

local({
  here <- if (file.exists("R/utils.R")) "." else
          if (file.exists("../R/utils.R")) ".." else
          stop("test must be run from repo root or tests/")
  source(file.path(here, "R", "utils.R"), local = TRUE)

  # ---- .assert_train_shapes ------------------------------------------
  # Valid: matched lengths
  stopifnot(isTRUE(.assert_train_shapes(1:10, data.frame(a = 1:10), "test")))
  cat("OK matched lengths pass\n")

  # Empty y
  err <- tryCatch(.assert_train_shapes(integer(0),
                                       data.frame(a = 1:5), "test"),
                  error = function(e) conditionMessage(e))
  stopifnot(grepl("empty", err))
  cat("OK empty y rejected: ", err, "\n", sep = "")

  # Empty X (zero rows)
  err2 <- tryCatch(.assert_train_shapes(1:5,
                                        data.frame(a = integer(0)), "test"),
                   error = function(e) conditionMessage(e))
  stopifnot(grepl("empty", err2))
  cat("OK empty X rejected: ", err2, "\n", sep = "")

  # Mismatch
  err3 <- tryCatch(.assert_train_shapes(1:5,
                                        data.frame(a = 1:7), "test"),
                   error = function(e) conditionMessage(e))
  stopifnot(grepl("internal pipeline bug", err3))
  cat("OK length mismatch rejected: ", err3, "\n", sep = "")

  # ---- .assert_lstm_shapes -------------------------------------------
  # Valid: 3D array with matching first dim and y
  X <- array(0, dim = c(20L, 5L, 3L))
  storage.mode(X) <- "double"
  y <- numeric(20L)
  stopifnot(isTRUE(.assert_lstm_shapes(X, y)))
  cat("OK valid 3D array passes\n")

  # Wrong rank
  X2 <- matrix(0, 20L, 5L)
  err4 <- tryCatch(.assert_lstm_shapes(X2, 1:20),
                   error = function(e) conditionMessage(e))
  stopifnot(grepl("3D array", err4))
  cat("OK 2D array rejected: ", err4, "\n", sep = "")

  # Empty windows
  X3 <- array(0, dim = c(0L, 5L, 3L))
  err5 <- tryCatch(.assert_lstm_shapes(X3, numeric(0)),
                   error = function(e) conditionMessage(e))
  stopifnot(grepl("empty", err5))
  cat("OK empty windows rejected: ", err5, "\n", sep = "")

  # Length mismatch
  X4 <- array(0, dim = c(20L, 5L, 3L))
  err6 <- tryCatch(.assert_lstm_shapes(X4, 1:10),
                   error = function(e) conditionMessage(e))
  stopifnot(grepl("windows but y has", err6))
  cat("OK window/target mismatch rejected: ", err6, "\n", sep = "")

  cat("train shapes: PASS\n")
})
