# =====================================================================
# LSTM supervised-window shape test
#
# Verifies that lstm_make_supervised() returns the 3D R array shape and
# storage mode that the keras3 backend now expects (the recent
# ValueError "Unrecognized data type" came from keras3 receiving
# ambiguous numpy dtypes; the fix casts via keras3::as_tensor(..., "float32")
# but still requires the upstream R array to be a 3D double array).
#
# Pure R; no Python required.
#
# Run with:  Rscript tests/test_lstm_shapes.R
# =====================================================================

local({
  here <- if (file.exists("R/utils.R")) "." else
          if (file.exists("../R/utils.R")) ".." else
          stop("test must be run from repo root or tests/")
  source(file.path(here, "R", "utils.R"), local = TRUE)
  source(file.path(here, "R", "model_registry.R"), local = TRUE)

  # ---- Univariate ----------------------------------------------------
  set.seed(1)
  y <- as.numeric(sin(seq(0, 4 * pi, length.out = 80))) + rnorm(80, 0, 0.05)
  sup1 <- lstm_make_supervised(y, timesteps = 12)

  stopifnot(is.list(sup1))
  stopifnot(is.array(sup1$X))
  stopifnot(length(dim(sup1$X)) == 3L)
  stopifnot(storage.mode(sup1$X) == "double")
  stopifnot(dim(sup1$X)[1] == length(sup1$y))
  stopifnot(dim(sup1$X)[2] == 12L)
  stopifnot(dim(sup1$X)[3] == 1L)
  stopifnot(sup1$n_features == 1L)
  stopifnot(sup1$timesteps == 12L)
  cat("OK univariate: dim=", paste(dim(sup1$X), collapse = "x"),
      " storage=", storage.mode(sup1$X), "\n", sep = "")

  # ---- Multivariate --------------------------------------------------
  set.seed(2)
  M <- cbind(stats::rnorm(60), stats::rexp(60, 1), stats::runif(60))
  sup2 <- lstm_make_supervised(M, timesteps = 8L, target_col = 3L)
  stopifnot(length(dim(sup2$X)) == 3L)
  stopifnot(storage.mode(sup2$X) == "double")
  stopifnot(dim(sup2$X)[1] == nrow(M) - 8L)
  stopifnot(dim(sup2$X)[2] == 8L)
  stopifnot(dim(sup2$X)[3] == 3L)
  stopifnot(sup2$n_features == 3L)
  cat("OK multivariate: dim=", paste(dim(sup2$X), collapse = "x"),
      " storage=", storage.mode(sup2$X), "\n", sep = "")

  # ---- Edge: too short -----------------------------------------------
  err <- tryCatch(lstm_make_supervised(1:5, timesteps = 10L),
                  error = function(e) conditionMessage(e))
  stopifnot(grepl("too short", err))
  cat("OK too-short series rejected: ", err, "\n", sep = "")

  # ---- Edge: bad timesteps -------------------------------------------
  err2 <- tryCatch(lstm_make_supervised(1:30, timesteps = -1L),
                   error = function(e) conditionMessage(e))
  stopifnot(grepl("positive integer", err2))
  cat("OK bad timesteps rejected: ", err2, "\n", sep = "")

  cat("lstm shapes: PASS\n")
})
