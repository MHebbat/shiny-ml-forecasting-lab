# Confirm the dispatcher surfaces .assert_*_shapes() messages verbatim
# rather than R's default "arguments imply differing number of rows".
suppressMessages(source("R/utils.R"))
suppressMessages(suppressWarnings(try(source("R/model_registry.R"), silent = TRUE)))

stopifnot(exists("safe_fit_dispatch"), exists(".assert_train_shapes"))

# 1) Empty training set -> the shape guard's "Cannot train" message.
empty_df <- data.frame(y = numeric(0), x = numeric(0))
fake_fn <- function(df, target, params, time_col = NULL) {
  .assert_train_shapes(df[[target]], df, "FakeModel")
  list(model = NULL, predict = function(...) numeric(0))
}
err <- tryCatch(safe_fit_dispatch(fake_fn, empty_df, "y", list()),
                error = function(e) conditionMessage(e))
stopifnot(grepl("Cannot train", err))
stopifnot(grepl("FakeModel", err))
stopifnot(!grepl("arguments imply differing number of rows", err))

# 2) When a fitter would otherwise hit data.frame's row mismatch, the
#    wrapper rewrites it to a friendlier message.
broken_fn <- function(df, target, params, time_col = NULL) {
  data.frame(a = 1:3, b = 1:5)
}
err2 <- tryCatch(safe_fit_dispatch(broken_fn, data.frame(y=1), "y", list()),
                 error = function(e) conditionMessage(e))
stopifnot(grepl("Cannot train", err2))
stopifnot(!grepl("arguments imply differing number of rows", err2))

# 3) Prophet path with 0 surviving rows must return a readable message,
#    not a row-mismatch error.
err3 <- tryCatch(prophet_prepare(data.frame(date = as.Date(NA), sales = NA),
                                  time_col = "date", target = "sales"),
                 error = function(e) conditionMessage(e))
stopifnot(grepl("could not be parsed|0 rows", err3))

cat("test_dispatcher_errors.R: OK\n")
