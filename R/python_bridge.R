# =====================================================================
# Python interop via reticulate
# Lazily attempts to import scientific Python; falls back gracefully.
# =====================================================================

.py_state <- new.env(parent = emptyenv())
.py_state$tried <- FALSE
.py_state$ok    <- FALSE

py_init <- function() {
  if (.py_state$tried) return(.py_state$ok)
  .py_state$tried <- TRUE
  ok <- tryCatch({
    if (!requireNamespace("reticulate", quietly = TRUE)) return(FALSE)
    # Try to find a sane python; user can override with RETICULATE_PYTHON env var
    py <- Sys.getenv("RETICULATE_PYTHON", unset = "")
    if (nzchar(py)) reticulate::use_python(py, required = FALSE)
    reticulate::py_available(initialize = TRUE)
  }, error = function(e) FALSE)
  .py_state$ok <- isTRUE(ok)
  .py_state$ok
}

py_is_available <- function() py_init()

py_has_module <- function(name) {
  if (!py_is_available()) return(FALSE)
  tryCatch(reticulate::py_module_available(name), error = function(e) FALSE)
}

# Train a Python sklearn / lightgbm model from R via reticulate
py_train_predict <- function(model_id, x_train, y_train, x_test, params = list(),
                             task = "regression") {
  if (!py_is_available()) stop("Python not available")
  reticulate::source_python("python/py_models.py")
  res <- reticulate::py$train_predict(
    model_id   = model_id,
    x_train    = as.data.frame(x_train),
    y_train    = as.numeric(y_train),
    x_test     = as.data.frame(x_test),
    params     = params,
    task       = task
  )
  list(
    predictions = as.numeric(res$predictions),
    feature_importance = res$feature_importance
  )
}
