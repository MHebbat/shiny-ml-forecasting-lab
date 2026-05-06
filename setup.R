# =====================================================================
# setup.R - one-shot installer for the Shiny ML Forecasting Lab
#
# Run with:  Rscript setup.R           # full install
#            Rscript setup.R --dry-run # show what would happen, no changes
#
# Steps:
#   1. Banner (OS, arch, R, Python detection)
#   2. R packages via install_r_packages() (shared with install_R.R)
#   3. Python configuration (only when needed)
#         macOS/arm64: install_python(3.11), recreate r-reticulate venv
#         if arch mismatch, py_install(tf, keras, sklearn, ...).
#         JAX fallback if TensorFlow refuses to install.
#   4. Parse-check every R/*.R file
#   5. Persist resolved status to ~/.shinyml_setup.json
#   6. Final box-drawn summary
#
# Each step is wrapped in tryCatch so optional failures (Python, JSON
# write) do not abort an otherwise successful R-only setup.
#
# Author: Malik Hebbat
# License: MIT
# =====================================================================

local({
  args     <- commandArgs(trailingOnly = TRUE)
  dry_run  <- any(args %in% c("--dry-run", "-n"))

  `%||%` <- function(x, y)
    if (is.null(x) || length(x) == 0 ||
        (length(x) == 1 && is.na(x))) y else x

  status <- list(
    timestamp        = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    os               = Sys.info()[["sysname"]],
    arch             = Sys.info()[["machine"]],
    r_version        = paste(R.version$major, R.version$minor, sep = "."),
    dry_run          = dry_run,
    r_required_ok    = NA, r_required_missing = character(0),
    r_optional_ok    = NA, r_optional_missing = character(0),
    python           = NA_character_,
    tensorflow_ok    = NA, jax_ok        = NA,
    keras_backend    = NA_character_,
    parse_check_ok   = NA, parse_errors  = character(0),
    lstm_available   = FALSE
  )

  # ---- Step 1: banner ------------------------------------------------
  banner <- function() {
    msg <- function(...) cat(..., "\n", sep = "")
    msg("==========================================================")
    msg("  Shiny ML Forecasting Lab - one-shot setup")
    msg("==========================================================")
    msg("  OS         : ", status$os, " (", status$arch, ")")
    msg("  R version  : ", status$r_version)
    msg("  Mode       : ", if (dry_run) "DRY RUN" else "INSTALL")
    py <- Sys.which("python3")
    if (!nzchar(py)) py <- Sys.which("python")
    msg("  Python on PATH: ", if (nzchar(py)) py else "(not found)")
    msg("==========================================================")
  }
  tryCatch(banner(), error = function(e)
    message("[setup] banner skipped: ", conditionMessage(e)))

  # ---- Step 2: R packages -------------------------------------------
  here <- if (file.exists("R/setup_helpers.R")) "."
          else stop("Run setup.R from the repo root")
  source(file.path(here, "R", "setup_helpers.R"), local = TRUE)

  res <- tryCatch(install_r_packages(dry_run = dry_run),
                  error = function(e) {
                    message("[setup] R install error: ", conditionMessage(e))
                    NULL
                  })
  if (!is.null(res)) {
    status$r_required_missing <- res$required_missing
    status$r_optional_missing <- res$optional_missing
    status$r_required_ok      <- length(res$required_missing) == 0
    status$r_optional_ok      <- length(res$optional_missing) == 0
  }

  # ---- Step 3: Python (only when needed) ----------------------------
  is_darwin_arm64 <- (status$os == "Darwin" && status$arch == "arm64")

  configure_python <- function() {
    if (!requireNamespace("reticulate", quietly = TRUE)) {
      message("[setup] reticulate not installed; skipping Python config.")
      return(invisible(NULL))
    }
    if (dry_run) {
      message("[setup] DRY RUN - would install Python 3.11 and r-reticulate venv with tensorflow, keras, scipy, pandas, numpy, scikit-learn")
      return(invisible(NULL))
    }
    py_path <- tryCatch({
      reticulate::install_python(version = "3.11:latest")
    }, error = function(e) {
      message("[setup] install_python failed: ", conditionMessage(e))
      Sys.which("python3")
    })
    status$python <<- py_path

    venv <- "r-reticulate"
    if (is_darwin_arm64) {
      # Recreate the venv if it exists with a different arch
      existing <- tryCatch(reticulate::virtualenv_python(venv),
                           error = function(e) NULL)
      if (!is.null(existing)) {
        # Heuristic: if the existing python's arch doesn't match arm64,
        # recreate it so TF native libs match the host CPU.
        out <- tryCatch(system2(existing, c("-c",
          "import platform;print(platform.machine())"),
          stdout = TRUE, stderr = TRUE),
          error = function(e) "")
        if (!any(grepl("arm64", out, ignore.case = TRUE))) {
          message("[setup] Recreating r-reticulate venv (arch mismatch).")
          tryCatch(reticulate::virtualenv_remove(venv, confirm = FALSE),
                   error = function(e) NULL)
        }
      }
    }
    tryCatch(
      reticulate::virtualenv_create(venv, python = py_path),
      error = function(e)
        message("[setup] virtualenv_create note: ", conditionMessage(e)))

    py_pkgs <- c("tensorflow", "keras", "scipy", "pandas",
                 "numpy", "scikit-learn")
    tf_ok <- tryCatch({
      reticulate::py_install(py_pkgs, envname = venv, method = "virtualenv")
      reticulate::use_virtualenv(venv, required = FALSE)
      reticulate::py_run_string("import tensorflow as tf")
      TRUE
    }, error = function(e) {
      message("[setup] TensorFlow install/import failed: ", conditionMessage(e))
      FALSE
    })
    status$tensorflow_ok <<- tf_ok
    if (tf_ok) {
      status$keras_backend <<- "tensorflow"
      status$lstm_available <<- TRUE
      return(invisible(NULL))
    }

    message("[setup] Falling back to JAX as the keras backend.")
    jax_ok <- tryCatch({
      reticulate::py_install("jax", envname = venv, method = "virtualenv")
      Sys.setenv(KERAS_BACKEND = "jax")
      reticulate::py_run_string("import jax")
      TRUE
    }, error = function(e) {
      message("[setup] JAX install/import failed: ", conditionMessage(e))
      FALSE
    })
    status$jax_ok <<- jax_ok
    if (jax_ok) {
      status$keras_backend <<- "jax"
      status$lstm_available <<- TRUE
    }
  }
  tryCatch(configure_python(), error = function(e)
    message("[setup] Python step error: ", conditionMessage(e)))

  # ---- Step 4: parse-check every R/*.R file -------------------------
  parse_check <- function() {
    files <- list.files(file.path(here, "R"), pattern = "\\.R$",
                        full.names = TRUE)
    errs <- character(0)
    for (f in files) {
      e <- tryCatch({ parse(file = f); NULL },
                    error = function(e) conditionMessage(e))
      if (!is.null(e)) errs <- c(errs, paste0(f, ": ", e))
    }
    status$parse_errors <<- errs
    status$parse_check_ok <<- length(errs) == 0
    if (length(errs)) {
      message("[setup] PARSE ERRORS:")
      for (e in errs) message("  ", e)
    } else {
      message("[setup] All R/*.R files parsed cleanly (",
              length(files), " files).")
    }
  }
  tryCatch(parse_check(), error = function(e)
    message("[setup] parse-check error: ", conditionMessage(e)))

  # ---- Step 5: persist status ---------------------------------------
  persist_status <- function() {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      message("[setup] jsonlite missing; skipping status write.")
      return(invisible(NULL))
    }
    f <- file.path(Sys.getenv("HOME", unset = "."), ".shinyml_setup.json")
    tryCatch({
      jsonlite::write_json(status, f, auto_unbox = TRUE, pretty = TRUE,
                           null = "null")
      message("[setup] Status written to ", f)
    }, error = function(e)
      message("[setup] Could not write status: ", conditionMessage(e)))
  }
  if (!dry_run) tryCatch(persist_status(), error = function(e)
    message("[setup] persist_status error: ", conditionMessage(e)))

  # ---- Step 6: summary ----------------------------------------------
  bar  <- paste(rep("─", 58), collapse = "")
  edge <- function(l, r) paste0(l, bar, r)
  line <- function(s) {
    s <- substr(s, 1, 56)
    cat(sprintf("│ %-56s │\n", s))
  }
  cat("\n")
  cat(edge("┌", "┐"), "\n", sep = "")
  ready <- isTRUE(status$r_required_ok) && isTRUE(status$parse_check_ok)
  if (ready && !isTRUE(status$lstm_available))
    line("Status: READY (LSTM disabled - TF unavailable)")
  else if (ready)
    line("Status: READY")
  else
    line("Status: NOT READY (see messages above)")
  line("")
  line(sprintf("R required missing: %d", length(status$r_required_missing)))
  line(sprintf("R optional missing: %d", length(status$r_optional_missing)))
  line(sprintf("Parse check: %s",
               if (isTRUE(status$parse_check_ok)) "ok" else "errors"))
  line(sprintf("Keras backend: %s", status$keras_backend %||% "(none)"))
  cat(edge("└", "┘"), "\n", sep = "")
  invisible(status)
})
