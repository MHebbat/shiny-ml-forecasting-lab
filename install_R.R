# =====================================================================
# install_R.R - cross-platform R package installer for the Shiny ML Lab
# Idempotent: re-running this script only installs what's missing.
# Works on macOS, Windows, and Linux without modification.
#
# Implementation lives in R/setup_helpers.R::install_r_packages() so that
# install_R.R and the new one-shot setup.R share the same package list.
# =====================================================================

local({
  # ---- Friendly OS report --------------------------------------------
  os <- if (.Platform$OS.type == "windows") "Windows"
        else if (Sys.info()[["sysname"]] == "Darwin") "macOS"
        else "Linux"
  message(sprintf("[install_R] %s | R %s.%s",
                  os, R.version$major, R.version$minor))

  here <- if (file.exists("R/setup_helpers.R")) "."
          else if (file.exists("../R/setup_helpers.R")) ".."
          else stop("Cannot locate R/setup_helpers.R - run from repo root")
  source(file.path(here, "R", "setup_helpers.R"), local = TRUE)

  res <- install_r_packages(dry_run = FALSE)

  if (length(res$optional_missing) > 0) {
    message("[install_R] NOTE: optional package(s) not installed: ",
            paste(res$optional_missing, collapse = ", "),
            ". The app will still run; affected models will show a ",
            "missing-dependency notice in the Model Lab.")
  }

  if (length(res$required_missing) > 0) {
    warning("[install_R] Still missing required: ",
            paste(res$required_missing, collapse = ", "),
            "\n  Try installing manually: install.packages(c(",
            paste(sprintf("'%s'", res$required_missing), collapse = ", "),
            "))")
    quit(status = 1)
  }

  message("[install_R] Done. ", length(res$installed_required),
          " required package(s) ready (", length(res$installed_optional),
          "/", length(res$installed_optional) + length(res$optional_missing),
          " optional).")
})
