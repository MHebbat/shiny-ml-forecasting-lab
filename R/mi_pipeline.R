# =====================================================================
# Multiple Imputation pipeline (Rubin's rules)
#
# End-to-end pooling of survey estimates and svyglm models across
# multiple implicates. Built on top of `mitools` and `survey`.
#
# Key entry points:
#   mi_detect(df, hint = NULL) -> list of implicate frames
#   mi_designs(implicates, design_spec) -> mitools::imputationList of
#       survey designs (svydesign or svrepdesign per design_spec)
#   mi_estimate(designs, formula, family, model) -> pooled tibble with
#       term, estimate, std.error, statistic, p.value, df, fmi, riv,
#       lambda
#   mi_diagnostics(combined) -> compact list of pooling diagnostics
#
# Model types supported by mi_estimate():
#   svyglm       — linear / logistic / Poisson via svyglm()
#   svymean      — single-variable mean
#   svyquantile  — single-variable quantile (via interval midpoint)
#   svyratio     — ratio of two variables
#
# Author: Malik Hebbat
# =====================================================================

# ---- Detection -------------------------------------------------------
# Returns a list of data.frame implicates, plus an `attr(out, "kind")`
# describing how they were detected. If the data does not look like an
# MI dataset, returns a length-0 list with kind = "none".
#
# `hint` may carry one of:
#   list(kind = "long",     column = "implicate")
#   list(kind = "wide",     pattern = "(_imp|_i)([0-9]+)$")
#   list(kind = "explicit", implicate_list = list(df1, df2, ...))
mi_detect <- function(df, hint = NULL) {
  out <- list()

  # Explicit list (PHF template path -- caller already split frames)
  if (!is.null(hint) && identical(hint$kind, "explicit") &&
      is.list(hint$implicate_list) && length(hint$implicate_list) >= 2) {
    out <- lapply(hint$implicate_list, as.data.frame)
    attr(out, "kind") <- "explicit"
    return(out)
  }

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    attr(out, "kind") <- "none"
    return(out)
  }

  # Long format
  long_cols <- intersect(names(df),
                          c("implicate", "imputation", "imp", "imp_id"))
  if (length(long_cols) > 0) {
    col <- long_cols[1]
    levels_v <- sort(unique(stats::na.omit(df[[col]])))
    if (length(levels_v) >= 2) {
      out <- lapply(levels_v, function(lv) {
        sub <- df[df[[col]] == lv & !is.na(df[[col]]), , drop = FALSE]
        sub[[col]] <- NULL
        sub
      })
      attr(out, "kind") <- "long"
      return(out)
    }
  }

  # Wide-suffix format: var_imp1, var_imp2, ... or var_i1, var_i2, ...
  pat <- if (!is.null(hint) && identical(hint$kind, "wide") &&
              !is.null(hint$pattern)) hint$pattern else "(_imp|_i)([0-9]+)$"
  imp_cols <- grep(pat, names(df), value = TRUE)
  if (length(imp_cols) > 0) {
    nums <- as.integer(sub(paste0(".*", pat), "\\2", imp_cols))
    levs <- sort(unique(stats::na.omit(nums)))
    if (length(levs) >= 2) {
      base_cols <- setdiff(names(df), imp_cols)
      out <- lapply(levs, function(k) {
        # for each implicate k, take the columns matching ..._impK / ..._iK
        keep <- imp_cols[nums == k]
        # Strip suffix to get the canonical name
        canonical <- sub(pat, "", keep)
        sub <- df[, c(base_cols, keep), drop = FALSE]
        names(sub)[match(keep, names(sub))] <- canonical
        sub
      })
      attr(out, "kind") <- "wide"
      return(out)
    }
  }

  attr(out, "kind") <- "none"
  out
}

# ---- Build a survey design per implicate -----------------------------
# `design_spec` is a list with the fields used by make_design() in
# mod_survey.R: method, weight, strata, psu, repweights_pattern,
# repdesign_type, combined_weights, use_mse, plus optional variance
# options (variance_method, replicates, brr_rho).
#
# Returns a mitools::imputationList containing one survey.design (or
# svyrep.design) per implicate.
mi_designs <- function(implicates, design_spec = list()) {
  if (!requireNamespace("mitools", quietly = TRUE))
    stop("Package 'mitools' is required for MI pooling. ",
         "install.packages('mitools')")
  if (!requireNamespace("survey", quietly = TRUE))
    stop("Package 'survey' is required for MI pooling.")
  if (!is.list(implicates) || length(implicates) < 2)
    stop("mi_designs() needs at least 2 implicate frames.")

  method <- design_spec$method %||% "standard"

  build_one <- function(df) {
    df2 <- .svy_zap(df)
    weight_col <- design_spec$weight
    strata_col <- design_spec$strata
    psu_col    <- design_spec$psu

    norm <- function(x) {
      if (is.null(x) || length(x) == 0) return(NULL)
      x <- x[1]
      if (is.na(x) || !nzchar(x) || x == "(none)") return(NULL)
      if (!x %in% names(df2)) return(NULL)
      x
    }
    weight_col <- norm(weight_col)
    strata_col <- norm(strata_col)
    psu_col    <- norm(psu_col)

    if (!is.null(weight_col)) {
      w <- suppressWarnings(as.numeric(df2[[weight_col]]))
      keep <- !is.na(w) & is.finite(w) & w > 0
      df2 <- df2[keep, , drop = FALSE]
      df2[[weight_col]] <- w[keep]
    }

    if (method == "replicate") {
      pat <- design_spec$repweights_pattern
      rep_cols <- if (!is.null(pat) && nzchar(pat))
        grep(pat, names(df2), value = TRUE) else character(0)
      if (length(rep_cols) == 0)
        stop("No replicate-weight columns matched pattern '",
             pat %||% "", "' in implicate.")
      rep_args <- list(
        data        = df2,
        repweights  = df2[, rep_cols, drop = FALSE],
        type        = toupper(as.character(design_spec$repdesign_type %||% "BRR")),
        combined.weights = isTRUE(design_spec$combined_weights %||% TRUE),
        mse         = isTRUE(design_spec$use_mse %||% TRUE)
      )
      if (!is.null(weight_col)) rep_args$weights <- df2[[weight_col]]
      des <- do.call(survey::svrepdesign, rep_args)
    } else {
      ids     <- if (!is.null(psu_col)) stats::as.formula(paste0("~", psu_col)) else stats::as.formula("~1")
      strata  <- if (!is.null(strata_col)) stats::as.formula(paste0("~", strata_col)) else NULL
      weights <- if (!is.null(weight_col)) stats::as.formula(paste0("~", weight_col)) else NULL
      des <- tryCatch(
        survey::svydesign(ids = ids, strata = strata, weights = weights,
                          data = df2, nest = TRUE),
        error = function(e)
          survey::svydesign(ids = ids, strata = strata, weights = weights,
                            data = df2))
    }

    # Optional: convert Taylor design to replicate design with a chosen
    # variance method.
    vm <- design_spec$variance_method %||% NA_character_
    if (!is.na(vm) && method == "standard" &&
        vm %in% c("bootstrap", "JK1", "JKn", "BRR", "Fay")) {
      reps <- as.integer(design_spec$replicates %||% 200L)
      if (!is.finite(reps) || reps < 50L) reps <- 200L
      args <- list(design = des, type = vm, replicates = reps)
      if (vm == "Fay") {
        args$type <- "Fay"
        args$fay.rho <- as.numeric(design_spec$brr_rho %||% 0)
      }
      des <- tryCatch(do.call(survey::as.svrepdesign, args),
                      error = function(e) des)
    }
    des
  }

  des_list <- lapply(implicates, build_one)
  mitools::imputationList(des_list)
}

# ---- Pooled estimation ----------------------------------------------
# `formula` may be either a one-sided formula (~x for svymean / svyquantile)
# or a two-sided formula (y ~ x1 + x2 for svyglm). For svyratio supply
# `numerator` and `denominator` formulae.
#
# Returns a tibble (or data.frame fallback) with one row per term:
#   term, estimate, std.error, statistic, p.value, df, fmi, riv, lambda
#
# fmi    = fraction of missing information
# riv    = relative increase in variance due to nonresponse
# lambda = within-imputation variance share (1 - fmi when df is large)
mi_estimate <- function(designs, formula = NULL, family = NULL,
                         model = c("svyglm","svymean","svyquantile","svyratio"),
                         numerator = NULL, denominator = NULL,
                         quantile_probs = 0.5) {
  model <- match.arg(model)
  if (!requireNamespace("mitools", quietly = TRUE))
    stop("Package 'mitools' is required for MI pooling.")
  if (!requireNamespace("survey", quietly = TRUE))
    stop("Package 'survey' is required for MI pooling.")
  if (!inherits(designs, "imputationList"))
    stop("`designs` must be a mitools::imputationList. Use mi_designs().")

  des_list <- designs$imputations
  fam <- if (is.null(family)) stats::gaussian()
         else if (is.function(family)) family()
         else if (is.character(family)) get(family, mode = "function")()
         else family

  fits <- switch(model,
    "svyglm" = {
      if (is.null(formula))
        stop("svyglm needs a two-sided formula (y ~ x1 + x2).")
      lapply(des_list, function(d)
        survey::svyglm(formula, family = fam, design = d))
    },
    "svymean" = {
      if (is.null(formula))
        stop("svymean needs a one-sided formula (~x).")
      lapply(des_list, function(d)
        survey::svymean(formula, design = d, na.rm = TRUE))
    },
    "svyquantile" = {
      if (is.null(formula))
        stop("svyquantile needs a one-sided formula (~x).")
      lapply(des_list, function(d)
        survey::svyquantile(formula, design = d,
                             quantiles = quantile_probs,
                             na.rm = TRUE, ci = TRUE))
    },
    "svyratio" = {
      if (is.null(numerator) || is.null(denominator))
        stop("svyratio needs both numerator and denominator one-sided formulas.")
      lapply(des_list, function(d)
        survey::svyratio(numerator, denominator,
                          design = d, na.rm = TRUE))
    }
  )

  # In `with(..., svyglm)` the call returns a regular list of fits when
  # the verb is a model. mitools::MIcombine accepts such lists directly.
  combined <- tryCatch(mitools::MIcombine(fits),
                       error = function(e) NULL)
  if (is.null(combined))
    stop("MIcombine() failed to pool implicate estimates.")

  cf  <- as.numeric(combined$coefficients)
  vc  <- combined$variance
  se  <- if (is.matrix(vc)) sqrt(diag(vc)) else sqrt(as.numeric(vc))
  df_v <- if (is.null(combined$df)) rep(NA_real_, length(cf)) else as.numeric(combined$df)

  # Rubin's quantities — pull from MIcombine's internals if present;
  # otherwise compute from the per-implicate fits.
  m <- length(fits)
  if (!is.null(combined$missinfo)) {
    fmi <- as.numeric(combined$missinfo)
  } else {
    fmi <- rep(NA_real_, length(cf))
  }
  # Recover within-imp vs between-imp variance for diagnostics
  per_se <- tryCatch({
    do.call(rbind, lapply(fits, function(f) {
      # f might be a svyglm fit, or a svystat (svymean/svytotal). Use
      # SE() if available, otherwise sqrt(diag(vcov())).
      v <- tryCatch(stats::vcov(f), error = function(e) NULL)
      if (is.null(v)) return(rep(NA_real_, length(cf)))
      sqrt(diag(as.matrix(v)))
    }))
  }, error = function(e) NULL)

  if (!is.null(per_se) && nrow(per_se) == m) {
    within_var <- colMeans(per_se^2, na.rm = TRUE)
    coefs_mat <- tryCatch({
      do.call(rbind, lapply(fits, function(f) {
        cv <- tryCatch(stats::coef(f), error = function(e) NULL)
        if (is.null(cv)) return(rep(NA_real_, length(cf)))
        as.numeric(cv)
      }))
    }, error = function(e) NULL)
    if (!is.null(coefs_mat) && nrow(coefs_mat) == m) {
      between_var <- apply(coefs_mat, 2, function(v) stats::var(v, na.rm = TRUE))
      total_var <- within_var + (1 + 1/m) * between_var
      riv <- ((1 + 1/m) * between_var) / pmax(within_var, .Machine$double.eps)
      lambda <- ((1 + 1/m) * between_var) / pmax(total_var, .Machine$double.eps)
      df_rubin <- (m - 1) * (1 + 1/riv)^2
      if (any(is.na(fmi))) {
        fmi <- (riv + 2 / (df_rubin + 3)) / (riv + 1)
      }
    } else {
      riv <- rep(NA_real_, length(cf))
      lambda <- rep(NA_real_, length(cf))
    }
  } else {
    riv <- rep(NA_real_, length(cf))
    lambda <- rep(NA_real_, length(cf))
  }

  zstat <- cf / pmax(se, .Machine$double.eps)
  # 2-sided p using normal approximation when df is unavailable
  pval <- if (any(is.finite(df_v))) {
    2 * stats::pt(-abs(zstat), df = pmax(df_v, 1, na.rm = TRUE))
  } else {
    2 * stats::pnorm(-abs(zstat))
  }

  term_names <- names(cf)
  if (is.null(term_names) || length(term_names) != length(cf))
    term_names <- paste0("term", seq_along(cf))

  out <- data.frame(
    term      = term_names,
    estimate  = cf,
    std.error = se,
    statistic = zstat,
    p.value   = pval,
    df        = df_v,
    fmi       = fmi,
    riv       = riv,
    lambda    = lambda,
    stringsAsFactors = FALSE
  )
  attr(out, "n_implicates") <- m
  attr(out, "model")        <- model
  attr(out, "fits")         <- fits
  attr(out, "combined")     <- combined
  out
}

# ---- Diagnostics -----------------------------------------------------
mi_diagnostics <- function(combined) {
  if (is.null(combined) || !is.data.frame(combined))
    return(list(message = "No pooled estimate available."))
  m <- attr(combined, "n_implicates") %||% NA_integer_
  list(
    n_implicates = m,
    model        = attr(combined, "model") %||% NA_character_,
    summary      = combined[, c("term","estimate","std.error",
                                 "fmi","lambda","df")],
    avg_fmi      = mean(combined$fmi, na.rm = TRUE),
    max_fmi      = suppressWarnings(max(combined$fmi, na.rm = TRUE)),
    avg_lambda   = mean(combined$lambda, na.rm = TRUE)
  )
}

# Internal helper duplicating .svy_zap behaviour without depending on
# load order. mod_survey.R defines an identical helper; this fallback
# is used by tests that source mi_pipeline.R standalone.
.svy_zap <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      lab <- attr(col, "labels", exact = TRUE)
      if (!is.null(lab) && length(lab) > 0 && length(lab) <= 50) {
        df[[nm]] <- factor(unclass(col), levels = unname(lab),
                            labels = names(lab))
      } else {
        df[[nm]] <- as.numeric(unclass(col))
      }
    }
  }
  df
}
