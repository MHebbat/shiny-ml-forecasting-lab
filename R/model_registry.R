# =====================================================================
# Model Registry
# Each model entry exposes:
#   id, label, engine ("R" | "Python"), task_types, description,
#   best_for (character vector), avoid_when (character vector),
#   long_doc (markdown string for the documentation drawer),
#   reference_url, dependencies (character vector of pkg/python module hints),
#   beta (logical, optional), groups (named list mapping
#     "Section title" -> character vector of param names),
#   params (list of UI specs),
#   available (function() -> list(ok=bool, msg=character)) — optional,
#     used by the Model Lab UI to show inline missing-dependency notices.
#   fit(df, target, params, time_col=NULL) -> list(model=..., predict=function(newdata, h)..., feat_imp=..., diagnostics=list(...))
#
# Author: Malik Hebbat
# =====================================================================

# Helper to declare a tuneable parameter for the UI
# `description` is a short explanation rendered as a help-tooltip / muted
# line under the control. Falls back to `help` when not provided.
P <- function(name, label, type = "numeric", default = NULL, min = NULL, max = NULL,
              step = NULL, choices = NULL, help = "", description = NULL) {
  list(name = name, label = label, type = type, default = default,
       min = min, max = max, step = step, choices = choices, help = help,
       description = description %||% help)
}

# Quick safe loader
.has <- function(pkg) requireNamespace(pkg, quietly = TRUE)

# Validate a weights vector aligned with nrow(df). Accepts NULL.
.weights_ok <- function(w, n) {
  if (is.null(w)) return(FALSE)
  w <- suppressWarnings(as.numeric(w))
  if (length(w) != n) return(FALSE)
  any(is.finite(w) & w > 0)
}

# Common availability helpers
avail_ok <- function() list(ok = TRUE, msg = NULL)
avail_pkg <- function(pkg, hint = NULL) {
  if (.has(pkg)) avail_ok()
  else list(ok = FALSE,
            msg = sprintf("%s not installed — run %s to enable",
                          pkg,
                          hint %||% sprintf("install.packages('%s')", pkg)))
}
avail_python <- function(mod, hint = NULL) {
  if (!py_is_available()) {
    return(list(ok = FALSE,
                msg = "Python not available — install reticulate and a Python runtime"))
  }
  if (!py_has_module(mod)) {
    return(list(ok = FALSE,
                msg = sprintf("Python module '%s' not installed — run %s to enable",
                              mod,
                              hint %||%
                                sprintf("reticulate::py_install('%s')", mod))))
  }
  avail_ok()
}

# ---- Parameter coercion helpers --------------------------------------
# Convert a comma-separated text input into a numeric vector. Empty -> NULL.
.parse_num_vec <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(as.character(txt)))) return(NULL)
  parts <- strsplit(as.character(txt), "[,;]\\s*")[[1]]
  v <- suppressWarnings(as.numeric(parts))
  v <- v[!is.na(v)]
  if (length(v) == 0) NULL else v
}
# "auto" / "" -> NULL, else integer
.parse_auto_int <- function(v) {
  if (is.null(v)) return(NULL)
  s <- trimws(as.character(v))
  if (!nzchar(s) || tolower(s) == "auto") return(NULL)
  as.integer(suppressWarnings(as.numeric(s)))
}
# "auto" / "" / NA -> NA (auto), "T"/"TRUE" -> TRUE, "F"/"FALSE" -> FALSE
.parse_auto_lgl <- function(v) {
  if (is.null(v)) return(NA)
  s <- trimws(toupper(as.character(v)))
  if (!nzchar(s) || s == "AUTO" || s == "NA") return(NA)
  if (s %in% c("T","TRUE","YES","Y","1")) return(TRUE)
  if (s %in% c("F","FALSE","NO","N","0")) return(FALSE)
  NA
}

# Parse a layer-width spec like "[12, 5, 1]" or "12, 5, 1" -> integer vector.
# Substitutes the literal token "n_features" with the runtime value when given.
.parse_layer_widths <- function(txt, n_features = NULL) {
  if (is.null(txt)) return(NULL)
  s <- trimws(as.character(txt))
  if (!nzchar(s)) return(NULL)
  s <- gsub("[\\[\\]\\(\\) ]", "", s, perl = TRUE)
  if (!is.null(n_features) && is.finite(n_features)) {
    s <- gsub("n_features", as.character(as.integer(n_features)), s,
              ignore.case = TRUE)
  }
  parts <- strsplit(s, "[,;]")[[1]]
  v <- suppressWarnings(as.integer(parts))
  v <- v[!is.na(v) & v > 0]
  if (length(v) == 0) NULL else v
}

# Build sequence (X, y) tensors for LSTM training from a univariate numeric
# vector or a multivariate matrix.
#
# Inputs:
#   y         numeric vector (univariate) OR numeric matrix [n_obs, n_features]
#   timesteps number of past steps to feed at each prediction
#   target_col when y is a matrix, the column index of the supervised target
#              (defaults to the LAST column).
#
# Returns list(
#   X       = array [N, timesteps, n_features],
#   y       = numeric vector length N,
#   N       = N,
#   n_features = n_features,
#   timesteps  = timesteps
# )
# Pure R; no keras dependency. Exposed (no leading dot) so it can be unit
# tested.
lstm_make_supervised <- function(y, timesteps, target_col = NULL) {
  if (!is.numeric(timesteps) || length(timesteps) != 1 ||
      !is.finite(timesteps) || timesteps < 1)
    stop("lstm_make_supervised: `timesteps` must be a positive integer")
  timesteps <- as.integer(timesteps)
  if (is.matrix(y) || is.data.frame(y)) {
    M <- as.matrix(y)
    storage.mode(M) <- "double"
    if (any(!is.finite(M)))
      stop("lstm_make_supervised: NA / non-finite values in input matrix")
    n <- nrow(M); n_features <- ncol(M)
    if (is.null(target_col)) target_col <- n_features
    if (n <= timesteps)
      stop("LSTM: series too short for the requested timesteps")
    N <- n - timesteps
    X <- array(0, dim = c(N, timesteps, n_features))
    yy <- numeric(N)
    for (i in seq_len(N)) {
      X[i, , ] <- M[i:(i + timesteps - 1L), , drop = FALSE]
      yy[i]    <- M[i + timesteps, target_col]
    }
    return(list(X = X, y = yy, N = N,
                n_features = n_features, timesteps = timesteps))
  }
  y <- as.numeric(y)
  n <- length(y)
  if (n <= timesteps) stop("LSTM: series too short for the requested timesteps")
  N <- n - timesteps
  X <- array(0, dim = c(N, timesteps, 1L))
  yy <- numeric(N)
  for (i in seq_len(N)) {
    X[i, , 1] <- y[i:(i + timesteps - 1L)]
    yy[i]     <- y[i + timesteps]
  }
  list(X = X, y = yy, N = N, n_features = 1L, timesteps = timesteps)
}
# Back-compat alias (older callers used the dot-prefixed private name).
.lstm_make_supervised <- lstm_make_supervised

# ---- Keras backend resolution ---------------------------------------
# Returns a list:
#   ok      TRUE if a usable backend was found
#   backend "keras3" | "keras" | NA
#   ns      a list with namespaced functions to call (allows uniform code
#           paths between keras3 and the legacy keras package)
#   reason  human-readable string when !ok
#
# We prefer keras3 (current CRAN package) and fall back to legacy keras.
# Both must additionally have a working TensorFlow Python backend.
.resolve_keras_backend <- function() {
  # Three-state predicate:
  #   1) keras3 R package not installed (and legacy keras absent too) → install
  #   2) keras3 R package installed BUT a Python TF runtime is not configured
  #      → user must run keras3::install_keras() once
  #   3) ready (R package + Python TF reachable)
  has_tf_via_reticulate <- function() {
    if (!requireNamespace("reticulate", quietly = TRUE)) return(FALSE)
    tryCatch(reticulate::py_module_available("tensorflow"),
             error = function(e) FALSE)
  }
  is_keras3_runtime_ready <- function() {
    # Prefer the package's own predicate when it exists; fall back to a
    # reticulate probe so we still work on older keras3 builds.
    via_pkg <- tryCatch(
      isTRUE(asNamespace("keras3")$is_keras_available()),
      error = function(e) NA)
    if (isTRUE(via_pkg)) return(TRUE)
    if (isFALSE(via_pkg)) return(FALSE)
    has_tf_via_reticulate()
  }
  if (.has("keras3")) {
    if (!is_keras3_runtime_ready()) return(list(ok = FALSE, backend = "keras3",
      ns = NULL, state = "installed_no_tf",
      reason = paste0("`keras3` R package is installed but a Python ",
        "TensorFlow runtime is not configured. ",
        "Run `keras3::install_keras()` once to set it up, then restart.")))
    return(list(ok = TRUE, backend = "keras3", state = "ready",
                ns = asNamespace("keras3"), reason = NULL))
  }
  if (.has("keras")) {
    if (!has_tf_via_reticulate()) return(list(ok = FALSE, backend = "keras",
      ns = NULL, state = "installed_no_tf",
      reason = paste0("`keras` R package is installed but a Python ",
        "TensorFlow runtime is not configured. ",
        "Run `keras::install_keras()` once to set it up, then restart.")))
    return(list(ok = TRUE, backend = "keras", state = "ready",
                ns = asNamespace("keras"), reason = NULL))
  }
  list(ok = FALSE, backend = NA, ns = NULL, state = "not_installed",
       reason = paste0("Neither `keras3` (preferred) nor `keras` is ",
         "installed. Run `install.packages('keras3'); ",
         "keras3::install_keras()` and restart."))
}

# ---- Model implementations -------------------------------------------

# Linear regression
fit_lm <- function(df, target, params, time_col = NULL) {
  .assert_train_shapes(df[[target]], df, "Linear regression")
  intercept <- isTRUE(params$intercept %||% TRUE)
  na_action <- params$na_action %||% "na.omit"
  rhs <- if (intercept) "." else ". - 1"
  f <- as.formula(paste(target, "~", rhs))
  lm_args <- list(formula = f, data = df,
                  na.action = switch(na_action,
                                      "na.exclude" = stats::na.exclude,
                                      stats::na.omit))
  if (.weights_ok(params$weights, nrow(df)))
    lm_args$weights <- as.numeric(params$weights)
  m <- do.call(stats::lm, lm_args)
  list(model = m,
       predict = function(newdata, h = NULL) as.numeric(predict(m, newdata = newdata)),
       feat_imp = NULL)
}

# Logistic regression
fit_logit <- function(df, target, params, time_col = NULL) {
  .assert_train_shapes(df[[target]], df, "Logistic regression")
  df[[target]] <- as.factor(df[[target]])
  link <- params$link %||% "logit"
  f <- as.formula(paste(target, "~ ."))
  glm_args <- list(formula = f, data = df,
                    family = binomial(link = link),
                    control = glm.control(
                      maxit  = as.integer(params$maxit %||% 50),
                      epsilon = as.numeric(params$epsilon %||% 1e-8)))
  if (.weights_ok(params$weights, nrow(df)))
    glm_args$weights <- as.numeric(params$weights)
  m <- do.call(stats::glm, glm_args)
  lev <- levels(df[[target]])
  list(model = m,
       predict = function(newdata, h = NULL) {
         p <- predict(m, newdata = newdata, type = "response")
         lev[ifelse(p > 0.5, 2, 1)]
       },
       feat_imp = NULL)
}

# Glmnet (regression or classification)
fit_glmnet <- function(df, target, params, time_col = NULL, family = "gaussian") {
  .assert_train_shapes(df[[target]], df, "glmnet")
  if (!.has("glmnet")) stop("Install 'glmnet'")
  x <- model.matrix(as.formula(paste(target, "~ . -1")), data = df)
  y <- df[[target]]
  train_cols <- colnames(x)
  train_df_cols <- setdiff(colnames(df), target)

  alpha       <- as.numeric(params$alpha %||% 1)
  nfolds      <- as.integer(params$nfolds %||% 5)
  nlambda     <- as.integer(params$nlambda %||% 100)
  standardize <- isTRUE(params$standardize %||% TRUE)
  lambda_user <- params$lambda %||% "auto"

  fit_args <- list(x = x, y = y, alpha = alpha, family = family,
                   nfolds = nfolds, nlambda = nlambda,
                   standardize = standardize)
  if (.weights_ok(params$weights, nrow(x)))
    fit_args$weights <- as.numeric(params$weights)
  # Optional explicit lambda (numeric or "auto")
  if (is.numeric(lambda_user) && length(lambda_user) >= 1 &&
      all(lambda_user >= 0)) {
    fit_args$lambda <- sort(unique(lambda_user), decreasing = TRUE)
  } else if (is.character(lambda_user) &&
             tolower(trimws(lambda_user)) != "auto" &&
             nzchar(lambda_user)) {
    parsed <- suppressWarnings(as.numeric(strsplit(lambda_user, "[,;]\\s*")[[1]]))
    parsed <- parsed[!is.na(parsed) & parsed >= 0]
    if (length(parsed) > 0) fit_args$lambda <- sort(unique(parsed), decreasing = TRUE)
  }

  m <- do.call(glmnet::cv.glmnet, fit_args)
  list(model = m,
       predict = function(newdata, h = NULL) {
         if (!target %in% colnames(newdata)) newdata[[target]] <- y[1]
         keep <- intersect(train_df_cols, colnames(newdata))
         newdata <- newdata[, c(keep, target), drop = FALSE]
         nx <- model.matrix(as.formula(paste(target, "~ . -1")), data = newdata)
         aligned <- matrix(0, nrow = nrow(nx), ncol = length(train_cols),
                            dimnames = list(NULL, train_cols))
         common <- intersect(colnames(nx), train_cols)
         if (length(common) > 0) aligned[, common] <- nx[, common, drop = FALSE]
         out <- predict(m, newx = aligned, s = "lambda.min",
                        type = if (family == "binomial") "class" else "response")
         as.numeric(out)
       },
       feat_imp = NULL)
}

# GAM
fit_gam <- function(df, target, params, time_col = NULL) {
  .assert_train_shapes(df[[target]], df, "GAM")
  if (!.has("mgcv")) stop("Install 'mgcv'")
  num_cols <- setdiff(names(df)[sapply(df, is.numeric)], target)
  k_val <- as.integer(params$k %||% -1)
  smooth_term <- function(col) {
    if (is.na(k_val) || k_val < 0) sprintf("s(%s)", col)
    else sprintf("s(%s, k=%d)", col, k_val)
  }
  smooth_terms <- if (length(num_cols) > 0)
    paste(vapply(num_cols, smooth_term, character(1)), collapse = " + ") else "1"
  cat_cols <- setdiff(names(df), c(num_cols, target))
  rhs <- if (length(cat_cols) > 0) paste(smooth_terms, "+", paste(cat_cols, collapse = " + ")) else smooth_terms

  method  <- params$method %||% "REML"
  do_select <- isTRUE(params$select)
  m <- mgcv::gam(as.formula(paste(target, "~", rhs)), data = df,
                 select = do_select,
                 method = method)
  list(model = m,
       predict = function(newdata, h = NULL) as.numeric(predict(m, newdata = newdata)),
       feat_imp = NULL)
}

# Random forest via ranger
fit_ranger <- function(df, target, params, time_col = NULL, classification = FALSE) {
  .assert_train_shapes(df[[target]], df, "Random forest (ranger)")
  if (!.has("ranger")) stop("Install 'ranger'")
  if (classification) df[[target]] <- as.factor(df[[target]])

  num_trees     <- as.integer(params$num_trees %||% 500)
  min_node_size <- as.integer(params$min_node_size %||%
                                (if (classification) 1 else 5))
  max_depth     <- as.integer(params$max_depth %||% 0)
  sample_frac   <- as.numeric(params$sample_fraction %||% 1)
  splitrule_in  <- params$splitrule %||% "default"
  splitrule <- if (identical(tolower(splitrule_in), "default") ||
                   !nzchar(as.character(splitrule_in))) {
    if (classification) "gini" else "variance"
  } else as.character(splitrule_in)

  mtry_in <- params$mtry %||% "auto"
  mtry_val <- .parse_auto_int(mtry_in)

  rf_args <- list(
    dependent.variable.name = target,
    data = df,
    num.trees = num_trees,
    min.node.size = min_node_size,
    importance = "impurity",
    classification = classification,
    sample.fraction = sample_frac,
    splitrule = splitrule
  )
  if (!is.null(mtry_val) && !is.na(mtry_val)) rf_args$mtry <- mtry_val
  if (!is.na(max_depth) && max_depth > 0) rf_args$max.depth <- max_depth
  if (.weights_ok(params$weights, nrow(df)))
    rf_args$case.weights <- as.numeric(params$weights)

  m <- do.call(ranger::ranger, rf_args)
  list(model = m,
       predict = function(newdata, h = NULL) {
         p <- predict(m, data = newdata)$predictions
         if (classification) as.character(p) else as.numeric(p)
       },
       feat_imp = sort(m$variable.importance, decreasing = TRUE))
}

# XGBoost
fit_xgb <- function(df, target, params, time_col = NULL, classification = FALSE) {
  .assert_train_shapes(df[[target]], df, "XGBoost")
  if (!.has("xgboost")) stop("Install 'xgboost'")
  y <- df[[target]]
  if (classification && is.character(y)) y <- as.factor(y)
  X <- model.matrix(as.formula(paste(target, "~ . -1")), data = df)
  train_cols <- colnames(X)
  train_df_cols <- setdiff(colnames(df), target)
  obj <- if (classification) {
    if (length(unique(y)) == 2) "binary:logistic" else "multi:softprob"
  } else "reg:squarederror"
  yy <- if (classification) as.integer(as.factor(y)) - 1 else as.numeric(y)
  num_class <- if (classification && length(unique(y)) > 2) length(unique(y)) else NULL

  tree_method_in <- params$tree_method %||% "auto"
  tree_method <- if (!nzchar(as.character(tree_method_in)) ||
                     identical(tolower(tree_method_in), "auto")) "auto"
                 else as.character(tree_method_in)

  parm <- list(
    objective        = obj,
    eta              = as.numeric(params$eta %||% 0.1),
    max_depth        = as.integer(params$max_depth %||% 6),
    subsample        = as.numeric(params$subsample %||% 0.9),
    colsample_bytree = as.numeric(params$colsample_bytree %||% 0.9),
    min_child_weight = as.numeric(params$min_child_weight %||% 1),
    gamma            = as.numeric(params$gamma %||% 0),
    lambda           = as.numeric(params$lambda %||% 1),
    alpha            = as.numeric(params$alpha %||% 0),
    tree_method      = tree_method
  )
  if (!is.null(num_class)) parm$num_class <- num_class

  early_stop <- as.integer(params$early_stopping_rounds %||% 0)
  xg_args <- list(
    data = X, label = yy, params = parm,
    nrounds = as.integer(params$nrounds %||% 200),
    verbose = 0
  )
  if (.weights_ok(params$weights, nrow(X)))
    xg_args$weight <- as.numeric(params$weights)
  if (!is.na(early_stop) && early_stop > 0) {
    # xgboost requires a watchlist for early stopping; use training data so it
    # remains a valid signal even without a separate eval set.
    xg_args$early_stopping_rounds <- early_stop
    xg_args$watchlist <- list(train = xgboost::xgb.DMatrix(X, label = yy))
  }
  m <- do.call(xgboost::xgboost, xg_args)
  imp <- tryCatch(xgboost::xgb.importance(model = m), error = function(e) NULL)
  list(model = m,
       predict = function(newdata, h = NULL) {
         if (!target %in% colnames(newdata)) newdata[[target]] <- y[1]
         keep <- intersect(train_df_cols, colnames(newdata))
         newdata <- newdata[, c(keep, target), drop = FALSE]
         for (col in keep) {
           if (is.character(newdata[[col]])) newdata[[col]] <- as.factor(newdata[[col]])
         }
         nx <- tryCatch(
           model.matrix(as.formula(paste(target, "~ . -1")), data = newdata),
           error = function(e) matrix(0, nrow = nrow(newdata), ncol = 0)
         )
         aligned <- matrix(0, nrow = nrow(newdata), ncol = length(train_cols),
                            dimnames = list(NULL, train_cols))
         common <- intersect(colnames(nx), train_cols)
         if (length(common) > 0) aligned[, common] <- nx[, common, drop = FALSE]
         p <- predict(m, aligned)
         if (classification && length(unique(y)) == 2) {
           lev <- levels(as.factor(y))
           lev[ifelse(p > 0.5, 2, 1)]
         } else if (classification) {
           lev <- levels(as.factor(y))
           mat <- matrix(p, ncol = length(lev), byrow = TRUE)
           lev[apply(mat, 1, which.max)]
         } else as.numeric(p)
       },
       feat_imp = if (!is.null(imp)) setNames(imp$Gain, imp$Feature) else NULL)
}

# Poisson / NegBin
fit_poisson <- function(df, target, params, time_col = NULL) {
  .assert_train_shapes(df[[target]], df, "Poisson GLM")
  link <- params$link %||% "log"
  glm_args <- list(formula = as.formula(paste(target, "~ .")), data = df,
                    family = poisson(link = link),
                    control = glm.control(
                      maxit  = as.integer(params$maxit %||% 50),
                      epsilon = as.numeric(params$epsilon %||% 1e-8)))
  if (.weights_ok(params$weights, nrow(df)))
    glm_args$weights <- as.numeric(params$weights)
  m <- do.call(stats::glm, glm_args)
  list(model = m,
       predict = function(newdata, h = NULL) as.numeric(predict(m, newdata = newdata, type = "response")),
       feat_imp = NULL)
}
fit_negbin <- function(df, target, params, time_col = NULL) {
  .assert_train_shapes(df[[target]], df, "Negative binomial")
  if (!.has("MASS")) stop("Install 'MASS'")
  link <- params$link %||% "log"
  m <- MASS::glm.nb(as.formula(paste(target, "~ .")), data = df,
                    link = link,
                    control = glm.control(
                      maxit  = as.integer(params$maxit %||% 50),
                      epsilon = as.numeric(params$epsilon %||% 1e-8)))
  list(model = m,
       predict = function(newdata, h = NULL) as.numeric(predict(m, newdata = newdata, type = "response")),
       feat_imp = NULL)
}

# Beta regression
fit_betareg <- function(df, target, params, time_col = NULL) {
  .assert_train_shapes(df[[target]], df, "Beta regression")
  if (!.has("betareg")) stop("Install 'betareg'")
  link     <- params$link %||% "logit"
  link_phi <- params$link_phi %||% "log"
  m <- betareg::betareg(as.formula(paste(target, "~ .")), data = df,
                         link = link, link.phi = link_phi)
  list(model = m,
       predict = function(newdata, h = NULL) as.numeric(predict(m, newdata = newdata)),
       feat_imp = NULL)
}

# ---- Time series ------------------------------------------------------
# ARIMA via forecast
fit_arima <- function(df, target, params, time_col = NULL) {
  .assert_train_shapes(df[[target]], df, "ARIMA")
  if (!.has("forecast")) stop("Install 'forecast'")
  y <- as.numeric(df[[target]])
  freq <- as.numeric(params$ts_frequency %||% 12)
  ts_y <- ts(y, frequency = freq)

  D_in <- params$D %||% "auto"
  D_val <- if (identical(tolower(as.character(D_in)), "auto") ||
               !nzchar(as.character(D_in))) NA
           else as.integer(D_in)

  m <- forecast::auto.arima(
    ts_y,
    seasonal     = isTRUE(params$seasonal %||% TRUE),
    stepwise     = isTRUE(params$stepwise %||% TRUE),
    approximation = isTRUE(params$approximation %||% TRUE),
    max.p = as.integer(params$max_p %||% 5),
    max.q = as.integer(params$max_q %||% 5),
    max.P = as.integer(params$max_P %||% 2),
    max.Q = as.integer(params$max_Q %||% 2),
    D     = D_val,
    ic    = params$ic %||% "aicc"
  )
  list(model = m,
       predict = function(newdata = NULL, h = 12) {
         fc <- forecast::forecast(m, h = h)
         data.frame(predicted = as.numeric(fc$mean),
                    lower = as.numeric(fc$lower[, 2]),
                    upper = as.numeric(fc$upper[, 2]))
       },
       feat_imp = NULL)
}

# ETS
fit_ets <- function(df, target, params, time_col = NULL) {
  .assert_train_shapes(df[[target]], df, "ETS")
  if (!.has("forecast")) stop("Install 'forecast'")
  y <- as.numeric(df[[target]])
  freq <- as.numeric(params$ts_frequency %||% 12)
  ts_y <- ts(y, frequency = freq)

  damped_in <- params$damped %||% "auto"
  damped <- .parse_auto_lgl(damped_in)
  ets_args <- list(
    y = ts_y,
    model = params$ets_model %||% "ZZZ",
    opt.crit = params$opt_crit %||% "lik"
  )
  if (!is.na(damped)) ets_args$damped <- damped
  m <- do.call(forecast::ets, ets_args)
  list(model = m,
       predict = function(newdata = NULL, h = 12) {
         fc <- forecast::forecast(m, h = h)
         data.frame(predicted = as.numeric(fc$mean),
                    lower = as.numeric(fc$lower[, 2]),
                    upper = as.numeric(fc$upper[, 2]))
       },
       feat_imp = NULL)
}

# TBATS
fit_tbats <- function(df, target, params, time_col = NULL) {
  .assert_train_shapes(df[[target]], df, "TBATS")
  if (!.has("forecast")) stop("Install 'forecast'")
  y <- as.numeric(df[[target]])
  freq <- as.numeric(params$ts_frequency %||% 12)

  seasonal_periods <- .parse_num_vec(params$seasonal_periods)
  ts_y <- if (!is.null(seasonal_periods) && length(seasonal_periods) > 0) {
    forecast::msts(y, seasonal.periods = seasonal_periods)
  } else ts(y, frequency = freq)

  use_box  <- .parse_auto_lgl(params$use_box_cox %||% "auto")
  use_trd  <- .parse_auto_lgl(params$use_trend %||% "auto")
  use_dmp  <- .parse_auto_lgl(params$use_damped_trend %||% "auto")

  tbats_args <- list(y = ts_y)
  if (!is.na(use_box)) tbats_args$use.box.cox <- use_box
  if (!is.na(use_trd)) tbats_args$use.trend <- use_trd
  if (!is.na(use_dmp)) tbats_args$use.damped.trend <- use_dmp

  m <- do.call(forecast::tbats, tbats_args)
  list(model = m,
       predict = function(newdata = NULL, h = 12) {
         fc <- forecast::forecast(m, h = h)
         data.frame(predicted = as.numeric(fc$mean),
                    lower = as.numeric(fc$lower[, 2]),
                    upper = as.numeric(fc$upper[, 2]))
       },
       feat_imp = NULL)
}

# Prophet (optional)
fit_prophet <- function(df, target, params, time_col = NULL) {
  .assert_train_shapes(df[[target]], df, "Prophet")
  if (!.has("prophet"))
    stop("Prophet not installed - run install.packages('prophet') to enable")
  if (is.null(time_col)) stop("Prophet requires a time column")
  d <- data.frame(
    ds = safe_as_date(df[[time_col]], column_name = time_col),
    y  = as.numeric(df[[target]]))
  if (all(is.na(d$ds)))
    stop(sprintf("Prophet: column '%s' could not be parsed as a date. ",
                 time_col),
         "Use Data Prep -> parse_datetime to declare its format.")

  yearly <- .parse_auto_lgl(params$yearly %||% "auto")
  weekly <- .parse_auto_lgl(params$weekly %||% "auto")
  daily  <- .parse_auto_lgl(params$daily %||% "FALSE")
  yearly_arg <- if (is.na(yearly)) "auto" else yearly
  weekly_arg <- if (is.na(weekly)) "auto" else weekly
  daily_arg  <- if (is.na(daily))  "auto" else daily

  growth <- params$growth %||% "linear"
  if (growth == "logistic") {
    cap <- max(d$y, na.rm = TRUE)
    if (!is.finite(cap) || cap == 0) cap <- 1
    d$cap <- cap * 1.5
    d$floor <- 0
  }

  m <- prophet::prophet(
    d,
    growth = growth,
    seasonality.mode         = params$seasonality_mode %||% "additive",
    changepoint.prior.scale  = as.numeric(params$changepoint_prior_scale %||% 0.05),
    seasonality.prior.scale  = as.numeric(params$seasonality_prior_scale %||% 10),
    holidays.prior.scale     = as.numeric(params$holidays_prior_scale %||% 10),
    n.changepoints           = as.integer(params$n_changepoints %||% 25),
    yearly.seasonality       = yearly_arg,
    weekly.seasonality       = weekly_arg,
    daily.seasonality        = daily_arg
  )
  list(model = m,
       predict = function(newdata = NULL, h = 12) {
         fut <- prophet::make_future_dataframe(m, periods = h,
                                               freq = params$prophet_freq %||% "month")
         if (growth == "logistic") {
           fut$cap   <- max(d$cap, na.rm = TRUE)
           fut$floor <- 0
         }
         fc <- predict(m, fut)
         tail(data.frame(predicted = fc$yhat, lower = fc$yhat_lower, upper = fc$yhat_upper), h)
       },
       feat_imp = NULL)
}

# ---- Python-backed wrappers -------------------------------------------
# LightGBM via reticulate
fit_lightgbm_py <- function(df, target, params, time_col = NULL, classification = FALSE) {
  .assert_train_shapes(df[[target]], df, "LightGBM")
  if (!py_is_available()) stop("Python not available; install reticulate + lightgbm")
  task <- if (classification) "classification" else "regression"
  X <- df[, setdiff(names(df), target), drop = FALSE]
  y <- df[[target]]
  fitted_X <- X
  list(
    model = list(engine = "lightgbm_py", params = params),
    predict = function(newdata, h = NULL) {
      nx <- newdata[, intersect(names(fitted_X), names(newdata)), drop = FALSE]
      out <- py_train_predict("lightgbm",
        x_train = X, y_train = y, x_test = nx,
        params = params, task = task)
      out$predictions
    },
    feat_imp = NULL
  )
}

# CatBoost via reticulate
fit_catboost_py <- function(df, target, params, time_col = NULL, classification = FALSE) {
  .assert_train_shapes(df[[target]], df, "CatBoost")
  if (!py_is_available()) stop("Python not available; install reticulate + catboost")
  task <- if (classification) "classification" else "regression"
  X <- df[, setdiff(names(df), target), drop = FALSE]
  y <- df[[target]]
  fitted_X <- X
  list(
    model = list(engine = "catboost_py", params = params),
    predict = function(newdata, h = NULL) {
      nx <- newdata[, intersect(names(fitted_X), names(newdata)), drop = FALSE]
      out <- py_train_predict("catboost",
        x_train = X, y_train = y, x_test = nx,
        params = params, task = task)
      out$predictions
    },
    feat_imp = NULL
  )
}

# ---- LSTM (deep learning) --------------------------------------------
# Backends, in order of preference:
#   - keras3 (CRAN, current): full-featured, recommended.
#   - keras  (legacy RStudio): full-featured fallback.
#   - TSLSTMplus: univariate-only convenience fallback for time-series.
# fit_lstm dispatches between them based on availability and task.
fit_lstm <- function(df, target, params, time_col = NULL, classification = FALSE) {
  .assert_train_shapes(df[[target]], df, "LSTM")
  task <- if (classification) "classification" else "time_series"

  k <- .resolve_keras_backend()       # list(ok, backend, ns, reason)
  use_tslstm <- .has("TSLSTMplus")

  timesteps <- as.integer(params$timesteps %||% 12)
  val_split <- as.numeric(params$validation_split %||% 0)
  .nrow_df <- nrow(df)
  .effective_train <- floor(.nrow_df * (1 - max(0, min(0.99, val_split))))
  if (!is.na(timesteps) && timesteps >= .effective_train)
    stop(sprintf(
      "LSTM: timesteps (%d) must be smaller than the effective training length (%d rows). Reduce timesteps or lower validation_split.",
      timesteps, .effective_train), call. = FALSE)
  bidirectional <- isTRUE(params$bidirectional %||% FALSE)
  units <- as.integer(params$units %||% 64)
  layers_n <- as.integer(params$layers %||% 1)
  dropout <- as.numeric(params$dropout %||% 0)
  rdrop <- as.numeric(params$recurrent_dropout %||% 0)
  batch <- as.integer(params$batch_size %||% 32)
  epochs <- as.integer(params$epochs %||% 100)
  lr <- as.numeric(params$learning_rate %||% 1e-3)
  opt <- as.character(params$optimizer %||% "adam")
  loss_in <- as.character(params$loss %||% "auto")
  patience <- as.integer(params$early_stopping_patience %||% 10)
  sched <- as.character(params$lr_scheduler %||% "none")
  stateful <- isTRUE(params$stateful %||% FALSE)
  return_sequences <- isTRUE(params$return_sequences %||% FALSE)
  seed <- as.integer(params$seed %||% 42)

  loss_for_task <- function() {
    if (loss_in != "auto" && nzchar(loss_in)) return(loss_in)
    if (classification) "sparse_categorical_crossentropy" else "mse"
  }

  # Uniform getter against either keras3 or keras namespace.
  K <- function(name) {
    if (is.null(k$ns)) stop("Keras backend not available")
    fn <- get0(name, envir = k$ns, mode = "function")
    if (is.null(fn)) stop(sprintf("Function `%s` missing in backend `%s`",
                                   name, k$backend))
    fn
  }

  set_seeds <- function() {
    set.seed(seed)
    # Both keras3 and keras expose set_random_seed; keras3 is the
    # documented modern API. Fall back to use_session_with_seed only if
    # set_random_seed is missing (very old keras).
    fn <- get0("set_random_seed", envir = k$ns, mode = "function")
    if (!is.null(fn)) tryCatch(fn(seed), error = function(e) NULL)
    else {
      fn <- get0("use_session_with_seed", envir = k$ns, mode = "function")
      if (!is.null(fn))
        tryCatch(fn(seed, disable_parallel_cpu = FALSE),
                 error = function(e) NULL)
    }
  }

  # ---- Path 1: keras3 / keras ----------------------------------------
  if (isTRUE(k$ok)) {
    out <- tryCatch({
      set_seeds()
      y_raw <- df[[target]]
      build_optimizer <- function() {
        switch(opt,
          "sgd"     = K("optimizer_sgd")(learning_rate = lr),
          "rmsprop" = K("optimizer_rmsprop")(learning_rate = lr),
          "adamw"   = {
            f <- get0("optimizer_adamw", envir = k$ns, mode = "function")
            if (is.null(f)) K("optimizer_adam")(learning_rate = lr)
            else f(learning_rate = lr)
          },
          K("optimizer_adam")(learning_rate = lr))
      }
      build_lstm_layer <- function(rs) {
        K("layer_lstm")(units = units, return_sequences = rs,
                          dropout = dropout, recurrent_dropout = rdrop,
                          stateful = stateful)
      }
      build_callbacks <- function() {
        cb <- list()
        if (patience > 0)
          cb <- c(cb, list(K("callback_early_stopping")(
            monitor = "loss", patience = patience,
            restore_best_weights = TRUE)))
        if (sched == "reduce_on_plateau")
          cb <- c(cb, list(K("callback_reduce_lr_on_plateau")(
            monitor = "loss", patience = max(1L, patience %/% 2L))))
        cb
      }
      if (classification) {
        # Sequence classification on a flat numeric matrix; multivariate
        # supported. Encode classes for sparse loss.
        classes <- sort(unique(stats::na.omit(y_raw)))
        y_idx <- match(y_raw, classes) - 1L
        feat_cols <- setdiff(names(df), target)
        X_tab <- if (length(feat_cols) > 0)
          as.matrix(as.data.frame(lapply(df[, feat_cols, drop = FALSE],
            function(c) if (is.factor(c)||is.character(c))
              as.integer(as.factor(c)) else as.numeric(c))))
        else
          matrix(seq_along(y_idx), ncol = 1)
        n_features <- max(1L, ncol(X_tab))
        # Reshape to [samples, timesteps=1, n_features]
        Xtr <- array(as.numeric(X_tab), dim = c(nrow(X_tab), 1L, n_features))
        ytr <- as.integer(y_idx)
        n_classes <- length(classes)

        inp <- K("layer_input")(shape = c(1L, n_features))
        z <- inp
        for (li in seq_len(layers_n)) {
          rs <- if (li < layers_n) TRUE else return_sequences
          rec <- build_lstm_layer(rs)
          z <- if (bidirectional) K("bidirectional")(z, rec) else rec(z)
        }
        out_layer <- K("layer_dense")(units = n_classes, activation = "softmax")
        out_t <- out_layer(z)
        model <- K("keras_model")(inp, out_t)
        model$compile(optimizer = build_optimizer(),
                      loss = loss_for_task(),
                      metrics = list("accuracy"))
        .assert_lstm_shapes(Xtr, ytr, model_label = "LSTM (classification)")
        Xtr_t <- lstm_as_float_tensor(Xtr, backend = k$backend)
        ytr_t <- lstm_as_int_tensor(ytr,   backend = k$backend)
        hist <- model$fit(x = Xtr_t, y = ytr_t,
                          epochs = epochs, batch_size = batch,
                          verbose = 0L, callbacks = build_callbacks())

        list(model = model,
             predict = function(newdata, h = NULL) {
               if (length(feat_cols) > 0) {
                 keep <- intersect(feat_cols, colnames(newdata))
                 nx <- as.data.frame(lapply(newdata[, keep, drop = FALSE],
                   function(c) if (is.factor(c)||is.character(c))
                     as.integer(as.factor(c)) else as.numeric(c)))
                 for (m in setdiff(feat_cols, keep)) nx[[m]] <- 0
                 nx <- nx[, feat_cols, drop = FALSE]
                 Xn <- array(as.numeric(as.matrix(nx)),
                             dim = c(nrow(nx), 1L, n_features))
               } else {
                 Xn <- array(seq_len(nrow(newdata)),
                             dim = c(nrow(newdata), 1L, 1L))
               }
               Xn_t <- lstm_as_float_tensor(Xn, backend = k$backend)
               probs <- model$predict(Xn_t, verbose = 0L)
               idx <- apply(probs, 1, which.max)
               classes[idx]
             },
             feat_imp = NULL,
             diagnostics = list(
               backend = k$backend,
               epochs_run = length(hist$history$loss %||% list()),
               final_loss = utils::tail(hist$history$loss %||% list(NA), 1)[[1]],
               history = hist$history))
      } else {
        # Time series / regression-on-sequence. Supports either univariate
        # (target only) or multivariate (target + numeric features).
        feat_cols <- setdiff(names(df), c(target, time_col %||% character(0)))
        feat_cols <- feat_cols[vapply(feat_cols,
          function(nm) is.numeric(df[[nm]]) || is.logical(df[[nm]]),
          logical(1))]
        if (length(feat_cols) > 0) {
          M <- as.matrix(cbind(
            as.data.frame(lapply(df[, feat_cols, drop = FALSE], as.numeric)),
            setNames(list(as.numeric(df[[target]])), target)))
          # Drop incomplete rows
          ok <- complete.cases(M)
          M <- M[ok, , drop = FALSE]
          sup <- lstm_make_supervised(M, timesteps,
                                       target_col = ncol(M))
        } else {
          y <- as.numeric(stats::na.omit(df[[target]]))
          sup <- lstm_make_supervised(y, timesteps)
        }
        n_features <- sup$n_features

        inp <- K("layer_input")(shape = c(timesteps, n_features))
        z <- inp
        for (li in seq_len(layers_n)) {
          rs <- if (li < layers_n) TRUE else return_sequences
          rec <- build_lstm_layer(rs)
          z <- if (bidirectional) K("bidirectional")(z, rec) else rec(z)
        }
        out_t <- K("layer_dense")(units = 1L)(z)
        model <- K("keras_model")(inp, out_t)
        model$compile(optimizer = build_optimizer(), loss = loss_for_task())
        .assert_lstm_shapes(sup$X, sup$y, model_label = "LSTM")
        Xtr_t <- lstm_as_float_tensor(sup$X, backend = k$backend)
        ytr_t <- lstm_as_float_tensor(sup$y, backend = k$backend)
        hist <- model$fit(x = Xtr_t, y = ytr_t,
                          epochs = epochs, batch_size = batch,
                          verbose = 0L, callbacks = build_callbacks())

        # Cache the last training window for recursive forecasting
        last_window <- if (n_features == 1L) {
          utils::tail(as.numeric(sup$y), timesteps - 1)
          # use original supervised tail: rebuild from sup
          tmp <- if (sup$N > 0) {
            c(sup$X[sup$N, , 1], sup$y[sup$N])
          } else stop("LSTM: empty supervised set")
          utils::tail(tmp, timesteps)
        } else {
          # Multivariate: reuse the last `timesteps` rows of the input matrix
          M <- as.matrix(cbind(
            as.data.frame(lapply(df[, feat_cols, drop = FALSE], as.numeric)),
            setNames(list(as.numeric(df[[target]])), target)))
          ok <- complete.cases(M)
          M <- M[ok, , drop = FALSE]
          utils::tail(M, timesteps)
        }

        list(model = model,
             predict = function(newdata = NULL, h = 12) {
               yhat <- numeric(0)
               if (n_features == 1L) {
                 win <- last_window
                 for (i in seq_len(h)) {
                   Xn <- array(win, dim = c(1L, timesteps, 1L))
                   Xn_t <- lstm_as_float_tensor(Xn, backend = k$backend)
                   p <- as.numeric(model$predict(Xn_t, verbose = 0L))[1]
                   yhat <- c(yhat, p)
                   win <- c(win[-1], p)
                 }
               } else {
                 # Recursive forecast: at each step, predict next y, then
                 # roll the window forward holding feature columns at their
                 # last observed values (a common, conservative assumption).
                 win <- last_window
                 last_feat <- win[nrow(win), -ncol(win)]
                 for (i in seq_len(h)) {
                   Xn <- array(as.numeric(win),
                               dim = c(1L, timesteps, n_features))
                   Xn_t <- lstm_as_float_tensor(Xn, backend = k$backend)
                   p <- as.numeric(model$predict(Xn_t, verbose = 0L))[1]
                   yhat <- c(yhat, p)
                   new_row <- c(last_feat, p)
                   win <- rbind(win[-1, , drop = FALSE], new_row)
                 }
               }
               data.frame(predicted = yhat, lower = NA_real_, upper = NA_real_)
             },
             feat_imp = NULL,
             diagnostics = list(
               backend = k$backend,
               timesteps = timesteps,
               n_features = n_features,
               epochs_run = length(hist$history$loss %||% list()),
               final_loss = utils::tail(hist$history$loss %||% list(NA), 1)[[1]],
               history = hist$history))
      }
    }, error = function(e) {
      py_detail <- tryCatch({
        if (requireNamespace("reticulate", quietly = TRUE)) {
          pe <- reticulate::py_last_error()
          if (!is.null(pe)) {
            paste0("\n  py_last_error: ",
                   pe$type %||% "", ": ", pe$value %||% "",
                   if (!is.null(pe$message)) paste0(" | ", pe$message) else "")
          } else ""
        } else ""
      }, error = function(e2) "")
      message("LSTM keras backend (", k$backend, ") failed: ",
              conditionMessage(e), py_detail,
              " - falling back to TSLSTMplus if available.")
      NULL
    })
    if (!is.null(out)) return(out)
  }

  # ---- Path 2: TSLSTMplus fallback ------------------------------------
  if (use_tslstm && task == "time_series") {
    y <- as.numeric(df[[target]])
    m <- TSLSTMplus::ts.lstm(
      ts = y,
      tsLag = timesteps,
      LSTMUnits = units,
      Epochs = epochs,
      BatchSize = batch
    )
    return(list(
      model = m,
      predict = function(newdata = NULL, h = 12) {
        fc <- tryCatch(
          TSLSTMplus::predict(m, h = h),
          error = function(e) rep(utils::tail(y, 1), h))
        data.frame(predicted = as.numeric(fc), lower = NA_real_, upper = NA_real_)
      },
      feat_imp = NULL,
      diagnostics = list(backend = "TSLSTMplus", timesteps = timesteps)
    ))
  }

  reason <- if (!isTRUE(k$ok)) k$reason else
    "Neither `keras3`/`keras` nor `TSLSTMplus` is usable in this session."
  stop(paste0("LSTM unavailable: ", reason,
              "\n  Recommended setup: install.packages('keras3'); ",
              "keras3::install_keras(); then restart the app.",
              "\n  Univariate-only fallback: install.packages('TSLSTMplus')."))
}

# ---- KAN (Kolmogorov-Arnold Network) ---------------------------------
fit_kan <- function(df, target, params, time_col = NULL, classification = FALSE) {
  .assert_train_shapes(df[[target]], df, "KAN")
  if (!py_is_available())
    stop("KAN requires Python; run reticulate::py_install(c('pykan','torch'))")
  task <- if (classification) "classification" else "regression"
  feat_cols <- setdiff(names(df), target)
  X <- df[, feat_cols, drop = FALSE]
  X <- as.data.frame(lapply(X, function(c) if (is.factor(c)||is.character(c))
                                              as.integer(as.factor(c))
                                            else as.numeric(c)))
  y <- df[[target]]
  n_features <- max(1L, ncol(X))

  width <- .parse_layer_widths(params$width %||%
    sprintf("[%d, 5, 1]", n_features), n_features = n_features)
  if (is.null(width) || length(width) < 2) width <- c(n_features, 5L, 1L)

  py_params <- list(
    width            = as.integer(width),
    grid             = as.integer(params$grid %||% 5),
    k                = as.integer(params$k %||% 3),
    seed             = as.integer(params$seed %||% 0),
    lamb             = as.numeric(params$lamb %||% 0),
    lamb_l1          = as.numeric(params$lamb_l1 %||% 1),
    lamb_entropy     = as.numeric(params$lamb_entropy %||% 2),
    lamb_coef        = as.numeric(params$lamb_coef %||% 0),
    lamb_coefdiff    = as.numeric(params$lamb_coefdiff %||% 0),
    steps            = as.integer(params$steps %||% 100),
    optimizer        = as.character(params$optimizer %||% "LBFGS"),
    learning_rate    = as.numeric(params$learning_rate %||% 1e-2),
    prune_threshold  = as.numeric(params$prune_threshold %||% 1e-2),
    plot_functions   = isTRUE(params$plot_functions %||% FALSE)
  )

  fitted_X <- X
  list(
    model = list(engine = "kan_py", params = py_params),
    predict = function(newdata, h = NULL) {
      keep <- intersect(names(fitted_X), names(newdata))
      nx <- newdata[, keep, drop = FALSE]
      nx <- as.data.frame(lapply(nx, function(c) if (is.factor(c)||is.character(c))
                                                    as.integer(as.factor(c))
                                                  else as.numeric(c)))
      for (m in setdiff(names(fitted_X), keep)) nx[[m]] <- 0
      nx <- nx[, names(fitted_X), drop = FALSE]
      out <- py_train_predict("kan",
        x_train = X, y_train = y, x_test = nx,
        params = py_params, task = task)
      out$predictions
    },
    feat_imp = NULL,
    diagnostics = list(backend = "pykan", width = width)
  )
}

# Keras MLP via reticulate (Python TF)
fit_mlp_py <- function(df, target, params, time_col = NULL, classification = FALSE) {
  .assert_train_shapes(df[[target]], df, "Keras MLP")
  if (!py_is_available()) stop("Python not available; install reticulate + tensorflow")
  task <- if (classification) "classification" else "regression"
  X <- df[, setdiff(names(df), target), drop = FALSE]
  X <- as.data.frame(lapply(X, function(c) if (is.factor(c) || is.character(c)) as.integer(as.factor(c)) else as.numeric(c)))
  y <- df[[target]]
  fitted_X <- X
  list(
    model = list(engine = "mlp_py", params = params),
    predict = function(newdata, h = NULL) {
      nx <- newdata[, intersect(names(fitted_X), names(newdata)), drop = FALSE]
      nx <- as.data.frame(lapply(nx, function(c) if (is.factor(c)||is.character(c)) as.integer(as.factor(c)) else as.numeric(c)))
      out <- py_train_predict("mlp",
        x_train = X, y_train = y, x_test = nx,
        params = params, task = task)
      out$predictions
    },
    feat_imp = NULL
  )
}

# ---- Registry definition ---------------------------------------------
MODELS <- list(

  # ---- Regression ----------------------------------------------------
  lm = list(id = "lm", label = "Linear Regression", engine = "R",
            task_types = c("regression"), fn = fit_lm,
            available = function() avail_ok(),
            description = paste(
              "Ordinary least-squares linear regression — the classical statistical baseline.",
              "Fast, fully interpretable (each coefficient is a marginal effect), and the right starting point",
              "for any numeric target. Assumes a roughly linear relationship between predictors and target,",
              "approximately Gaussian residuals, and no severe multicollinearity. Use it as a sanity check",
              "before reaching for fancier models — if a tuned XGBoost only marginally beats lm, your problem",
              "is probably linear and the simpler model wins on interpretability and stability."),
            params = list(
              P("intercept", "Include intercept", "logical", TRUE,
                description = "Fit a constant term (turn off only when predictors are pre-centered)."),
              P("na_action", "NA handling", "select", "na.omit",
                choices = c("na.omit", "na.exclude"),
                description = "How to treat rows with missing values during fitting."))),

  glmnet_reg = list(id = "glmnet_reg", label = "Elastic Net (glmnet)", engine = "R",
            task_types = c("regression"),
            fn = function(df, t, p, time_col=NULL) fit_glmnet(df, t, p, time_col, "gaussian"),
            available = function() avail_pkg("glmnet"),
            description = paste(
              "Penalised linear regression with L1 (Lasso), L2 (Ridge), or a mixture (Elastic Net) regularisation.",
              "Set alpha=1 for Lasso (sparse coefficients, automatic feature selection), alpha=0 for Ridge",
              "(handles correlated predictors gracefully), or anywhere in between. Lambda is selected via",
              "cross-validation. Use this when you have many features (especially p > n), correlated predictors,",
              "or want feature selection baked into the fit. Strong baseline for high-dimensional tabular data."),
            params = list(
              P("alpha", "Alpha (0=Ridge, 1=Lasso)", "numeric", 1, 0, 1, 0.05,
                description = "0 = pure Ridge (L2), 1 = pure Lasso (L1), in between = Elastic Net."),
              P("lambda", "Lambda (\"auto\" or numeric)", "text", "auto",
                description = "Penalty strength. 'auto' chooses lambda.min by CV; otherwise comma-separated non-negative numbers."),
              P("nlambda", "Lambda grid size", "integer", 100, 10, 500, 10,
                description = "How many lambda values to consider during the CV path."),
              P("nfolds", "CV folds", "integer", 5, 3, 20, 1,
                description = "Number of cross-validation folds for selecting lambda."),
              P("standardize", "Standardize predictors", "logical", TRUE,
                description = "Centre & scale columns internally so the penalty treats them on equal footing."))),

  gam = list(id = "gam", label = "GAM (mgcv)", engine = "R",
            task_types = c("regression","binary_classification","count"),
            fn = fit_gam,
            available = function() avail_pkg("mgcv"),
            description = paste(
              "Generalised Additive Model — fits smooth nonlinear effects (penalised splines) for each numeric",
              "predictor while keeping the model additive and interpretable. Each smooth can be visualised on its",
              "own, so you see exactly how the target responds to each feature. Great middle ground between",
              "linear models and black-box ML when you suspect nonlinearity but still need to explain results",
              "to non-technical stakeholders. Enable 'Penalised selection' to let the fit shrink useless smooths to zero."),
            params = list(
              P("k", "Spline basis size (k)", "integer", -1, -1, 50, 1,
                description = "Maximum basis dimension per smooth. -1 lets mgcv choose; raise for wigglier curves."),
              P("select", "Penalised selection", "logical", FALSE,
                description = "If TRUE, smooths can be shrunk all the way to zero (variable selection)."),
              P("method", "Smoothing parameter method", "select", "REML",
                choices = c("REML", "ML", "GCV.Cp"),
                description = "REML/ML are stable for inference; GCV.Cp is the classical default."))),

  ranger_reg = list(id = "ranger_reg", label = "Random Forest (ranger)", engine = "R",
            task_types = c("regression"),
            fn = function(df, t, p, time_col=NULL) fit_ranger(df, t, p, time_col, FALSE),
            available = function() avail_pkg("ranger"),
            description = paste(
              "Random Forest regression via the fast ranger implementation. Builds many decorrelated decision",
              "trees on bootstrap samples and averages their predictions. Robust default with low tuning pain —",
              "handles nonlinearity, interactions, and mixed feature types out of the box. Provides impurity-based",
              "feature importance for free."),
            params = list(
              P("num_trees", "Number of trees", "integer", 500, 100, 2000, 50,
                description = "More trees = more stable predictions, slower training. 500 is a good default."),
              P("mtry", "mtry (\"auto\" or int)", "text", "auto",
                description = "Predictors sampled per split. 'auto' uses sqrt(p) for classification, p/3 for regression."),
              P("min_node_size", "Min node size", "integer", 5, 1, 50, 1,
                description = "Minimum observations per terminal node. Lower = more flexible, more overfit risk."),
              P("max_depth", "Max depth (0 = unlimited)", "integer", 0, 0, 30, 1,
                description = "Hard cap on tree depth. 0 disables the limit."),
              P("sample_fraction", "Sample fraction", "numeric", 1, 0.3, 1, 0.05,
                description = "Fraction of rows drawn (with replacement) for each tree."),
              P("splitrule", "Split rule", "select", "default",
                choices = c("default", "variance", "extratrees", "maxstat"),
                description = "How candidate splits are scored. 'extratrees' adds extra randomness."))),

  xgb_reg = list(id = "xgb_reg", label = "XGBoost (R)", engine = "R",
            task_types = c("regression"),
            fn = function(df, t, p, time_col=NULL) fit_xgb(df, t, p, time_col, FALSE),
            available = function() avail_pkg("xgboost"),
            description = paste(
              "Extreme Gradient Boosting — the gold-standard for tabular regression. Builds trees sequentially,",
              "each correcting the residuals of the previous ensemble. Highly tunable: eta controls learning rate",
              "(lower = more rounds needed but better generalisation), max_depth controls tree complexity,",
              "subsample/colsample inject randomness for regularisation."),
            params = list(
              P("nrounds", "Boosting rounds", "integer", 200, 50, 2000, 50,
                description = "Number of boosting iterations. Use early_stopping_rounds to avoid overshooting."),
              P("eta", "Learning rate (eta)", "numeric", 0.1, 0.01, 0.5, 0.01,
                description = "Step size shrinkage. Lower = more rounds needed but better generalisation."),
              P("max_depth", "Max tree depth", "integer", 6, 2, 15, 1,
                description = "Maximum depth per tree. 4-8 is typical; deeper risks overfitting."),
              P("min_child_weight", "Min child weight", "numeric", 1, 1, 20, 0.5,
                description = "Minimum sum of instance weight per child. Higher = more conservative."),
              P("subsample", "Row subsample", "numeric", 0.9, 0.5, 1, 0.05,
                description = "Fraction of rows sampled per tree. <1 acts as regularisation."),
              P("colsample_bytree", "Column subsample", "numeric", 0.9, 0.3, 1, 0.05,
                description = "Fraction of columns sampled per tree."),
              P("gamma", "Gamma (split loss)", "numeric", 0, 0, 5, 0.1,
                description = "Minimum loss reduction for a split. Higher = more conservative trees."),
              P("lambda", "L2 reg (lambda)", "numeric", 1, 0, 10, 0.1,
                description = "L2 regularisation on leaf weights."),
              P("alpha", "L1 reg (alpha)", "numeric", 0, 0, 10, 0.1,
                description = "L1 regularisation on leaf weights (induces sparsity)."),
              P("tree_method", "Tree method", "select", "auto",
                choices = c("auto", "hist", "approx", "exact"),
                description = "'hist' is fastest on big data; 'auto' lets xgboost decide."),
              P("early_stopping_rounds", "Early stopping rounds", "integer", 0, 0, 200, 5,
                description = "0 = off. Otherwise stop if training metric does not improve for N rounds."))),

  lightgbm_reg = list(id = "lightgbm_reg", label = "LightGBM", engine = "Python",
            task_types = c("regression"),
            fn = function(df, t, p, time_col=NULL) fit_lightgbm_py(df, t, p, time_col, FALSE),
            available = function() avail_python("lightgbm"),
            description = paste(
              "Microsoft's gradient-boosting library — like XGBoost but with leaf-wise (best-first) tree growth",
              "and histogram-based binning, making it markedly faster on large datasets. num_leaves is the key",
              "tuning lever (higher = more flexible but more prone to overfitting)."),
            params = list(
              P("num_iterations", "Iterations (boosting rounds)", "integer", 300, 50, 5000, 50,
                description = "Number of boosting iterations. Pair with low learning_rate for best generalisation."),
              P("learning_rate", "Learning rate", "numeric", 0.05, 0.001, 0.5, 0.005,
                description = "Step size. Lower = needs more iterations but generalises better."),
              P("num_leaves", "Num leaves", "integer", 31, 4, 512, 1,
                description = "Maximum leaves per tree. Key flexibility/overfit knob; keep < 2^max_depth."),
              P("max_depth", "Max depth (-1=unlimited)", "integer", -1, -1, 32, 1,
                description = "Hard cap on tree depth. -1 = unlimited."),
              P("min_data_in_leaf", "Min data in leaf", "integer", 20, 1, 500, 1,
                description = "Minimum samples per leaf. Higher = more regularised."),
              P("feature_fraction", "Feature fraction", "numeric", 1, 0.3, 1, 0.05,
                description = "Fraction of features sampled per tree."),
              P("bagging_fraction", "Bagging fraction", "numeric", 1, 0.3, 1, 0.05,
                description = "Fraction of rows sampled per iteration."),
              P("lambda_l1", "L1 reg", "numeric", 0, 0, 10, 0.1,
                description = "L1 regularisation."),
              P("lambda_l2", "L2 reg", "numeric", 0, 0, 10, 0.1,
                description = "L2 regularisation."))),

  catboost_reg = list(id = "catboost_reg", label = "CatBoost", engine = "Python",
            task_types = c("regression"),
            fn = function(df, t, p, time_col=NULL) fit_catboost_py(df, t, p, time_col, FALSE),
            available = function() avail_python("catboost"),
            description = paste(
              "Yandex's gradient boosting library, designed to handle categorical features natively without",
              "manual one-hot encoding. Tends to need less hyperparameter tuning than XGBoost / LightGBM."),
            params = list(
              P("iterations", "Iterations", "integer", 500, 50, 5000, 50,
                description = "Number of boosting iterations."),
              P("learning_rate", "Learning rate", "numeric", 0.05, 0.001, 0.5, 0.005,
                description = "Step size shrinkage."),
              P("depth", "Tree depth", "integer", 6, 1, 12, 1,
                description = "Symmetric tree depth. CatBoost trees are oblivious (balanced)."),
              P("l2_leaf_reg", "L2 leaf reg", "numeric", 3, 0, 30, 0.5,
                description = "L2 regularisation on leaf weights."),
              P("bagging_temperature", "Bagging temperature", "numeric", 1, 0, 10, 0.1,
                description = "Bayesian bagging strength. 0 = no bagging."),
              P("random_strength", "Random strength", "numeric", 1, 0, 10, 0.1,
                description = "Noise added to split scoring; helps prevent overfitting."))),

  mlp_reg = list(id = "mlp_reg", label = "Neural Net MLP (Keras)", engine = "Python",
            task_types = c("regression"),
            fn = function(df, t, p, time_col=NULL) fit_mlp_py(df, t, p, time_col, FALSE),
            available = function() avail_python("tensorflow",
              hint = "reticulate::py_install('tensorflow') (or scikit-learn fallback)"),
            description = paste(
              "Fully-connected feed-forward neural network (MLP) built with Keras / TensorFlow, with a",
              "scikit-learn MLP fallback if TF is unavailable."),
            params = list(
              P("hidden_units", "Hidden units (comma-sep)", "text", "128,64",
                description = "Layer widths, e.g. '128,64,32' for three hidden layers."),
              P("dropout", "Dropout", "numeric", 0.1, 0, 0.7, 0.05,
                description = "Dropout rate applied after each hidden layer."),
              P("learning_rate", "Learning rate", "numeric", 0.001, 1e-5, 0.1, 1e-4,
                description = "Initial optimizer learning rate."),
              P("batch_size", "Batch size", "integer", 32, 4, 1024, 4,
                description = "Mini-batch size for gradient updates."),
              P("epochs", "Epochs", "integer", 50, 1, 1000, 1,
                description = "Passes over the training data."),
              P("optimizer", "Optimizer", "select", "adam",
                choices = c("adam", "sgd", "rmsprop"),
                description = "Optimization algorithm."),
              P("activation", "Activation", "select", "relu",
                choices = c("relu", "elu", "tanh"),
                description = "Hidden-layer activation function."))),

  # ---- Classification ------------------------------------------------
  logit = list(id = "logit", label = "Logistic Regression", engine = "R",
            task_types = c("binary_classification"),
            fn = fit_logit,
            available = function() avail_ok(),
            description = paste(
              "Binary logistic regression — the classical baseline for two-class problems. Models log-odds as",
              "a linear function of the features; coefficients translate to interpretable odds ratios."),
            params = list(
              P("link", "Link function", "select", "logit",
                choices = c("logit", "probit", "cloglog"),
                description = "Map from linear predictor to probability."),
              P("maxit", "Max iterations", "integer", 50, 10, 200, 5,
                description = "Maximum IRLS iterations for the fit."),
              P("epsilon", "Convergence epsilon", "numeric", 1e-8, 1e-9, 1e-4, 1e-9,
                description = "Tolerance for convergence."))),

  glmnet_cls = list(id = "glmnet_cls", label = "Penalized Logistic (glmnet)", engine = "R",
            task_types = c("binary_classification"),
            fn = function(df, t, p, time_col=NULL) fit_glmnet(df, t, p, time_col, "binomial"),
            available = function() avail_pkg("glmnet"),
            description = paste(
              "Logistic regression with L1 / L2 / Elastic Net regularisation. Strong baseline for",
              "high-dimensional binary classification."),
            params = list(
              P("alpha", "Alpha (0=Ridge, 1=Lasso)", "numeric", 1, 0, 1, 0.05,
                description = "0 = Ridge, 1 = Lasso, in between = Elastic Net."),
              P("lambda", "Lambda (\"auto\" or numeric)", "text", "auto",
                description = "'auto' = lambda.min via CV; otherwise comma-separated non-negative numbers."),
              P("nlambda", "Lambda grid size", "integer", 100, 10, 500, 10,
                description = "Number of lambda values to evaluate."),
              P("nfolds", "CV folds", "integer", 5, 3, 20, 1,
                description = "Number of CV folds."),
              P("standardize", "Standardize predictors", "logical", TRUE,
                description = "Centre & scale columns before fitting."))),

  ranger_cls = list(id = "ranger_cls", label = "Random Forest Classifier", engine = "R",
            task_types = c("binary_classification","multiclass_classification"),
            fn = function(df, t, p, time_col=NULL) fit_ranger(df, t, p, time_col, TRUE),
            available = function() avail_pkg("ranger"),
            description = paste(
              "Random Forest for binary or multiclass classification via ranger."),
            params = list(
              P("num_trees", "Number of trees", "integer", 500, 100, 2000, 50,
                description = "More trees = more stable predictions, slower training."),
              P("mtry", "mtry (\"auto\" or int)", "text", "auto",
                description = "Predictors sampled per split. 'auto' uses sqrt(p)."),
              P("min_node_size", "Min node size", "integer", 1, 1, 50, 1,
                description = "Minimum observations per terminal node."),
              P("max_depth", "Max depth (0 = unlimited)", "integer", 0, 0, 30, 1,
                description = "Hard cap on tree depth. 0 disables the limit."),
              P("sample_fraction", "Sample fraction", "numeric", 1, 0.3, 1, 0.05,
                description = "Fraction of rows drawn per tree."),
              P("splitrule", "Split rule", "select", "default",
                choices = c("default", "gini", "extratrees", "hellinger"),
                description = "How candidate splits are scored. 'extratrees' adds extra randomness."))),

  xgb_cls = list(id = "xgb_cls", label = "XGBoost Classifier", engine = "R",
            task_types = c("binary_classification","multiclass_classification"),
            fn = function(df, t, p, time_col=NULL) fit_xgb(df, t, p, time_col, TRUE),
            available = function() avail_pkg("xgboost"),
            description = paste(
              "XGBoost for binary (logistic) or multiclass (softmax) classification — the performance benchmark",
              "for tabular classification on most real-world problems."),
            params = list(
              P("nrounds", "Boosting rounds", "integer", 200, 50, 2000, 50,
                description = "Number of boosting iterations."),
              P("eta", "Learning rate (eta)", "numeric", 0.1, 0.01, 0.5, 0.01,
                description = "Step size shrinkage."),
              P("max_depth", "Max tree depth", "integer", 6, 2, 15, 1,
                description = "Maximum depth per tree."),
              P("min_child_weight", "Min child weight", "numeric", 1, 1, 20, 0.5,
                description = "Minimum sum of instance weight per child."),
              P("subsample", "Row subsample", "numeric", 0.9, 0.5, 1, 0.05,
                description = "Fraction of rows sampled per tree."),
              P("colsample_bytree", "Column subsample", "numeric", 0.9, 0.3, 1, 0.05,
                description = "Fraction of columns sampled per tree."),
              P("gamma", "Gamma", "numeric", 0, 0, 5, 0.1,
                description = "Minimum loss reduction for a split."),
              P("lambda", "L2 reg", "numeric", 1, 0, 10, 0.1,
                description = "L2 regularisation on leaf weights."),
              P("alpha", "L1 reg", "numeric", 0, 0, 10, 0.1,
                description = "L1 regularisation on leaf weights."),
              P("tree_method", "Tree method", "select", "auto",
                choices = c("auto", "hist", "approx", "exact"),
                description = "'hist' is fastest on big data."),
              P("early_stopping_rounds", "Early stopping rounds", "integer", 0, 0, 200, 5,
                description = "0 = off."))),

  lightgbm_cls = list(id = "lightgbm_cls", label = "LightGBM Classifier", engine = "Python",
            task_types = c("binary_classification","multiclass_classification"),
            fn = function(df, t, p, time_col=NULL) fit_lightgbm_py(df, t, p, time_col, TRUE),
            available = function() avail_python("lightgbm"),
            description = paste(
              "LightGBM for binary or multiclass classification."),
            params = list(
              P("num_iterations", "Iterations", "integer", 300, 50, 5000, 50,
                description = "Number of boosting iterations."),
              P("learning_rate", "Learning rate", "numeric", 0.05, 0.001, 0.5, 0.005,
                description = "Step size."),
              P("num_leaves", "Num leaves", "integer", 31, 4, 512, 1,
                description = "Max leaves per tree."),
              P("max_depth", "Max depth (-1=unlimited)", "integer", -1, -1, 32, 1,
                description = "Hard cap on tree depth."),
              P("min_data_in_leaf", "Min data in leaf", "integer", 20, 1, 500, 1,
                description = "Minimum samples per leaf."),
              P("feature_fraction", "Feature fraction", "numeric", 1, 0.3, 1, 0.05,
                description = "Fraction of features sampled per tree."),
              P("bagging_fraction", "Bagging fraction", "numeric", 1, 0.3, 1, 0.05,
                description = "Fraction of rows sampled per iteration."),
              P("lambda_l1", "L1 reg", "numeric", 0, 0, 10, 0.1,
                description = "L1 regularisation."),
              P("lambda_l2", "L2 reg", "numeric", 0, 0, 10, 0.1,
                description = "L2 regularisation."))),

  mlp_cls = list(id = "mlp_cls", label = "Neural Net Classifier (Keras)", engine = "Python",
            task_types = c("binary_classification","multiclass_classification"),
            fn = function(df, t, p, time_col=NULL) fit_mlp_py(df, t, p, time_col, TRUE),
            available = function() avail_python("tensorflow",
              hint = "reticulate::py_install('tensorflow') (or scikit-learn fallback)"),
            description = paste(
              "Multi-layer perceptron classifier built with Keras / TensorFlow."),
            params = list(
              P("hidden_units", "Hidden units (comma-sep)", "text", "128,64",
                description = "Layer widths, e.g. '128,64,32'."),
              P("dropout", "Dropout", "numeric", 0.1, 0, 0.7, 0.05,
                description = "Dropout rate applied after each hidden layer."),
              P("learning_rate", "Learning rate", "numeric", 0.001, 1e-5, 0.1, 1e-4,
                description = "Initial optimizer learning rate."),
              P("batch_size", "Batch size", "integer", 32, 4, 1024, 4,
                description = "Mini-batch size."),
              P("epochs", "Epochs", "integer", 50, 1, 1000, 1,
                description = "Passes over the training data."),
              P("optimizer", "Optimizer", "select", "adam",
                choices = c("adam", "sgd", "rmsprop"),
                description = "Optimization algorithm."),
              P("activation", "Activation", "select", "relu",
                choices = c("relu", "elu", "tanh"),
                description = "Hidden-layer activation function."))),

  # ---- Counts / proportions / survival -------------------------------
  poisson = list(id = "poisson", label = "Poisson GLM", engine = "R",
            task_types = c("count"), fn = fit_poisson,
            available = function() avail_ok(),
            description = paste(
              "Poisson generalised linear model with a log link — the textbook model for count data."),
            params = list(
              P("link", "Link function", "select", "log",
                choices = c("log", "identity", "sqrt"),
                description = "Map from linear predictor to expected count."),
              P("maxit", "Max iterations", "integer", 50, 10, 200, 5,
                description = "Maximum IRLS iterations."),
              P("epsilon", "Convergence epsilon", "numeric", 1e-8, 1e-9, 1e-4, 1e-9,
                description = "Convergence tolerance."))),

  negbin = list(id = "negbin", label = "Negative Binomial GLM", engine = "R",
            task_types = c("count"), fn = fit_negbin,
            available = function() avail_pkg("MASS"),
            description = paste(
              "Negative Binomial GLM (MASS::glm.nb) — the standard upgrade from Poisson when count data is",
              "over-dispersed."),
            params = list(
              P("link", "Link function", "select", "log",
                choices = c("log", "identity", "sqrt"),
                description = "Map from linear predictor to expected count."),
              P("maxit", "Max iterations", "integer", 50, 10, 200, 5,
                description = "Maximum IRLS iterations."),
              P("epsilon", "Convergence epsilon", "numeric", 1e-8, 1e-9, 1e-4, 1e-9,
                description = "Convergence tolerance."))),

  betareg = list(id = "betareg", label = "Beta Regression", engine = "R",
            task_types = c("proportion"), fn = fit_betareg,
            available = function() avail_pkg("betareg"),
            description = paste(
              "Beta regression for continuous targets bounded in (0, 1) — proportions, rates, percentages."),
            params = list(
              P("link", "Mean link", "select", "logit",
                choices = c("logit", "probit", "cloglog", "loglog"),
                description = "Link for the mean parameter."),
              P("link_phi", "Precision link", "select", "log",
                choices = c("identity", "log", "sqrt"),
                description = "Link for the precision (phi) parameter."))),

  # ---- Time series ---------------------------------------------------
  arima = list(id = "arima", label = "Auto ARIMA", engine = "R",
            task_types = c("time_series"), fn = fit_arima,
            available = function() avail_pkg("forecast"),
            description = paste(
              "Automatic ARIMA via forecast::auto.arima — searches over (p, d, q) and seasonal (P, D, Q) orders",
              "using AIC/BIC."),
            params = list(
              P("ts_frequency", "Seasonal period", "integer", 12, 1, 365, 1,
                description = "12=monthly, 4=quarterly, 7=daily-with-weekly, 52=weekly-with-yearly."),
              P("seasonal", "Seasonal", "logical", TRUE,
                description = "Allow seasonal terms in the search."),
              P("stepwise", "Stepwise search", "logical", TRUE,
                description = "Faster greedy search; turn off for an exhaustive grid."),
              P("approximation", "Approximation", "logical", TRUE,
                description = "Use approximate likelihoods for the search (faster on long series)."),
              P("max_p", "Max p", "integer", 5, 0, 10, 1,
                description = "Upper bound on the AR order."),
              P("max_q", "Max q", "integer", 5, 0, 10, 1,
                description = "Upper bound on the MA order."),
              P("max_P", "Max P (seasonal)", "integer", 2, 0, 5, 1,
                description = "Upper bound on the seasonal AR order."),
              P("max_Q", "Max Q (seasonal)", "integer", 2, 0, 5, 1,
                description = "Upper bound on the seasonal MA order."),
              P("D", "Seasonal differencing D", "select", "auto",
                choices = c("auto", "0", "1", "2"),
                description = "Order of seasonal differencing. 'auto' lets auto.arima decide."),
              P("ic", "Information criterion", "select", "aicc",
                choices = c("aic", "aicc", "bic"),
                description = "Model selection criterion."))),

  ets   = list(id = "ets", label = "ETS (exponential smoothing)", engine = "R",
            task_types = c("time_series"), fn = fit_ets,
            available = function() avail_pkg("forecast"),
            description = paste(
              "Exponential smoothing state-space model — Error/Trend/Seasonal decomposition.",
              "'ZZZ' picks the best combination automatically by AIC."),
            params = list(
              P("ts_frequency", "Seasonal period", "integer", 12, 1, 365, 1,
                description = "Seasonal period of the input series."),
              P("ets_model", "Model spec", "select", "ZZZ",
                choices = c("ZZZ", "ANN", "AAN", "AAA", "MNN", "MAN", "MAM", "MMN", "MMM"),
                description = "ZZZ = auto. Otherwise three letters for Error/Trend/Seasonal (A/M/N)."),
              P("damped", "Damped trend", "select", "auto",
                choices = c("auto", "TRUE", "FALSE"),
                description = "Whether to dampen the trend."),
              P("opt_crit", "Optimization criterion", "select", "lik",
                choices = c("lik", "mse", "amse", "sigma", "mae"),
                description = "Loss minimised when fitting smoothing parameters."))),

  tbats = list(id = "tbats", label = "TBATS (multi-seasonal)", engine = "R",
            task_types = c("time_series"), fn = fit_tbats,
            available = function() avail_pkg("forecast"),
            description = paste(
              "TBATS — for time series with multiple seasonal periods."),
            params = list(
              P("ts_frequency", "Seasonal period", "integer", 12, 1, 365, 1,
                description = "Used when no comma-separated periods are provided below."),
              P("seasonal_periods", "Seasonal periods (comma-sep)", "text", "",
                description = "E.g. '7,365.25' for daily data with weekly + yearly cycles. Leave empty for single seasonality."),
              P("use_box_cox", "Box-Cox transform", "select", "auto",
                choices = c("auto", "TRUE", "FALSE"),
                description = "Apply a Box-Cox power transform automatically."),
              P("use_trend", "Use trend", "select", "auto",
                choices = c("auto", "TRUE", "FALSE"),
                description = "Include a trend component."),
              P("use_damped_trend", "Damped trend", "select", "auto",
                choices = c("auto", "TRUE", "FALSE"),
                description = "Damp the trend component."))),

  # ---- Deep Learning -------------------------------------------------
  lstm = list(id = "lstm", label = "LSTM (Long Short-Term Memory)", engine = "R",
            task_types = c("time_series","binary_classification","multiclass_classification"),
            fn = fit_lstm,
            beta = FALSE,
            available = function() {
              k <- .resolve_keras_backend()
              if (isTRUE(k$ok)) return(list(ok = TRUE,
                msg = sprintf("Using `%s` backend with TensorFlow.", k$backend)))
              if (.has("TSLSTMplus")) return(list(ok = TRUE,
                msg = paste0("Using TSLSTMplus fallback (univariate only). ",
                             k$reason %||% "",
                             " Install keras3 + TF for full features.")))
              list(ok = FALSE,
                   msg = paste0(
                     k$reason %||%
                       "LSTM requires `keras3` (recommended) or `keras` with a Python TensorFlow backend, or `TSLSTMplus` for univariate fallback.",
                     " Run: install.packages('keras3'); keras3::install_keras(); then restart the app."))
            },
            reference_url = "https://keras3.posit.co/",
            dependencies = c("R: keras3 (preferred, with Python tensorflow)",
                              "R: keras (legacy fallback)",
                              "R: TSLSTMplus (univariate fallback)"),
            description = paste(
              "LSTM (Long Short-Term Memory). Recurrent neural network with gating that captures long-range",
              "dependencies in sequences. Strong choice when you have genuine sequential structure",
              "(long context, nonlinear dynamics, regime shifts) and enough data — typically hundreds of series",
              "or long histories. For short, simple business series, ARIMA/ETS/Prophet or GBMs on lagged features",
              "are often simpler and competitive."),
            best_for = c(
              "high-frequency data (minute / hourly / daily)",
              "multiple interacting signals or many parallel series",
              "long histories with nonlinear dynamics or regime shifts",
              "sequence classification (text-like or sensor sequences)"
            ),
            avoid_when = c(
              "you have a short, single series with a clear trend / seasonality (use ETS / ARIMA / Prophet)",
              "your data is tabular non-sequential (use XGBoost / LightGBM / GAM)",
              "you cannot install a Python TensorFlow runtime"
            ),
            long_doc = paste(sep = "\n\n",
              "**Architecture.** An LSTM is a recurrent neural network whose memory cell is regulated by three",
              "learned gates — input, forget, and output. At every step the network can decide *what to remember,*",
              "*what to discard,* and *what to expose downstream.* This gating sidesteps the vanishing-gradient",
              "problem that crippled vanilla RNNs on long sequences and lets the model carry information across",
              "hundreds of timesteps without the signal collapsing.",
              "**Why it matters.** Hochreiter & Schmidhuber's 1997 paper specifically targeted long-range",
              "credit assignment: how can a sequence model relate an event at step *t* to a consequence at step",
              "*t + 200*? Their answer — the constant error carousel inside the cell — turns the recurrence into a",
              "near-additive update on the cell state, so gradients flow without exponential decay.",
              "**Framing.** For forecasting we typically build a *sequence-to-one* supervised problem: feed the last",
              "`timesteps` observations as input and predict the next value. For classification we feed each",
              "sequence and emit a single class. *Stateful* mode preserves cell state across batches (useful for",
              "very long series); *bidirectional* lets a non-causal sequence be read both ways and helps when the",
              "full sequence is available at inference time (e.g. text classification, not online forecasting).",
              "**When classical baselines win.** A well-specified ETS or auto.arima can *beat* an LSTM on a single",
              "monthly series with strong seasonality and only 60 observations — there simply isn't enough data to",
              "train millions of weights. Reach for LSTM when (a) you have a long, high-frequency, or multi-series",
              "panel and (b) the dynamics are visibly nonlinear. References: Hochreiter & Schmidhuber 1997;",
              "Goodfellow et al., *Deep Learning* Ch. 10; keras.posit.co documentation."),
            groups = list(
              "Architecture"          = c("units","layers","bidirectional","stateful","return_sequences"),
              "Sequence shape"        = c("timesteps"),
              "Regularization"        = c("dropout","recurrent_dropout"),
              "Optimization"          = c("optimizer","learning_rate","loss","lr_scheduler"),
              "Training schedule"     = c("batch_size","epochs","early_stopping_patience"),
              "Reproducibility"       = c("seed")
            ),
            params = list(
              P("units", "Hidden units", "integer", 64, 8, 512, 8,
                description = "Number of LSTM cells per layer. More = more capacity, slower training."),
              P("layers", "Stacked layers", "integer", 1, 1, 4, 1,
                description = "How many LSTM layers to stack. Deeper networks need more data."),
              P("bidirectional", "Bidirectional", "logical", FALSE,
                description = "Read each sequence forward AND backward. Use when full sequence is available offline."),
              P("dropout", "Dropout", "numeric", 0, 0, 0.7, 0.05,
                description = "Dropout on the input/output of each LSTM layer."),
              P("recurrent_dropout", "Recurrent dropout", "numeric", 0, 0, 0.7, 0.05,
                description = "Dropout applied to the recurrent transition (state-to-state)."),
              P("timesteps", "Timesteps (window length)", "integer", 12, 2, 1000, 1,
                description = "How many lagged values per training example. Set to one seasonal cycle (e.g. 12 for monthly)."),
              P("batch_size", "Batch size", "integer", 32, 8, 512, 8,
                description = "Mini-batch size for SGD."),
              P("epochs", "Epochs", "integer", 100, 10, 500, 10,
                description = "Maximum passes over the training data; early stopping can cut this short."),
              P("learning_rate", "Learning rate", "numeric", 1e-3, 1e-5, 1e-1, 1e-4,
                description = "Initial optimizer learning rate."),
              P("optimizer", "Optimizer", "select", "adam",
                choices = c("adam","sgd","rmsprop","adamw"),
                description = "SGD variant. 'adam' is the safe default."),
              P("loss", "Loss", "select", "auto",
                choices = c("auto","mse","mae","huber","binary_crossentropy","sparse_categorical_crossentropy"),
                description = "'auto' picks mse for forecasting, sparse_categorical_crossentropy for classification."),
              P("early_stopping_patience", "Early stopping patience", "integer", 10, 0, 50, 1,
                description = "Stop training if loss does not improve for N epochs. 0 = off."),
              P("lr_scheduler", "LR scheduler", "select", "none",
                choices = c("none","step","cosine","reduce_on_plateau"),
                description = "Schedule for the learning rate over epochs."),
              P("stateful", "Stateful", "logical", FALSE,
                description = "Preserve LSTM cell state across batches. Useful for very long single series."),
              P("return_sequences", "Return sequences", "logical", FALSE,
                description = "Emit one output per timestep (for seq-to-seq tasks)."),
              P("seed", "Random seed", "integer", 42, 0, .Machine$integer.max, 1,
                description = "Seed for reproducible weight initialisation."))),

  kan = list(id = "kan", label = "KAN (Kolmogorov-Arnold Network) — BETA", engine = "Python",
            task_types = c("regression","binary_classification","multiclass_classification"),
            fn = fit_kan,
            beta = TRUE,
            available = function() avail_python("kan",
              hint = "reticulate::py_install(c('pykan','torch'))"),
            reference_url = "https://github.com/KindXiaoming/pykan",
            dependencies = c("Python: pykan", "Python: torch"),
            description = paste(
              "KAN (Kolmogorov-Arnold Network). New architecture inspired by the Kolmogorov-Arnold representation",
              "theorem; learnable univariate functions live on edges instead of fixed activations on nodes.",
              "Can match or beat MLPs on smooth low-to-medium-dimensional regression and PDE tasks with smaller",
              "networks and improved interpretability — you can inspect the learned 1-D functions. Status:",
              "research-grade. The practical gains over well-tuned MLPs and GBMs are still modest in many",
              "benchmarks. Use for experimentation and interpretability studies, not bread-and-butter tabular pipelines."),
            best_for = c(
              "smooth, low- to medium-dimensional regression",
              "cases where interpretability of learned 1-D functions is valuable",
              "PDE-style problems and physical simulations"
            ),
            avoid_when = c(
              "production tabular pipelines (gradient boosting wins consistently)",
              "high-dimensional sparse data",
              "very large datasets — pykan trains slowly compared to GBMs",
              "true time-series forecasting — use LSTM / Prophet / ARIMA instead"
            ),
            long_doc = paste(sep = "\n\n",
              "**The theorem.** Kolmogorov-Arnold (1957) states that any multivariate continuous function on a",
              "bounded domain can be represented as a finite composition of *univariate* continuous functions and",
              "addition. KANs are a neural-network instantiation of that decomposition: instead of fixed activations",
              "(ReLU, tanh) sitting on nodes with learned linear weights on edges, a KAN puts *learned* univariate",
              "splines (B-splines parameterised by `grid` knots and order `k`) on the edges and a simple sum on the",
              "nodes.",
              "**Why this changes the inductive bias.** A standard MLP composes many low-dimensional linear maps",
              "with a single fixed nonlinearity. A KAN composes many learned 1-D nonlinearities with addition. For",
              "smooth functions on low- to medium-dimensional inputs (think PDE solutions, physics regressions),",
              "this matches the structure of the target much better and tends to need fewer parameters. The trade-off",
              "is that each edge now has its own spline, so total parameter count and training time per step grow",
              "with `grid` and `k`.",
              "**Interpretability.** Because every edge is a 1-D function you can literally *plot it.* pykan's",
              "`model.plot()` renders each learned univariate function, and there is symbolic regression machinery",
              "(`auto_symbolic`) that tries to fit closed-form expressions like `sin`, `exp`, `log` to those splines.",
              "This is the headline selling point — for the (narrow but real) class of problems where it works, you",
              "get a model whose internals can be read like a textbook.",
              "**Caveats and the open debate.** KANs are research-grade. Independent reproductions have shown that",
              "well-tuned MLPs and gradient boosters often match or beat KANs on standard tabular benchmarks once",
              "you control for parameter count and training budget. Training is slower because LBFGS / Adam over",
              "splines is heavier per step. Sparse-data regimes can be unstable. Use KAN as an *experimental* tool",
              "for interpretability-first projects, not as a default. References: Liu et al. 2024,",
              "*KAN: Kolmogorov-Arnold Networks* (arXiv:2404.19756); pykan README at github.com/KindXiaoming/pykan."),
            groups = list(
              "Architecture"     = c("width","grid","k"),
              "Optimization"     = c("optimizer","learning_rate","steps"),
              "Regularization"   = c("lamb","lamb_l1","lamb_entropy","lamb_coef","lamb_coefdiff","prune_threshold"),
              "Reproducibility"  = c("seed"),
              "Diagnostics"      = c("plot_functions")
            ),
            params = list(
              P("width", "Layer widths", "text", "[n_features, 5, 1]",
                description = "Comma list of layer widths. 'n_features' is substituted at runtime. e.g. '[n_features, 5, 1]'."),
              P("grid", "Grid (spline knots)", "integer", 5, 3, 50, 1,
                description = "Number of grid knots per univariate spline. Higher = wigglier, more parameters."),
              P("k", "Spline order (k)", "integer", 3, 1, 10, 1,
                description = "B-spline order. 3 = cubic (default and usually fine)."),
              P("seed", "Random seed", "integer", 0, 0, .Machine$integer.max, 1,
                description = "Seed for spline initialisation."),
              P("lamb", "Lambda (overall reg)", "numeric", 0, 0, 1e-1, 1e-4,
                description = "Overall regularisation strength."),
              P("lamb_l1", "Lambda L1", "numeric", 1, 0, 1, 1e-2,
                description = "L1 penalty on edge functions (sparsity)."),
              P("lamb_entropy", "Lambda entropy", "numeric", 2, 0, 10, 1e-2,
                description = "Entropy regularisation; encourages clean, low-entropy edge functions."),
              P("lamb_coef", "Lambda coef", "numeric", 0, 0, 1, 1e-3,
                description = "Penalty on spline coefficients."),
              P("lamb_coefdiff", "Lambda coef-diff", "numeric", 0, 0, 1, 1e-3,
                description = "Penalty on differences of consecutive coefficients (smoothness)."),
              P("steps", "Training steps", "integer", 100, 10, 2000, 10,
                description = "Number of optimisation steps."),
              P("optimizer", "Optimizer", "select", "LBFGS",
                choices = c("LBFGS","Adam"),
                description = "LBFGS converges in fewer steps for small problems; Adam scales to larger ones."),
              P("learning_rate", "Learning rate", "numeric", 1e-2, 1e-4, 1e-1, 1e-4,
                description = "Step size for Adam (ignored by LBFGS)."),
              P("prune_threshold", "Prune threshold", "numeric", 1e-2, 0, 1, 1e-3,
                description = "Magnitude below which an edge function is pruned after training."),
              P("plot_functions", "Plot learned 1-D functions", "logical", FALSE,
                description = "After training, render pykan's diagnostic plot of each edge function (best effort)."))),

  prophet = list(id = "prophet", label = "Prophet (Meta)", engine = "R",
            task_types = c("time_series"), fn = fit_prophet,
            available = function() avail_pkg("prophet",
              hint = "install.packages('prophet')"),
            description = paste(
              "Meta's Prophet — additive model with piecewise-linear trend, Fourier-based seasonality,",
              "and holiday effects. Requires a date/time column."),
            params = list(
              P("growth", "Growth", "select", "linear",
                choices = c("linear", "logistic", "flat"),
                description = "Trend type. 'logistic' requires a saturation cap (auto-set to 1.5 * max(y))."),
              P("seasonality_mode", "Seasonality mode", "select", "additive",
                choices = c("additive", "multiplicative"),
                description = "Multiplicative when seasonality scales with the level of the series."),
              P("changepoint_prior_scale", "Changepoint prior scale", "numeric", 0.05, 0.001, 0.5, 0.005,
                description = "Flexibility of the trend; higher = more changepoints."),
              P("seasonality_prior_scale", "Seasonality prior scale", "numeric", 10, 0.01, 50, 0.5,
                description = "Strength of the seasonality regularisation."),
              P("holidays_prior_scale", "Holidays prior scale", "numeric", 10, 0.01, 50, 0.5,
                description = "Strength of holiday effects."),
              P("n_changepoints", "Number of changepoints", "integer", 25, 0, 50, 1,
                description = "Number of potential trend changepoints in the history."),
              P("yearly", "Yearly seasonality", "select", "auto",
                choices = c("auto", "TRUE", "FALSE"),
                description = "Include yearly seasonality."),
              P("weekly", "Weekly seasonality", "select", "auto",
                choices = c("auto", "TRUE", "FALSE"),
                description = "Include weekly seasonality."),
              P("daily", "Daily seasonality", "select", "FALSE",
                choices = c("auto", "TRUE", "FALSE"),
                description = "Include daily seasonality (only useful for sub-daily data)."),
              P("prophet_freq", "Forecast frequency", "select", "month",
                choices = c("day", "week", "month", "quarter", "year"),
                description = "Step granularity of the future dataframe.")))
)

# Filter registry by task type
models_for_task <- function(task_type) {
  Filter(function(m) task_type %in% m$task_types, MODELS)
}

# Friendly engine badge
engine_badge <- function(engine) {
  cls <- if (engine == "Python") "badge-py" else "badge-r"
  tags$span(class = paste("badge", cls), engine)
}

# Probe model availability without throwing — UI shows a graceful warning
# instead of an opaque error when an optional dependency is missing.
model_availability <- function(model_id) {
  m <- MODELS[[model_id]]
  if (is.null(m)) return(list(ok = FALSE, msg = "Unknown model"))
  if (is.null(m$available)) return(avail_ok())
  tryCatch(m$available(),
           error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
}

# Canonical install command for a model. Used by the dependency modal in
# Model Lab to show a one-liner the user can copy. We never auto-execute
# installs from inside the running Shiny session.
model_install_command <- function(model_id) {
  per_model <- list(
    lstm        = "install.packages('keras3'); keras3::install_keras()",
    kan         = "install.packages('torch'); torch::install_torch()",
    prophet     = "install.packages('prophet')",
    glmnet_reg  = "install.packages('glmnet')",
    glmnet_clf  = "install.packages('glmnet')",
    gam         = "install.packages('mgcv')",
    ranger_reg  = "install.packages('ranger')",
    ranger_clf  = "install.packages('ranger')",
    xgb_reg     = "install.packages('xgboost')",
    xgb_clf     = "install.packages('xgboost')",
    lightgbm    = "install.packages('lightgbm')",
    catboost    = "install.packages('catboost', repos = 'https://catboost.ai/repo/r')",
    arima       = "install.packages('forecast')",
    ets         = "install.packages('forecast')",
    tbats       = "install.packages('forecast')"
  )
  cmd <- per_model[[model_id]]
  if (!is.null(cmd)) return(cmd)
  m <- MODELS[[model_id]]
  if (is.null(m)) return(NULL)
  deps <- m$dependencies %||% character(0)
  r_deps <- gsub("^R:\\s*", "", grep("^R:", deps, value = TRUE))
  if (length(r_deps) > 0) {
    pkg <- sub("\\s.*$", "", r_deps[1])
    return(sprintf("install.packages('%s')", pkg))
  }
  py_deps <- gsub("^Python:\\s*", "", grep("^Python:", deps, value = TRUE))
  if (length(py_deps) > 0) {
    mod <- sub("\\s.*$", "", py_deps[1])
    return(sprintf("reticulate::py_install('%s')", mod))
  }
  NULL
}

# ---- Editorial metadata for existing models -------------------------------
# Authored once here so the Training Studio's "About this model" drawer can
# render best_for / avoid_when / long_doc / reference_url / dependencies for
# *every* model — not just the new LSTM/KAN entries — without ballooning the
# main MODELS list. Models defined inline above (LSTM, KAN) take precedence.
EXTRA_DOCS <- list(
  lm = list(
    best_for = c("linear, monotonic relationships",
                 "small/medium datasets where interpretability matters",
                 "sanity checking before reaching for fancier models"),
    avoid_when = c("strongly nonlinear targets",
                   "many correlated predictors (use Elastic Net)",
                   "heavy-tailed residuals (consider robust regression)"),
    reference_url = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html",
    dependencies = c("R: stats (base)"),
    long_doc = paste(sep = "\n\n",
      "**Model.** Ordinary least squares finds the coefficient vector that minimises the sum of squared residuals.",
      "Each coefficient is the marginal effect of its predictor with everything else held fixed.",
      "**Assumptions.** Linearity in parameters, approximately Gaussian residuals, homoscedasticity, and no",
      "near-perfect multicollinearity. Inference (p-values, CIs) leans on these assumptions; point predictions are",
      "more forgiving.",
      "**When it wins.** Whenever the truth is close to linear and you need something stakeholders can read off",
      "the coefficient table. It's also the right floor for benchmarking — if a tuned XGBoost barely beats lm,",
      "the simpler model usually wins.",
      "**Pitfalls.** Outliers can dominate the fit; consider robust alternatives (`MASS::rlm`) or winsorisation.",
      "Reference: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html.")),
  glmnet_reg = list(
    best_for = c("p > n or many correlated features",
                 "automatic feature selection (Lasso)",
                 "high-dimensional tabular regression"),
    avoid_when = c("strongly nonlinear effects (use GAM / GBM)",
                   "tiny datasets where vanilla lm suffices"),
    reference_url = "https://glmnet.stanford.edu/",
    dependencies = c("R: glmnet"),
    long_doc = paste(sep = "\n\n",
      "**Model.** Penalised linear regression: minimise (RSS + lambda * [(1 - alpha) * 0.5 * ||beta||^2 + alpha * ||beta||_1]).",
      "alpha=1 is Lasso (sparse); alpha=0 is Ridge (handles correlated features). Lambda is selected by k-fold CV.",
      "**Why it works.** Regularisation trades a small amount of bias for a large reduction in variance — exactly",
      "what you want when n is small relative to p. Lasso additionally drives some coefficients to zero, giving you",
      "feature selection for free.",
      "**Tuning.** alpha is the L1 / L2 mixing knob; nlambda controls the granularity of the CV path. Standardise",
      "predictors so the penalty treats them on equal footing.",
      "**References.** Friedman, Hastie & Tibshirani, *glmnet* paper (J. Stat. Softw. 2010); https://glmnet.stanford.edu/.")),
  gam = list(
    best_for = c("nonlinear-but-additive effects",
                 "interpretable plots of each smooth",
                 "moderate-sized datasets"),
    avoid_when = c("you need strong feature interactions (use GBMs)",
                   "ultra-high-dimensional sparse data"),
    reference_url = "https://cran.r-project.org/package=mgcv",
    dependencies = c("R: mgcv"),
    long_doc = paste(sep = "\n\n",
      "**Model.** A Generalised Additive Model fits each numeric predictor as a penalised smooth `s(x)` and adds",
      "them up. The smoothness of each term is chosen by the data via REML / GCV.",
      "**Why it works.** Most real-world relationships are nonlinear but mostly *additive* — a GAM captures the",
      "nonlinearity without giving up the per-feature interpretability of a linear model.",
      "**Tuning.** k caps the basis dimension; -1 lets mgcv choose. 'Penalised selection' lets useless smooths be",
      "shrunk all the way to zero (a soft variable selection).",
      "**References.** Wood, *Generalized Additive Models: An Introduction with R* (2nd ed., CRC, 2017); ?mgcv::gam.")),
  ranger_reg = list(
    best_for = c("strong nonparametric baseline with little tuning",
                 "mixed feature types out of the box",
                 "feature importance via impurity"),
    avoid_when = c("extrapolation outside the training range is required",
                   "you need calibrated probability outputs (use a logistic / Brier model)"),
    reference_url = "https://cran.r-project.org/package=ranger",
    dependencies = c("R: ranger"),
    long_doc = paste(sep = "\n\n",
      "**Model.** Random Forests build many decorrelated decision trees on bootstrap samples; predictions average",
      "across trees. ranger is a fast C++ implementation, well suited to mid-to-large datasets.",
      "**Why it works.** Bagging reduces variance, the random feature subset at each split decorrelates the trees,",
      "and the average is a much smoother estimator than any single tree.",
      "**Tuning.** num_trees > 500 for stable importance estimates; mtry, min_node_size, max_depth, sample_fraction",
      "are the main knobs. extratrees splitrule injects extra randomness for more regularisation.",
      "**References.** Breiman 2001 (*Random Forests*); Wright & Ziegler 2017 (*ranger*).")),
  xgb_reg = list(
    best_for = c("tabular regression on real-world messy data",
                 "datasets where interactions matter",
                 "fast inference once trained"),
    avoid_when = c("very small datasets where lm/GAM win",
                   "strict interpretability mandates without SHAP"),
    reference_url = "https://xgboost.readthedocs.io/",
    dependencies = c("R: xgboost"),
    long_doc = paste(sep = "\n\n",
      "**Model.** Gradient boosting builds trees sequentially, each fitted to the negative gradient of the loss",
      "with respect to the running prediction. XGBoost adds a second-order (Hessian) term, regularisation on leaf",
      "weights, and a histogram-based split finder.",
      "**Why it works.** Boosting turns weak learners (shallow trees) into a strong learner; regularisation and",
      "subsampling fight overfitting; the tree base learner handles nonlinearity and interactions natively.",
      "**Tuning order.** (1) eta + nrounds with early stopping; (2) max_depth and min_child_weight; (3) subsample",
      "and colsample_bytree; (4) gamma / lambda / alpha for additional regularisation.",
      "**References.** Chen & Guestrin 2016 (XGBoost paper); https://xgboost.readthedocs.io/.")),
  lightgbm_reg = list(
    best_for = c("very large datasets",
                 "speed-sensitive training loops"),
    avoid_when = c("tiny datasets (overfits a leaf-wise tree easily)"),
    reference_url = "https://lightgbm.readthedocs.io/",
    dependencies = c("Python: lightgbm"),
    long_doc = paste(sep = "\n\n",
      "**Model.** LightGBM grows trees leaf-wise (best-first) instead of level-wise, picking the leaf with the",
      "biggest loss reduction at each step. Combined with histogram-based binning this delivers substantial speed-ups",
      "on large datasets versus depth-first GBMs.",
      "**Why it works.** Leaf-wise growth concentrates capacity where the loss is highest, so for a fixed number",
      "of leaves you usually get a better fit than depth-wise growth. Histograms reduce split-search cost from O(n)",
      "to O(bins).",
      "**Tuning.** num_leaves is the master knob (keep < 2^max_depth to avoid overfit); pair low learning_rate",
      "with many iterations; min_data_in_leaf is the strongest regulariser.",
      "**References.** Ke et al. 2017 (NeurIPS) — *LightGBM: A Highly Efficient Gradient Boosting Decision Tree*.")),
  catboost_reg = list(
    best_for = c("datasets with many categorical features",
                 "fewer hyperparameters to tune"),
    avoid_when = c("you need very fast training on huge data (LightGBM is faster)"),
    reference_url = "https://catboost.ai/",
    dependencies = c("Python: catboost"),
    long_doc = paste(sep = "\n\n",
      "**Model.** CatBoost is a gradient boosting library from Yandex that handles categorical features natively",
      "via *ordered target statistics* (each row's encoding uses only earlier rows, breaking the prediction-shift",
      "leak that one-hot encoding can produce in the presence of high-cardinality categories).",
      "**Why it works.** Symmetric (oblivious) trees and ordered boosting reduce overfitting; the categorical",
      "handling removes the need for manual one-hot encoding.",
      "**Tuning.** iterations + learning_rate are the primary axis; depth (typically 4-8) and l2_leaf_reg are next;",
      "bagging_temperature and random_strength inject randomness for regularisation.",
      "**References.** Prokhorenkova et al. 2018 (NeurIPS) — *CatBoost: unbiased boosting with categorical features*.")),
  mlp_reg = list(
    best_for = c("nonlinear regression with abundant data",
                 "embedding a model inside a larger neural pipeline"),
    avoid_when = c("small datasets (a GAM or GBM will be more reliable)"),
    reference_url = "https://keras.posit.co/",
    dependencies = c("Python: tensorflow (optional fallback: scikit-learn)"),
    long_doc = paste(sep = "\n\n",
      "**Model.** A multi-layer perceptron is a stack of fully connected layers with elementwise nonlinearities.",
      "Universal approximation guarantees that, with enough hidden units, an MLP can represent any continuous",
      "function on a bounded domain.",
      "**Why it works (when it does).** Backprop + SGD finds local minima of the loss surface that generalise",
      "surprisingly well in over-parameterised regimes. Dropout and weight decay control overfitting.",
      "**Tuning.** Width and depth (hidden_units), dropout, learning_rate, and batch_size are the main levers.",
      "Use early stopping if you can carve out a validation set.",
      "**References.** Goodfellow et al., *Deep Learning* (MIT Press, 2016); https://keras.posit.co/.")),
  logit = list(
    best_for = c("interpretable two-class baselines",
                 "calibrated probability outputs"),
    avoid_when = c("nonlinear class boundaries dominate (use trees / GBMs)"),
    reference_url = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html",
    dependencies = c("R: stats (base)"),
    long_doc = paste(sep = "\n\n",
      "**Model.** Logistic regression models the log-odds of the positive class as a linear combination of the",
      "features. Coefficients translate to odds ratios — a one-unit increase in x_j multiplies the odds by exp(beta_j).",
      "**Why it works.** It is the maximum-entropy classifier subject to feature-mean constraints; it is convex,",
      "fast, and produces well-calibrated probabilities by default.",
      "**Tuning.** Link function (logit / probit / cloglog) rarely matters for prediction; pick logit unless you",
      "have a domain reason. Use penalised variants (glmnet) when p is large or features are correlated.",
      "**References.** Hosmer, Lemeshow & Sturdivant, *Applied Logistic Regression* (3rd ed., Wiley, 2013).")),
  glmnet_cls = list(
    best_for = c("p > n binary classification",
                 "high-dimensional text/genomic data"),
    avoid_when = c("nonlinear class boundaries dominate"),
    reference_url = "https://glmnet.stanford.edu/",
    dependencies = c("R: glmnet"),
    long_doc = paste(sep = "\n\n",
      "**Model.** Penalised logistic regression with the same Lasso / Ridge / Elastic Net machinery as glmnet for",
      "regression, but with the binomial deviance as the loss.",
      "**Why it works.** Combines logistic's calibrated probabilities with regularisation's bias-variance trade-off.",
      "Lasso variant gives feature selection inside the fit.",
      "**Tuning.** alpha + nlambda + nfolds; standardise features.",
      "**References.** Friedman, Hastie & Tibshirani 2010; https://glmnet.stanford.edu/.")),
  ranger_cls = list(
    best_for = c("multiclass tabular classification",
                 "robust default with little tuning"),
    avoid_when = c("calibrated probabilities are critical without recalibration"),
    reference_url = "https://cran.r-project.org/package=ranger",
    dependencies = c("R: ranger"),
    long_doc = paste(sep = "\n\n",
      "**Model.** Random Forest classifier — same machinery as the regressor with class-vote / probability outputs.",
      "**Why it works.** See ranger_reg; for classification it is the standard off-the-shelf strong baseline.",
      "**Tuning.** num_trees, mtry (often sqrt(p) is good), min_node_size, sample_fraction. extratrees splitrule",
      "for additional regularisation.",
      "**References.** Breiman 2001; Wright & Ziegler 2017.")),
  xgb_cls = list(
    best_for = c("tabular binary / multiclass classification",
                 "winning Kaggle-style benchmarks"),
    avoid_when = c("very small datasets where lm/GAM win"),
    reference_url = "https://xgboost.readthedocs.io/",
    dependencies = c("R: xgboost"),
    long_doc = paste(sep = "\n\n",
      "**Model.** XGBoost classifier — same gradient-boosted trees, with logistic / softmax loss.",
      "**Why it works.** See xgb_reg.",
      "**Tuning.** Same order as the regressor; for very imbalanced targets, tune scale_pos_weight or use",
      "stratified CV.",
      "**References.** Chen & Guestrin 2016; https://xgboost.readthedocs.io/.")),
  lightgbm_cls = list(
    best_for = c("very large multiclass datasets",
                 "speed-sensitive training loops"),
    avoid_when = c("tiny datasets"),
    reference_url = "https://lightgbm.readthedocs.io/",
    dependencies = c("Python: lightgbm"),
    long_doc = paste(sep = "\n\n",
      "**Model.** LightGBM classifier — see lightgbm_reg for the underlying boosting machinery.",
      "**Why it works / Tuning / References.** Same as lightgbm_reg.")),
  mlp_cls = list(
    best_for = c("deep tabular classification with abundant data",
                 "embedding inside a larger neural pipeline"),
    avoid_when = c("small datasets"),
    reference_url = "https://keras.posit.co/",
    dependencies = c("Python: tensorflow (optional fallback: scikit-learn)"),
    long_doc = paste(sep = "\n\n",
      "**Model.** Keras MLP classifier with softmax output.",
      "**Why it works / Tuning / References.** Same as mlp_reg; for classification use sparse_categorical_crossentropy.")),
  poisson = list(
    best_for = c("count outcomes (events per period)",
                 "interpretable rate ratios"),
    avoid_when = c("over-dispersed counts (use Negative Binomial)"),
    reference_url = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html",
    dependencies = c("R: stats (base)"),
    long_doc = paste(sep = "\n\n",
      "**Model.** Poisson GLM with a log link models log(E[Y|X]) as a linear function of features. exp(coef) is",
      "the multiplicative effect on the expected count.",
      "**Assumption.** Mean equals variance. Real count data is often over-dispersed — check with the dispersion",
      "test before trusting standard errors.",
      "**Reference.** McCullagh & Nelder, *Generalized Linear Models* (Chapman & Hall, 1989).")),
  negbin = list(
    best_for = c("over-dispersed count data",
                 "panels with rare-but-clustered events"),
    avoid_when = c("equidispersed counts (Poisson is more efficient)"),
    reference_url = "https://cran.r-project.org/package=MASS",
    dependencies = c("R: MASS"),
    long_doc = paste(sep = "\n\n",
      "**Model.** Negative Binomial GLM adds a dispersion parameter theta to Poisson, allowing variance > mean.",
      "**Why it works.** Real-world count data — insurance claims, hospital admissions, social-media events — is",
      "almost always over-dispersed. Poisson SEs collapse on such data; NB fixes that.",
      "**Reference.** Hilbe, *Negative Binomial Regression* (CUP, 2nd ed., 2011).")),
  betareg = list(
    best_for = c("continuous proportions / rates in (0, 1)",
                 "fractions, percentages, response shares"),
    avoid_when = c("targets touch the {0, 1} boundary often (use zero/one-inflated beta)"),
    reference_url = "https://cran.r-project.org/package=betareg",
    dependencies = c("R: betareg"),
    long_doc = paste(sep = "\n\n",
      "**Model.** Beta regression assumes Y | X ~ Beta(mu, phi) reparameterised so mu is the mean and phi the",
      "precision. Mean and precision can each be modelled with their own link.",
      "**Why it works.** Linear regression on a (0, 1) target produces nonsensical predictions outside the unit",
      "interval; logit-Gaussian heroics distort variance. Beta is the natural exponential family on (0, 1).",
      "**Reference.** Cribari-Neto & Zeileis 2010, *Beta Regression in R*, JSS.")),
  arima = list(
    best_for = c("univariate time series with trend / seasonality",
                 "moderate horizons with calibrated PIs"),
    avoid_when = c("multivariate dynamics dominate (use VAR / state-space / LSTM)",
                   "very short series (< 30 obs)"),
    reference_url = "https://otexts.com/fpp3/",
    dependencies = c("R: forecast"),
    long_doc = paste(sep = "\n\n",
      "**Model.** ARIMA(p, d, q)(P, D, Q)[m] combines autoregression, differencing, and moving-average terms with",
      "their seasonal counterparts. forecast::auto.arima searches over orders by AIC/BIC and chooses the best model.",
      "**Why it works.** ARIMA is a flexible parametric family with calibrated prediction intervals when the",
      "process is approximately stationary after differencing.",
      "**Tuning.** Trust auto.arima for first cuts; raise max.p/q for richer dynamics; force stepwise=FALSE for an",
      "exhaustive grid when speed permits.",
      "**Reference.** Hyndman & Athanasopoulos, *Forecasting: Principles and Practice* (3rd ed., OTexts).")),
  ets = list(
    best_for = c("level / trend / seasonal smoothing",
                 "stable seasonal series"),
    avoid_when = c("structural breaks dominate (consider TBATS or Prophet)"),
    reference_url = "https://otexts.com/fpp3/",
    dependencies = c("R: forecast"),
    long_doc = paste(sep = "\n\n",
      "**Model.** ETS is a state-space exponential smoothing family with three components: Error (A/M), Trend",
      "(N/A/M, optionally damped), and Seasonal (N/A/M). 'ZZZ' picks the best combination by AIC.",
      "**Why it works.** Recursive smoothing weights recent observations more heavily — a strong inductive bias",
      "for series whose level / trend / seasonality evolves slowly.",
      "**Reference.** Hyndman, Koehler, Ord & Snyder, *Forecasting with Exponential Smoothing* (Springer, 2008).")),
  tbats = list(
    best_for = c("multiple seasonal periods (e.g. weekly + yearly)",
                 "long, high-frequency series"),
    avoid_when = c("short series",
                   "you need fast training loops (TBATS is slow)"),
    reference_url = "https://otexts.com/fpp3/",
    dependencies = c("R: forecast"),
    long_doc = paste(sep = "\n\n",
      "**Model.** TBATS = Trigonometric seasonality + Box-Cox + ARMA errors + Trend + Seasonal. Handles multiple",
      "seasonal periods (e.g. 7 and 365.25 for daily data) using Fourier terms.",
      "**Why it works.** Real-world series often have nested seasonalities that single-period ETS / ARIMA cannot",
      "capture; trigonometric seasonality scales gracefully.",
      "**Reference.** De Livera, Hyndman & Snyder 2011 (*JASA*).")),
  prophet = list(
    best_for = c("business time series with holidays / changepoints",
                 "non-experts who need readable trend / seasonality components"),
    avoid_when = c("very short series",
                   "high-frequency dynamics where LSTM / state-space win"),
    reference_url = "https://facebook.github.io/prophet/",
    dependencies = c("R: prophet"),
    long_doc = paste(sep = "\n\n",
      "**Model.** Prophet is a Bayesian additive model with three components: piecewise-linear (or logistic) trend",
      "with automatic changepoint detection, Fourier-based seasonality, and holiday effects.",
      "**Why it works.** Designed for business analysts: defaults are sane, missing data is tolerated, and the",
      "components are interpretable. Trades a small amount of accuracy for robustness and explainability.",
      "**Reference.** Taylor & Letham 2018, *Forecasting at Scale*.")
  )
)

# Merge a model entry with its EXTRA_DOCS row (entry-level fields win).
model_meta <- function(model_id) {
  m <- MODELS[[model_id]]
  if (is.null(m)) return(NULL)
  extra <- EXTRA_DOCS[[model_id]] %||% list()
  for (k in c("best_for","avoid_when","long_doc","reference_url","dependencies","groups")) {
    if (is.null(m[[k]]) && !is.null(extra[[k]])) m[[k]] <- extra[[k]]
  }
  m
}

# Convenience accessors used by the Training Studio UI.
model_long_doc      <- function(model_id) model_meta(model_id)$long_doc      %||% ""
model_best_for      <- function(model_id) model_meta(model_id)$best_for      %||% character(0)
model_avoid_when    <- function(model_id) model_meta(model_id)$avoid_when    %||% character(0)
model_reference_url <- function(model_id) model_meta(model_id)$reference_url %||% ""
model_dependencies  <- function(model_id) model_meta(model_id)$dependencies  %||% character(0)
model_groups        <- function(model_id) model_meta(model_id)$groups        %||% list()
model_is_beta       <- function(model_id) isTRUE(model_meta(model_id)$beta)
