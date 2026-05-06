# =====================================================================
# Model Registry
# Each model entry exposes:
#   id, label, engine ("R" | "Python"), task_types, description,
#   params (list of UI specs),
#   available (function() -> list(ok=bool, msg=character)) — optional,
#     used by the Model Lab UI to show inline missing-dependency notices.
#   fit(df, target, params, time_col=NULL) -> list(model=..., predict=function(newdata, h)..., feat_imp=...)
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

# ---- Model implementations -------------------------------------------

# Linear regression
fit_lm <- function(df, target, params, time_col = NULL) {
  intercept <- isTRUE(params$intercept %||% TRUE)
  na_action <- params$na_action %||% "na.omit"
  rhs <- if (intercept) "." else ". - 1"
  f <- as.formula(paste(target, "~", rhs))
  m <- lm(f, data = df,
          na.action = switch(na_action,
                              "na.exclude" = stats::na.exclude,
                              stats::na.omit))
  list(model = m,
       predict = function(newdata, h = NULL) as.numeric(predict(m, newdata = newdata)),
       feat_imp = NULL)
}

# Logistic regression
fit_logit <- function(df, target, params, time_col = NULL) {
  df[[target]] <- as.factor(df[[target]])
  link <- params$link %||% "logit"
  f <- as.formula(paste(target, "~ ."))
  m <- glm(f, data = df,
           family = binomial(link = link),
           control = glm.control(maxit  = as.integer(params$maxit %||% 50),
                                  epsilon = as.numeric(params$epsilon %||% 1e-8)))
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
  link <- params$link %||% "log"
  m <- glm(as.formula(paste(target, "~ .")), data = df,
           family = poisson(link = link),
           control = glm.control(maxit  = as.integer(params$maxit %||% 50),
                                  epsilon = as.numeric(params$epsilon %||% 1e-8)))
  list(model = m,
       predict = function(newdata, h = NULL) as.numeric(predict(m, newdata = newdata, type = "response")),
       feat_imp = NULL)
}
fit_negbin <- function(df, target, params, time_col = NULL) {
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
  if (!.has("prophet"))
    stop("Prophet not installed - run install.packages('prophet') to enable")
  if (is.null(time_col)) stop("Prophet requires a time column")
  d <- data.frame(ds = as.Date(df[[time_col]]), y = as.numeric(df[[target]]))

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

# Keras MLP via reticulate (Python TF)
fit_mlp_py <- function(df, target, params, time_col = NULL, classification = FALSE) {
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
