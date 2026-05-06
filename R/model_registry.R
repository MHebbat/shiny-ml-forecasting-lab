# =====================================================================
# Model Registry
# Each model entry exposes:
#   id, label, engine ("R" | "Python"), task_types, description,
#   params (list of UI specs),
#   fit(df, target, params, time_col=NULL) -> list(model=..., predict=function(newdata, h)..., feat_imp=...)
#
# Author: Malik Hebbat
# =====================================================================

# Helper to declare a tuneable parameter for the UI
P <- function(name, label, type = "numeric", default = NULL, min = NULL, max = NULL,
              step = NULL, choices = NULL, help = "") {
  list(name = name, label = label, type = type, default = default,
       min = min, max = max, step = step, choices = choices, help = help)
}

# Quick safe loader
.has <- function(pkg) requireNamespace(pkg, quietly = TRUE)

# ---- Model implementations -------------------------------------------

# Linear regression
fit_lm <- function(df, target, params, time_col = NULL) {
  f <- as.formula(paste(target, "~ ."))
  m <- lm(f, data = df)
  list(model = m, predict = function(newdata, h = NULL) as.numeric(predict(m, newdata = newdata)),
       feat_imp = NULL)
}

# Logistic regression
fit_logit <- function(df, target, params, time_col = NULL) {
  df[[target]] <- as.factor(df[[target]])
  f <- as.formula(paste(target, "~ ."))
  m <- glm(f, data = df, family = binomial())
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
  train_cols <- colnames(x)               # captured at training time
  train_df_cols <- setdiff(colnames(df), target)
  alpha <- params$alpha %||% 1
  m <- glmnet::cv.glmnet(x, y, alpha = alpha, family = family,
                         nfolds = params$nfolds %||% 5)
  list(model = m,
       predict = function(newdata, h = NULL) {
         # Add a dummy target so model.matrix has same formula behaviour
         if (!target %in% colnames(newdata)) newdata[[target]] <- y[1]
         # Drop unseen columns, keep only original training feature cols
         keep <- intersect(train_df_cols, colnames(newdata))
         newdata <- newdata[, c(keep, target), drop = FALSE]
         nx <- model.matrix(as.formula(paste(target, "~ . -1")), data = newdata)
         # Align columns to training set; missing cols -> 0, drop unseen levels
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
  smooth_terms <- if (length(num_cols) > 0)
    paste(sprintf("s(%s)", num_cols), collapse = " + ") else "1"
  cat_cols <- setdiff(names(df), c(num_cols, target))
  rhs <- if (length(cat_cols) > 0) paste(smooth_terms, "+", paste(cat_cols, collapse = " + ")) else smooth_terms
  m <- mgcv::gam(as.formula(paste(target, "~", rhs)), data = df,
                 select = isTRUE(params$select))
  list(model = m,
       predict = function(newdata, h = NULL) as.numeric(predict(m, newdata = newdata)),
       feat_imp = NULL)
}

# Random forest via ranger
fit_ranger <- function(df, target, params, time_col = NULL, classification = FALSE) {
  if (!.has("ranger")) stop("Install 'ranger'")
  if (classification) df[[target]] <- as.factor(df[[target]])
  m <- ranger::ranger(
    dependent.variable.name = target,
    data = df,
    num.trees = params$num_trees %||% 500,
    mtry = params$mtry %||% NULL,
    min.node.size = params$min_node_size %||% (if (classification) 1 else 5),
    importance = "impurity",
    classification = classification
  )
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
  # Coerce character to factor for classification so unseen levels can be ignored later
  if (classification && is.character(y)) y <- as.factor(y)
  X <- model.matrix(as.formula(paste(target, "~ . -1")), data = df)
  train_cols <- colnames(X)
  train_df_cols <- setdiff(colnames(df), target)
  obj <- if (classification) {
    if (length(unique(y)) == 2) "binary:logistic" else "multi:softprob"
  } else "reg:squarederror"
  yy <- if (classification) as.integer(as.factor(y)) - 1 else as.numeric(y)
  num_class <- if (classification && length(unique(y)) > 2) length(unique(y)) else NULL
  parm <- list(
    objective = obj,
    eta = params$eta %||% 0.1,
    max_depth = params$max_depth %||% 6,
    subsample = params$subsample %||% 0.9,
    colsample_bytree = params$colsample_bytree %||% 0.9,
    min_child_weight = params$min_child_weight %||% 1
  )
  if (!is.null(num_class)) parm$num_class <- num_class
  m <- xgboost::xgboost(data = X, label = yy, params = parm,
                        nrounds = params$nrounds %||% 200,
                        verbose = 0)
  imp <- tryCatch(xgboost::xgb.importance(model = m), error = function(e) NULL)
  list(model = m,
       predict = function(newdata, h = NULL) {
         if (!target %in% colnames(newdata)) newdata[[target]] <- y[1]
         keep <- intersect(train_df_cols, colnames(newdata))
         newdata <- newdata[, c(keep, target), drop = FALSE]
         # Coerce factors so unseen levels can be filtered safely
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
  m <- glm(as.formula(paste(target, "~ .")), data = df, family = poisson())
  list(model = m,
       predict = function(newdata, h = NULL) as.numeric(predict(m, newdata = newdata, type = "response")),
       feat_imp = NULL)
}
fit_negbin <- function(df, target, params, time_col = NULL) {
  if (!.has("MASS")) stop("Install 'MASS'")
  m <- MASS::glm.nb(as.formula(paste(target, "~ .")), data = df)
  list(model = m,
       predict = function(newdata, h = NULL) as.numeric(predict(m, newdata = newdata, type = "response")),
       feat_imp = NULL)
}

# Beta regression
fit_betareg <- function(df, target, params, time_col = NULL) {
  if (!.has("betareg")) stop("Install 'betareg'")
  m <- betareg::betareg(as.formula(paste(target, "~ .")), data = df)
  list(model = m,
       predict = function(newdata, h = NULL) as.numeric(predict(m, newdata = newdata)),
       feat_imp = NULL)
}

# ---- Time series ------------------------------------------------------
# ARIMA via forecast
fit_arima <- function(df, target, params, time_col = NULL) {
  if (!.has("forecast")) stop("Install 'forecast'")
  y <- as.numeric(df[[target]])
  freq <- params$ts_frequency %||% 12
  ts_y <- ts(y, frequency = freq)
  m <- forecast::auto.arima(ts_y, seasonal = isTRUE(params$seasonal %||% TRUE))
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
  freq <- params$ts_frequency %||% 12
  ts_y <- ts(y, frequency = freq)
  m <- forecast::ets(ts_y, model = params$ets_model %||% "ZZZ")
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
  freq <- params$ts_frequency %||% 12
  ts_y <- ts(y, frequency = freq)
  m <- forecast::tbats(ts_y)
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
  if (!.has("prophet")) stop("Install 'prophet' (optional)")
  if (is.null(time_col)) stop("Prophet requires a time column")
  d <- data.frame(ds = as.Date(df[[time_col]]), y = as.numeric(df[[target]]))
  m <- prophet::prophet(d,
    yearly.seasonality = isTRUE(params$yearly %||% TRUE),
    weekly.seasonality = isTRUE(params$weekly %||% TRUE),
    daily.seasonality  = isTRUE(params$daily  %||% FALSE))
  list(model = m,
       predict = function(newdata = NULL, h = 12) {
         fut <- prophet::make_future_dataframe(m, periods = h,
                                               freq = params$prophet_freq %||% "month")
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
  fitted_X <- X  # remember columns for predict
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
            description = paste(
              "Ordinary least-squares linear regression — the classical statistical baseline.",
              "Fast, fully interpretable (each coefficient is a marginal effect), and the right starting point",
              "for any numeric target. Assumes a roughly linear relationship between predictors and target,",
              "approximately Gaussian residuals, and no severe multicollinearity. Use it as a sanity check",
              "before reaching for fancier models — if a tuned XGBoost only marginally beats lm, your problem",
              "is probably linear and the simpler model wins on interpretability and stability."),
            params = list()),

  glmnet_reg = list(id = "glmnet_reg", label = "Elastic Net (glmnet)", engine = "R",
            task_types = c("regression"),
            fn = function(df, t, p, time_col=NULL) fit_glmnet(df, t, p, time_col, "gaussian"),
            description = paste(
              "Penalised linear regression with L1 (Lasso), L2 (Ridge), or a mixture (Elastic Net) regularisation.",
              "Set alpha=1 for Lasso (sparse coefficients, automatic feature selection), alpha=0 for Ridge",
              "(handles correlated predictors gracefully), or anywhere in between. Lambda is selected via",
              "cross-validation. Use this when you have many features (especially p ≫ n), correlated predictors,",
              "or want feature selection baked into the fit. Strong baseline for high-dimensional tabular data."),
            params = list(
              P("alpha","Alpha (0=Ridge, 1=Lasso)","numeric", 1, 0, 1, 0.05),
              P("nfolds","CV folds","integer", 5, 3, 20, 1))),

  gam = list(id = "gam", label = "GAM (mgcv)", engine = "R",
            task_types = c("regression","binary_classification","count"),
            fn = fit_gam,
            description = paste(
              "Generalised Additive Model — fits smooth nonlinear effects (penalised splines) for each numeric",
              "predictor while keeping the model additive and interpretable. Each smooth can be visualised on its",
              "own, so you see exactly how the target responds to each feature. Great middle ground between",
              "linear models and black-box ML when you suspect nonlinearity but still need to explain results",
              "to non-technical stakeholders. Enable 'Penalised selection' to let the fit shrink useless smooths to zero."),
            params = list(P("select","Penalised selection","logical", FALSE))),

  ranger_reg = list(id = "ranger_reg", label = "Random Forest (ranger)", engine = "R",
            task_types = c("regression"),
            fn = function(df, t, p, time_col=NULL) fit_ranger(df, t, p, time_col, FALSE),
            description = paste(
              "Random Forest regression via the fast ranger implementation. Builds many decorrelated decision",
              "trees on bootstrap samples and averages their predictions. Robust default with low tuning pain —",
              "handles nonlinearity, interactions, and mixed feature types out of the box. Provides impurity-based",
              "feature importance for free. Tends to under-perform gradient boosting on tabular benchmarks but",
              "is harder to overfit and a great robustness check. Increase num_trees for stability, lower",
              "min_node_size for more flexible trees."),
            params = list(
              P("num_trees","Number of trees","integer", 500, 50, 5000, 50),
              P("mtry","mtry (cols per split)","integer", NA, 1, 1000, 1),
              P("min_node_size","Min node size","integer", 5, 1, 100, 1))),

  xgb_reg = list(id = "xgb_reg", label = "XGBoost (R)", engine = "R",
            task_types = c("regression"),
            fn = function(df, t, p, time_col=NULL) fit_xgb(df, t, p, time_col, FALSE),
            description = paste(
              "Extreme Gradient Boosting — the gold-standard for tabular regression. Builds trees sequentially,",
              "each correcting the residuals of the previous ensemble. Highly tunable: eta controls learning rate",
              "(lower = more rounds needed but better generalisation), max_depth controls tree complexity,",
              "subsample/colsample inject randomness for regularisation. Wins most tabular Kaggle competitions and",
              "production benchmarks. Start with default values, then tune nrounds with early stopping or grid-search",
              "eta in {0.01, 0.05, 0.1} and max_depth in {3, 6, 9}."),
            params = list(
              P("nrounds","Boosting rounds","integer", 200, 10, 5000, 10),
              P("eta","Learning rate","numeric", 0.1, 0.001, 1, 0.01),
              P("max_depth","Max depth","integer", 6, 1, 20, 1),
              P("subsample","Row subsample","numeric", 0.9, 0.3, 1, 0.05),
              P("colsample_bytree","Col subsample","numeric", 0.9, 0.3, 1, 0.05),
              P("min_child_weight","Min child weight","numeric", 1, 0, 50, 0.5))),

  lightgbm_reg = list(id = "lightgbm_reg", label = "LightGBM", engine = "Python",
            task_types = c("regression"),
            fn = function(df, t, p, time_col=NULL) fit_lightgbm_py(df, t, p, time_col, FALSE),
            description = paste(
              "Microsoft's gradient-boosting library — like XGBoost but with leaf-wise (best-first) tree growth",
              "and histogram-based binning, making it markedly faster on large datasets and often slightly more",
              "accurate. num_leaves is the key tuning lever (higher = more flexible but more prone to overfitting).",
              "Excellent default choice when you have lots of data (>100k rows) and care about training speed.",
              "Runs through Python via reticulate — install with `reticulate::py_install('lightgbm')`."),
            params = list(
              P("n_estimators","Estimators","integer", 300, 10, 5000, 10),
              P("learning_rate","Learning rate","numeric", 0.05, 0.001, 1, 0.005),
              P("num_leaves","Num leaves","integer", 31, 4, 512, 1),
              P("max_depth","Max depth (-1=none)","integer", -1, -1, 32, 1))),

  catboost_reg = list(id = "catboost_reg", label = "CatBoost", engine = "Python",
            task_types = c("regression"),
            fn = function(df, t, p, time_col=NULL) fit_catboost_py(df, t, p, time_col, FALSE),
            description = paste(
              "Yandex's gradient boosting library, designed to handle categorical features natively without",
              "manual one-hot encoding (uses ordered target statistics under the hood). Tends to need less",
              "hyperparameter tuning than XGBoost / LightGBM and is especially strong on messy real-world",
              "business data with high-cardinality categoricals. Pick this when your data has many categorical",
              "columns or you want fewer knobs to twist. Runs through Python via reticulate."),
            params = list(
              P("iterations","Iterations","integer", 500, 10, 5000, 10),
              P("learning_rate","Learning rate","numeric", 0.05, 0.001, 1, 0.005),
              P("depth","Depth","integer", 6, 1, 12, 1))),

  mlp_reg = list(id = "mlp_reg", label = "Neural Net MLP (Keras)", engine = "Python",
            task_types = c("regression"),
            fn = function(df, t, p, time_col=NULL) fit_mlp_py(df, t, p, time_col, FALSE),
            description = paste(
              "Fully-connected feed-forward neural network (multi-layer perceptron) built with Keras / TensorFlow.",
              "Configure the architecture with comma-separated hidden units (e.g. '128,64,32'), tune dropout for",
              "regularisation and learning rate for convergence. On tabular data, gradient-boosted trees usually",
              "win — but neural nets shine when you have very large datasets, complex interactions, or want to",
              "combine tabular features with embeddings later. Falls back to scikit-learn's MLP if TensorFlow is",
              "not installed."),
            params = list(
              P("hidden","Hidden units (comma-sep)","text", "64,32"),
              P("epochs","Epochs","integer", 50, 1, 1000, 1),
              P("batch_size","Batch size","integer", 32, 4, 1024, 4),
              P("dropout","Dropout","numeric", 0.1, 0, 0.9, 0.05),
              P("learning_rate","Learning rate","numeric", 0.001, 1e-5, 1, 1e-4))),

  # ---- Classification ------------------------------------------------
  logit = list(id = "logit", label = "Logistic Regression", engine = "R",
            task_types = c("binary_classification"),
            fn = fit_logit,
            description = paste(
              "Binary logistic regression — the classical baseline for two-class problems. Models log-odds as",
              "a linear function of the features; coefficients translate to interpretable odds ratios.",
              "Threshold defaults to 0.5 on the predicted probability. Always run this first: if a tuned",
              "random forest only barely beats logit, your problem is essentially linear and the simpler model",
              "is more defensible in regulated domains (credit, healthcare, public policy)."),
            params = list()),

  glmnet_cls = list(id = "glmnet_cls", label = "Penalized Logistic (glmnet)", engine = "R",
            task_types = c("binary_classification"),
            fn = function(df, t, p, time_col=NULL) fit_glmnet(df, t, p, time_col, "binomial"),
            description = paste(
              "Logistic regression with L1 / L2 / Elastic Net regularisation. Same trade-offs as glmnet for",
              "regression: alpha=1 gives sparse Lasso solutions (automatic feature selection), alpha=0 gives",
              "Ridge (handles correlated predictors). Strong baseline for high-dimensional binary classification —",
              "text classification with TF-IDF features, genomic data, or any wide tabular problem. Lambda is",
              "chosen via cross-validation; the model returns class labels directly."),
            params = list(
              P("alpha","Alpha (0=Ridge,1=Lasso)","numeric", 1, 0, 1, 0.05),
              P("nfolds","CV folds","integer", 5, 3, 20, 1))),

  ranger_cls = list(id = "ranger_cls", label = "Random Forest Classifier", engine = "R",
            task_types = c("binary_classification","multiclass_classification"),
            fn = function(df, t, p, time_col=NULL) fit_ranger(df, t, p, time_col, TRUE),
            description = paste(
              "Random Forest for binary or multiclass classification via ranger. Same robust, low-tuning",
              "behaviour as the regression variant — ensembles of bootstrapped decision trees vote on the class.",
              "Handles imbalanced classes reasonably well, supports any number of classes natively, and gives",
              "you variable importance for free. A very safe default that's hard to make worse with bad",
              "hyperparameters. Increase num_trees if predictions feel unstable."),
            params = list(
              P("num_trees","Number of trees","integer", 500, 50, 5000, 50),
              P("min_node_size","Min node size","integer", 1, 1, 100, 1))),

  xgb_cls = list(id = "xgb_cls", label = "XGBoost Classifier", engine = "R",
            task_types = c("binary_classification","multiclass_classification"),
            fn = function(df, t, p, time_col=NULL) fit_xgb(df, t, p, time_col, TRUE),
            description = paste(
              "XGBoost for binary (logistic) or multiclass (softmax) classification. The performance benchmark",
              "for tabular classification on most real-world problems. Same tuning playbook as XGBoost regression:",
              "start with default eta=0.1 and max_depth=6, increase nrounds with early stopping, then explore",
              "learning-rate / depth combinations. Provides probability outputs internally; the wrapper returns",
              "class labels."),
            params = list(
              P("nrounds","Boosting rounds","integer", 200, 10, 5000, 10),
              P("eta","Learning rate","numeric", 0.1, 0.001, 1, 0.01),
              P("max_depth","Max depth","integer", 6, 1, 20, 1))),

  lightgbm_cls = list(id = "lightgbm_cls", label = "LightGBM Classifier", engine = "Python",
            task_types = c("binary_classification","multiclass_classification"),
            fn = function(df, t, p, time_col=NULL) fit_lightgbm_py(df, t, p, time_col, TRUE),
            description = paste(
              "LightGBM for binary or multiclass classification. Faster than XGBoost on large datasets thanks",
              "to histogram binning and leaf-wise tree growth, often with comparable or slightly better accuracy.",
              "Pick this when training time matters (>100k rows) or when you're benchmarking gradient-boosting",
              "variants against each other. Runs through Python via reticulate."),
            params = list(
              P("n_estimators","Estimators","integer", 300, 10, 5000, 10),
              P("learning_rate","Learning rate","numeric", 0.05, 0.001, 1, 0.005),
              P("num_leaves","Num leaves","integer", 31, 4, 512, 1))),

  mlp_cls = list(id = "mlp_cls", label = "Neural Net Classifier (Keras)", engine = "Python",
            task_types = c("binary_classification","multiclass_classification"),
            fn = function(df, t, p, time_col=NULL) fit_mlp_py(df, t, p, time_col, TRUE),
            description = paste(
              "Multi-layer perceptron classifier built with Keras / TensorFlow. Sigmoid output for binary,",
              "softmax for multiclass. Configure architecture with comma-separated hidden units, tune dropout",
              "to fight overfitting. Useful when you have many training examples and complex feature",
              "interactions, or as a building block before adding embeddings / images / text inputs. On purely",
              "tabular data a tuned LightGBM / XGBoost is usually the better choice."),
            params = list(
              P("hidden","Hidden units (comma-sep)","text", "64,32"),
              P("epochs","Epochs","integer", 50, 1, 1000, 1),
              P("batch_size","Batch size","integer", 32, 4, 1024, 4),
              P("dropout","Dropout","numeric", 0.1, 0, 0.9, 0.05))),

  # ---- Counts / proportions / survival -------------------------------
  poisson = list(id = "poisson", label = "Poisson GLM", engine = "R",
            task_types = c("count"), fn = fit_poisson,
            description = paste(
              "Poisson generalised linear model with a log link — the textbook model for count data",
              "(non-negative integers: events per period, claims per policy, visits per user). Assumes the",
              "mean equals the variance; if your data is over-dispersed (variance ≫ mean) prefer the",
              "Negative Binomial GLM instead. Coefficients are interpretable as multiplicative effects on",
              "the expected count (exp(beta) = rate ratio)."),
            params = list()),

  negbin = list(id = "negbin", label = "Negative Binomial GLM", engine = "R",
            task_types = c("count"), fn = fit_negbin,
            description = paste(
              "Negative Binomial GLM (MASS::glm.nb) — the standard upgrade from Poisson when count data is",
              "over-dispersed (a very common situation in real-world business data). Adds a dispersion",
              "parameter that lets variance exceed the mean. Same interpretable log-link as Poisson; coefficients",
              "translate to rate ratios. If Poisson residuals fan out, switch to this."),
            params = list()),

  betareg = list(id = "betareg", label = "Beta Regression", engine = "R",
            task_types = c("proportion"), fn = fit_betareg,
            description = paste(
              "Beta regression for continuous targets bounded in (0, 1) — proportions, rates, percentages",
              "that don't include exact 0 or 1. Models the mean via a logit link and a separate precision",
              "parameter, so it correctly captures the heteroscedasticity inherent in bounded data",
              "(variance shrinks near 0 and 1). Use this for things like market share, fractional defect",
              "rates, or vote shares — anything strictly between 0 and 1."),
            params = list()),

  # ---- Time series ---------------------------------------------------
  arima = list(id = "arima", label = "Auto ARIMA", engine = "R",
            task_types = c("time_series"), fn = fit_arima,
            description = paste(
              "Automatic ARIMA (AutoRegressive Integrated Moving Average) via forecast::auto.arima — the",
              "classical workhorse for univariate time series. Searches over (p, d, q) and seasonal (P, D, Q)",
              "orders using AIC/BIC, including differencing for stationarity. Excellent for short to medium",
              "univariate series with clear autocorrelation structure. Set the seasonal period correctly",
              "(12=monthly, 4=quarterly, 7=daily-with-weekly, 52=weekly-with-yearly). Returns point forecasts",
              "plus 95% prediction intervals."),
            params = list(
              P("ts_frequency","Seasonal period","integer", 12, 1, 365, 1),
              P("seasonal","Seasonal","logical", TRUE))),

  ets   = list(id = "ets", label = "ETS (exponential smoothing)", engine = "R",
            task_types = c("time_series"), fn = fit_ets,
            description = paste(
              "Exponential smoothing state-space model — decomposes the series into Error, Trend, and Seasonal",
              "components, each of which can be additive (A), multiplicative (M), or absent (N). Use 'ZZZ' to",
              "let the algorithm pick the best combination automatically by AIC. Often outperforms ARIMA on",
              "business series with clear trend and seasonality, and produces well-calibrated prediction",
              "intervals. A safer default than ARIMA when you don't want to think about differencing."),
            params = list(
              P("ts_frequency","Seasonal period","integer", 12, 1, 365, 1),
              P("ets_model","ETS spec (ZZZ=auto)","text", "ZZZ"))),

  tbats = list(id = "tbats", label = "TBATS (multi-seasonal)", engine = "R",
            task_types = c("time_series"), fn = fit_tbats,
            description = paste(
              "TBATS — Trigonometric, Box-Cox transformation, ARMA errors, Trend, and Seasonal components.",
              "Designed for time series with multiple, possibly non-integer, seasonal periods (e.g. daily data",
              "with both weekly and yearly cycles, or sub-hourly data with daily and weekly cycles). Slower",
              "to fit than ARIMA/ETS but the right tool when single-seasonality models can't capture all the",
              "structure. Pick this for high-frequency operational data (calls, web traffic, energy demand)."),
            params = list(P("ts_frequency","Seasonal period","integer", 12, 1, 365, 1))),

  prophet = list(id = "prophet", label = "Prophet (Meta)", engine = "R",
            task_types = c("time_series"), fn = fit_prophet,
            description = paste(
              "Meta's Prophet — additive model with piecewise-linear trend, Fourier-based seasonality",
              "(yearly / weekly / daily toggleable), and built-in holiday effects. Designed for business time",
              "series with strong seasonality, missing data, outliers, and trend changepoints — minimal tuning,",
              "sensible defaults, robust to messiness. Requires a date/time column. Excellent for KPIs,",
              "sales, traffic; weaker for series without clear seasonal structure where ARIMA/ETS may win."),
            params = list(
              P("yearly","Yearly seasonality","logical", TRUE),
              P("weekly","Weekly seasonality","logical", TRUE),
              P("daily","Daily seasonality","logical", FALSE),
              P("prophet_freq","Forecast frequency","select","month",
                choices = c("day","week","month","quarter","year"))))
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
