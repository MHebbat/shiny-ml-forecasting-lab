# Smoke-test the predictor-analysis helpers on synthetic data.
# Does NOT require optional pkgs (iml, pdp, sandwich, lmtest, AER):
# the helpers must degrade gracefully.
suppressMessages(source("R/utils.R"))
suppressMessages(source("R/causal_helpers.R"))

set.seed(7)
n <- 100
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
y  <- 1 + 0.7 * x1 - 0.3 * x2 + rnorm(n, 0, 0.5)
df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

# Fit a vanilla lm wrapped in the registry's fit-shape (model + predict)
m_lm <- lm(y ~ x1 + x2 + x3, data = df)
fit  <- list(
  model = m_lm,
  predict = function(newdata) {
    p <- predict(m_lm, newdata = newdata)
    data.frame(predicted = as.numeric(p))
  }
)

# 1) Importance — built-in (abs t-statistic) for lm
imp <- predictor_importance(fit, df, "y", method = "builtin")
stopifnot(!is.null(imp))
stopifnot(all(c("feature","importance","source") %in% names(imp)))
stopifnot(nrow(imp) == 3L)
stopifnot(all(is.finite(imp$importance)))

# 2) Partial dependence — hand-rolled grid (no pdp dep)
pdp_list <- partial_dependence(fit, df, "y",
                                features = c("x1","x2"), grid_n = 10)
stopifnot(length(pdp_list) == 2L)
stopifnot(all(c("x","yhat","feature") %in% names(pdp_list[[1]])))
stopifnot(nrow(pdp_list[[1]]) == 10L)
stopifnot(all(is.finite(pdp_list[[1]]$yhat)))

# 3) DiD scaffold
df_did <- data.frame(y = c(rnorm(50,0), rnorm(50,1.0)),
                     treat = rep(c(0,1), each = 50),
                     post  = rep(c(0,1, 0,1), each = 25))
did <- did_2x2(df_did, "y", "treat", "post")
stopifnot(!is.null(did$ate), is.finite(did$ate))
stopifnot(grepl("DiD", did$notes))

# 4) IPW scaffold
df_ps <- data.frame(y = rnorm(100), treat = rbinom(100, 1, 0.4),
                    x = rnorm(100))
ipw <- ipw_ps(df_ps, "y", "treat", covariates = "x", estimand = "ATE")
stopifnot(is.finite(ipw$estimate))
stopifnot(grepl("IPW", ipw$notes))

# 5) IV scaffold gracefully reports missing AER
iv <- iv_2sls(df_ps, "y", "treat", instruments = "x")
stopifnot(is.list(iv), !is.null(iv$notes))
# When AER is installed -> ok = TRUE; otherwise ok = FALSE with a hint.

# 6) Robust SEs scaffold gracefully reports missing sandwich/lmtest
rs <- robust_coef_table(m_lm, type = "HC3")
stopifnot(is.list(rs), !is.null(rs$notes))

cat("test_predictor_analysis.R: OK\n")
