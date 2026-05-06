# Causal inference notes

This document accompanies the **Predictor analysis** sub-section of the
Model Lab Diagnostics tab. It explains the difference between a model's
predictive importance and a *causal* effect, and when each of the
optional causal scaffolds bundled with the lab is appropriate.

## DAG-first reasoning

A directed acyclic graph (DAG) is the cheapest way to be honest about
what your data can and cannot answer. Before estimating any causal
quantity, write down:

1. The **outcome** (Y).
2. The **treatment / exposure** (D).
3. The **confounders** that affect both D and Y.
4. The **mediators** that lie on the path D -> Y.
5. The **colliders** you must NOT condition on.

If you cannot draw the DAG, you cannot interpret a coefficient causally.
Predictive importance is fine for its own purpose — picking features
that improve forecast accuracy — but it is not a causal effect.

## Predictive importance vs. causal effect

| Quantity | Tells you... | Does NOT tell you... |
|---|---|---|
| Permutation / built-in importance | which features the *fitted* model leans on | what would happen if you intervened on a feature |
| Standardised coefficient | the *partial association* with Y in the fitted model | the causal effect, unless confounding is correctly handled |
| Partial-dependence plot | the *average prediction* as you sweep one feature | the counterfactual response at the unit level |

## When each scaffold is appropriate

### `did_2x2(df, y, treat, post, controls)`
Two-period 2x2 difference-in-differences. **Identification assumes
parallel pre-trends** between treated and control. Validate with an
event-study (e.g. plotting outcome trajectories or running a saturated
event-time regression). DiD is sensitive to:

- staggered treatment adoption — see Goodman-Bacon (2021), Callaway &
  Sant'Anna (2021), de Chaisemartin & D'Haultfoeuille (2020) for
  modern fixes.
- compositional changes between periods.
- time-varying confounders.

### `ipw_ps(df, y, treat, covariates, estimand)`
Inverse probability weighting from a logit propensity-score model.
Identification requires **no unmeasured confounding** and **overlap**
(both treated and control units across the support of the
covariates). Always check covariate balance after weighting and
consider trimming or stabilising weights.

### `iv_2sls(df, y, x_endog, instruments, controls)`
Two-stage least squares via `AER::ivreg`. Identification requires:

- **Relevance** — the first-stage F should comfortably exceed 10
  (Stock & Yogo); weak instruments invalidate inference.
- **Exclusion restriction** — the instrument affects Y only through
  the endogenous regressor.

### `robust_coef_table(model, type)`
Heteroskedasticity-robust standard errors via `sandwich::vcovHC` and
`lmtest::coeftest`. Use for inference when residual variance is not
constant. HC3 is the default; HC1 is appropriate for very large
samples.

## Bundesbank-relevant references

- Angrist, J. D., & Pischke, J.-S. (2009). *Mostly Harmless
  Econometrics*. Princeton.
- Pearl, J., & Mackenzie, D. (2018). *The Book of Why*. Basic Books.
- Cunningham, S. (2021). *Causal Inference: The Mixtape*. Yale.
- Imbens, G. W., & Rubin, D. B. (2015). *Causal Inference for
  Statistics, Social, and Biomedical Sciences*. Cambridge.
- Callaway, B., & Sant'Anna, P. H. C. (2021). "Difference-in-
  Differences with multiple time periods." *Journal of Econometrics*.
- Goodman-Bacon, A. (2021). "Difference-in-differences with variation
  in treatment timing." *Journal of Econometrics*.

## Bottom line

Use the importance / PDP block to **describe** the fitted model. Use
the causal scaffolds only after you have written down the DAG, checked
the assumptions specific to the chosen method, and validated the
diagnostics that go with it. The scaffolds in this lab are not a
substitute for a careful identification strategy.
