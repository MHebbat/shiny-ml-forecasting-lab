# Complex survey designs in the Lab

The Survey & Panel tab supports two design methods and two built-in
templates for German / euro-area household-finance panels.

## Design methods

### Standard (Taylor linearisation)
Backed by `survey::svydesign(ids, strata, weights, data, nest = TRUE)`.
Use this for most national household, labour, and expenditure surveys
(e.g. SOEP, ESS, EU-SILC).

| Form field | Maps to |
| --- | --- |
| Weight column | `weights = ~<col>` |
| Strata column | `strata = ~<col>` |
| PSU / cluster | `ids = ~<col>` (nested) |

If declaration without nesting succeeds, the Lab falls back to that and
flags it in the status banner.

### Replicate weights
Backed by `survey::svrepdesign(repweights, type, weights, combined.weights, mse)`.
Use this when your panel ships replicate weights (BRR, Fay, jackknife,
or bootstrap) instead of strata + PSU.

| Form field | Maps to |
| --- | --- |
| Weight column | `weights` (final probability weight) |
| Replicate-weight regex | `repweights = data[, regex]` |
| Replicate type | `type` ∈ {BRR, Fay, JK1, JK2, JKn, bootstrap, ACS} |
| Combined weights | `combined.weights` toggle |
| Use MSE | `mse` toggle (recommended TRUE) |

`survey::withReplicates()` is invoked automatically by `svytable` /
`svychisq` for crosstabs.

## Templates

Click **Apply PHF** or **Apply HFCN** in the design declaration card to
auto-fill the form.

### Bundesbank Panel on Household Finances (PHF)

| Field | Convention |
| --- | --- |
| Weight | `wgt` |
| Replicate weights | `^wr_` (Fay) |
| Combined weights | yes |
| Strata | `wsr` (sampling stratum) |
| PSU | `psu` (or `wsr` when no separate PSU column is present) |
| Household id | `hid` |
| Person id | `pid` |
| Wave | `wave` (or `welle`) |
| Implicates | 5 (long-format `implicate` column) |

`type = "Fay"` is the canonical setting for PHF. The Lab applies it on
template click; you can override before clicking **Declare design**.

### ECB Household Finance and Consumption Network (HFCN)

| Field | Convention |
| --- | --- |
| Weight | `hw0001` |
| Replicate weights | `^hw[0-9]{4}$` (bootstrap) |
| Combined weights | yes |
| Strata | `sa0100` |
| PSU | `sa0010` |
| Household id | `id` |
| Wave | `wave` |
| Implicates | 5 (wide format `*_imp1` … `*_imp5`) |

## Multi-implicate (MI) detection

The Lab automatically detects multiple imputations:

- **Long format** — a column literally named `implicate`, `imputation`,
  `imp`, or `imp_id` carrying integers 1…M. Each subset is one
  implicate.
- **Wide format** — variable names ending in `_imp1`, `_imp2`, … or
  `_i1`, `_i2`, …. The Lab strips the suffix to reconstruct the
  canonical column name in each implicate frame.
- **Explicit list** — pass `state$survey_design$implicate_list = list(df1, …)`.

When detected, the design declaration card surfaces a blue badge
("MI detected: N implicates"). Single-implicate descriptives still
appear on the **Descriptives** tab; the **MI Pooled** tab gives full
Rubin pooling.

## Variance estimation choices

The **Variance estimation** dropdown in the design declaration card
governs SE computation independently of the base design method:

| Choice | Effect |
| --- | --- |
| Taylor linearization | `survey::svydesign` SEs (default) |
| Bootstrap | `as.svrepdesign(type='bootstrap', replicates=R)` |
| JK1 / JKn | `as.svrepdesign(type='JK1' / 'JKn')` |
| BRR | `as.svrepdesign(type='BRR')` |
| Fay's BRR | `as.svrepdesign(type='Fay', fay.rho=…)` |
| ACS-style replicate weights | `as.svrepdesign(type='ACS')` |

Replicate count is exposed via a slider (50–2000, default 200). For
Fay's BRR, a `rho` slider 0…0.5 (default 0) sets `fay.rho`.

The chosen method is recorded on `state$survey_design$variance$method`
and on the manifest. Every weighted descriptive, weighted xtab, and
`svyglm` SE downstream of the declaration honours it.

## MI pipeline (Rubin's rules)

`R/mi_pipeline.R` provides a self-contained pipeline:

```r
implicates <- mi_detect(df, hint = list(kind = "long",
                                          column = "implicate"))
# or hint = list(kind = "wide", pattern = "(_imp|_i)([0-9]+)$")

designs <- mi_designs(implicates, design_spec = list(
  method = "replicate",
  weight = "wgt",
  repweights_pattern = "^wr_",
  repdesign_type = "Fay",
  combined_weights = TRUE, use_mse = TRUE,
  variance_method = "Fay", replicates = 200, brr_rho = 0
))

pooled <- mi_estimate(designs,
                       formula = y ~ x1 + x2,
                       family  = stats::gaussian,
                       model   = "svyglm")
diag <- mi_diagnostics(pooled)
```

`pooled` is a tibble (one row per term) with `term`, `estimate`,
`std.error`, `statistic`, `p.value`, `df`, `fmi`, `riv`, `lambda`.
`diag$avg_fmi` and `diag$avg_lambda` summarise sensitivity to
imputation choice across pooled terms.

### Worked example: PHF

PHF ships with 5 implicates in long format (`implicate` column) and
Fay-style replicate weights matching `^wr_`. The design spec is:

```r
spec <- list(method = "replicate",
              weight = "wgt",
              repweights_pattern = "^wr_",
              repdesign_type = "Fay",
              combined_weights = TRUE, use_mse = TRUE,
              variance_method = "Fay")
```

A weighted mean of household net wealth `nfn`, pooled across
implicates:

```r
implicates <- mi_detect(phf, hint = list(kind = "long",
                                           column = "implicate"))
designs    <- mi_designs(implicates, spec)
mean_nfn   <- mi_estimate(designs, formula = ~nfn, model = "svymean")
mi_diagnostics(mean_nfn)
```

A weighted regression of log net wealth on age and education:

```r
fit <- mi_estimate(designs,
                    formula = log(nfn) ~ age + factor(edu),
                    family = stats::gaussian, model = "svyglm")
fit[, c("term","estimate","std.error","fmi","lambda")]
```

### Worked example: HFCN

HFCN ships with 5 wide-format implicates (`*_imp1..5`) and bootstrap
replicate weights matching `^hw[0-9]{4}$`:

```r
spec <- list(method = "replicate",
              weight = "hw0001",
              repweights_pattern = "^hw[0-9]{4}$",
              repdesign_type = "bootstrap",
              combined_weights = TRUE, use_mse = TRUE)
implicates <- mi_detect(hfcn)             # auto-detects wide format
designs    <- mi_designs(implicates, spec)
ratio      <- mi_estimate(
  designs,
  numerator   = ~da3001,                # total assets
  denominator = ~hi0100,                # gross household income
  model = "svyratio")
```

### Reading `MIcombine` output

`mitools::MIcombine` returns coefficients (`$coefficients`), variance
matrix (`$variance`), and degrees of freedom (`$df`). `mi_estimate`
also reconstructs the within-imputation variance, between-imputation
variance, and computes:

- **fmi** — fraction of missing information.
- **riv** — relative increase in variance.
- **lambda** — `(1 + 1/m) * B / T`, where `B` is between-imp variance
  and `T` is total variance.

High `fmi` (e.g. > 0.3) flags a parameter that is sensitive to the
imputation; you may want more implicates or a richer imputation model.

## Send to Model Lab

After declaring a design, click **Send to Model Lab →**. The Lab will:

1. Materialise a derived dataset slot named
   `<original> (survey-prepped)` and switch active to it.
2. Set `state$meta$sample_weights`, `state$meta$strata`, `state$meta$psu`,
   `state$meta$replicate_weights` so Model Lab can pass them to fitters.
3. Switch the navbar to Model Lab.

Models that natively accept a `weights` argument (`lm`, `glm`, `glmnet`,
`ranger`, `xgboost`, `gam`, `survreg`) receive the survey weights
automatically. Models that do not (LSTM, Prophet, ARIMA-on-residuals,
beta-regression in some configurations) surface a notice in the run
log: "Survey sample weights ignored by this model".
