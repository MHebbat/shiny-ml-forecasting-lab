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
  `imp`, or `imp_id` carrying integers 1…M.
- **Wide format** — variable names ending in `_imp1`, `_imp2`, … or
  `_i1`, `_i2`, … such that ≥2 distinct indices appear.

When detected, the design declaration card surfaces a blue badge
("MI detected: N implicates"). The current commit uses the **first
implicate** for descriptives and crosstabs and surfaces a notice. Pooling
via Rubin's rules (`mitools::MIcombine`) is staged for commit B.

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
