# Shiny ML & Forecasting Lab

> **Author:** Malik Hebbat · **License:** MIT

A **pure R Shiny** application for end-to-end predictive modeling — upload data at any cadence (daily / weekly / monthly / quarterly / yearly), pick from a curated registry of regression, classification, count, proportion, survival, and time-series models, tune hyperparameters in the UI, and persist runs to SQLite for comparison.

R is the primary language. **Python (via [`reticulate`](https://rstudio.github.io/reticulate/)) is optional** — used only for the LightGBM / CatBoost / Keras-MLP wrappers. The app degrades gracefully if Python is not configured.

```
Ingest → Explore → Data Prep → Privacy Audit → Survey & Panel → Model Lab → Predict → Dashboard → Editorial Studio → Runs
```

## Easy install

One command. R packages + Python TF/Keras venv + parse check, all idempotent.

```bash
git clone https://github.com/MHebbat/shiny-ml-forecasting-lab.git
cd shiny-ml-forecasting-lab
Rscript setup.R              # full install
Rscript setup.R --dry-run    # preview, no changes
```

Wrappers: `./setup.sh` (macOS/Linux), `.\setup.ps1` (Windows). Then run:

```bash
./run.sh         # macOS / Linux
run.bat          # Windows
make run         # via make
```

The launcher opens the app at `http://127.0.0.1:4848`. Full instructions in [`SETUP.md`](SETUP.md).

## Large uploads

The default upload limit is **5 GB**. Override with the `SHINYML_MAX_UPLOAD_GB`
environment variable before launching:

```bash
SHINYML_MAX_UPLOAD_GB=20 ./run.sh
```

This is set via `options(shiny.maxRequestSize = ...)` in `app.R`.

## Survey templates (PHF & HFCN)

The Survey & Panel tab ships with one-click templates for two euro-area
household-finance panels:

- **Bundesbank Panel on Household Finances (PHF)** — replicate weights
  via `^wr_` (Fay), 5 implicates (long), strata `wsr`, household id `hid`.
- **ECB Household Finance and Consumption Network (HFCN)** — replicate
  weights via `^hw[0-9]{4}$` (bootstrap), 5 implicates (wide
  `*_imp1..5`), strata `sa0100`.

Pick **Replicate weights** as the design method, then click **Apply PHF**
or **Apply HFCN** to auto-fill the form. See
[`docs/survey_designs.md`](docs/survey_designs.md) for the full column
conventions.

After declaring the design, click **Send to Model Lab →**: a derived
dataset slot is materialised, `state$meta$sample_weights` is wired up,
and the navbar switches to Model Lab. Models that support `weights`
(`lm`, `glm`, `glmnet`, `ranger`, `xgboost`, `gam`) automatically use
the survey weights; the run log states explicitly when a model ignores
them.

## Project save / load + brief reports

Every tab exposes **Save Project**: writes a self-contained bundle to
`~/.shinyml/projects/<name>/` containing the recipe, survey design,
trained model (if any), and the reproducibility manifest. The navbar
**PROJECT** picker lists saved bundles; **Load** rehydrates the state.

**Brief Report (HTML / PDF)** is exportable from Model Lab, Survey, and
Editorial Studio. PDF is rendered via `pagedown::chrome_print` when
Chrome is detected, with a fallback to `rmarkdown` + `pandoc`. If
neither is available, the HTML output is produced and a notice is
surfaced.

## Reproducibility manifest

Every project save and report export embeds a manifest with: app
version (git commit), dataset (name, n_rows × n_cols, SHA-256, source),
recipe step list, survey design (if any), model spec, validation
strategy, headline metrics + bootstrap CI, distilled `sessionInfo()`,
Python / TF versions when reticulate is active, and the git commit
hash. Click **Show Manifest** in Model Lab or Editorial Studio to view
it pretty-printed.

## Theme picker

The navbar has two pickers:

- **Chrome** — app-wide chrome theme. **Bundesbank Light** is the
  default; **Editorial Dark** preserves the existing dark UI;
  **Light Minimal** for a clean light variant.
- **Plot theme** — affects every Plotly chart. Defaults to Bundesbank.

## Features

- **Multi-format ingest**: CSV / TSV / TXT / XLSX / Parquet / JSON / RDS, plus **Stata `.dta`**, **SPSS `.sav`/`.por`**, and **SAS `.sas7bdat`/`.xpt`** via [`haven`](https://haven.tidyverse.org/) — variable and value labels are preserved and surfaced in Explore, Survey, and Codebook views
- **Frequency-aware**: declare daily / weekly / monthly / quarterly / yearly
- **Auto task detection**: regression · binary / multiclass classification · count · proportion · time series
- **🧪 Editorial Data Prep** — magazine-style health report (column profile, leakage scan, skew/missingness grading) with an editable **recipe builder**: drop zero-variance / ID-like / high-missing columns, impute, lump rare categories, derive date features, Yeo-Johnson transform, winsorize, scale, one-hot encode. Optional **AI co-pilot** suggests steps from the column profile only — never raw rows.
- **🔒 Privacy Audit** — PII pattern scan (email, phone, IBAN, IPv4, postal, credit card) plus EN/DE sensitive-keyword detection, **k-anonymity** on quasi-identifier combos, deterministic pseudonymizer, append-only audit log in SQLite, and a hard **AI egress gate** (`state$privacy_allow_ai`) that blocks any external AI call when toggled off.
- **📊 Survey & Panel Suite** — declare a [`survey`](https://CRAN.R-project.org/package=survey) design (weights, strata, PSU, FPC), get **weighted descriptives**, Likert detection, weighted crosstabs with `svychisq`, label-aware codebook, and panel handlers (per-wave counts, attrition, balanced/unbalanced detection).
- **Model registry** (extend in `R/model_registry.R`) — each model has a built-in description shown in the UI:
  - Regression — `lm`, `glmnet` (Lasso/Ridge/Elastic Net), `mgcv::gam`, `ranger`, `xgboost`, **LightGBM** (Py), **CatBoost** (Py), **Keras MLP** (Py)
  - Classification — `glm` logit, penalized logit (`glmnet`), `ranger`, `xgboost`, LightGBM, Keras MLP
  - Counts / proportions — Poisson GLM, Negative Binomial GLM, Beta regression
  - Time series — Auto ARIMA, ETS, TBATS, Prophet
  - **Deep learning** — **LSTM** (recurrent, sequence forecasting + classification), **KAN** (Kolmogorov–Arnold Network, BETA, Python via `pykan`)
- **🎛 Training Studio** (the rebuilt Model Lab) — every model now ships with a plain-English description, *best-for / avoid-when* bullet lists, an availability badge for required dependencies, a reference link, and a collapsible "About this model" drawer with a 4-paragraph deep-dive (math, assumptions, when classical baselines win, references). Hyperparameters are grouped into logical sections (Boosting / Tree structure / Regularization / Sampling / Stopping for tree-based models; Architecture / Optimization / Training schedule / Reproducibility for deep models) with **Suggested defaults / Aggressive / Conservative** preset buttons. A separate **Training configuration** panel exposes validation strategy (holdout / k-fold / repeated CV / rolling-origin), bootstrap iterations, class weighting (none / inverse-frequency / SMOTE), early stopping, parallel backend (`future::plan(multisession)` if `future` is installed), random-seed lock, and a **compute-budget cap (minutes)** that time-bounds the run via `setTimeLimit`. A live **Resolved run parameters** card prints the JSON that will actually be passed to the fitter before training, and a **Diagnostics** tab surfaces backend / epochs run / final loss / best iteration plus a training-loss curve where applicable.
- **Per-model tuning UI** generated from the registry's parameter spec — sliders, numeric inputs, toggles, and selects, grouped by purpose
- **Validation modes**: holdout split · K-fold CV · repeated K-fold CV · rolling-origin CV (time series)
- **Forecast horizon** configurable per run (steps mapped to your declared frequency)
- **🔥 Results Dashboard** auto-opens after every run — KPI tiles, fit plot, full residual diagnostics (residuals-vs-fitted, histogram, Q-Q, ACF), searchable per-row prediction table, and a flat searchable stat-cards view for every metric, hyperparameter, and diagnostic.
- **🤖 AI Verdict** — one click sends the run to OpenAI, Anthropic, or Perplexity for an objective, blunt assessment (PASS / WEAK / FAIL grade, residual critique, concrete next-step suggestions). Falls back to a deterministic heuristic verdict if no API key is set so the feature always works.
- **📰 Editorial Studio** — magazine-style presentation of the latest run: hero headline, deck, gold-accent metric grid, hero forecast plot, pull-quote, and one-click PNG / standalone-HTML export. Three layouts (Cover · Spread · Minimal) with Playfair Display + Inter typography.
- **Searchable Stats Explorer** — every metric / hyperparameter / diagnostic across every run, flattened into one DataTable with global + per-column search and CSV / Excel export. Find every `xgboost` run with `eta=0.05` in one filter.
- **Persistence**: every run, hyperparameter set, prediction, and AI analysis is stored in `db/app.sqlite`
- **Leaderboard**: compare runs across datasets / models / metrics
- **Dark theme** (`bslib` darkly + custom CSS)
- **Plot themes** — Studio (dark editorial) · Light minimal · **Bundesbank**. The Bundesbank theme reproduces the Deutsche Bundesbank Monthly Report look (corporate dark blue `#003D7C`, secondary red `#B61F29`, ochre, teal, neutral grey, light grey gridlines, Inter typography, white background). Switch globally from the navbar — every chart in Explore / Data Prep / Predict / Dashboard / Editorial Studio re-renders in the active theme.

## Deep learning models

Two deep-learning models ship with the Training Studio. Both are *optional* — the rest of the app installs and runs without them; if their dependencies are missing the UI shows an inline notice and the **Train** button refuses to start.

### LSTM (Long Short-Term Memory)

Recurrent network with gating that captures long-range dependencies in sequences. Primary task: **time-series forecasting**. Secondary: **sequence classification**. The Training Studio exposes the full deep-learning block: optimizer (adam / sgd / rmsprop / adamw), learning rate + weight decay, batch size, epochs, validation split, early-stopping patience and restore-best-weights, LR scheduler (none / step / cosine / reduce-on-plateau), loss override, mixed-precision toggle, gradient clipping (off / value / norm), and the LSTM-specific knobs: timesteps, n_features (auto-detected from the prepped frame), stateful, return_sequences, bidirectional, stacked layers (1..4), dropout, recurrent_dropout.

**Backend.** Primary: R [`keras3`](https://keras3.posit.co/) (current CRAN package, Keras 3 + TensorFlow via `reticulate`). Legacy fallback: the older R [`keras`](https://keras.posit.co/) package. Univariate-only fallback: [`TSLSTMplus`](https://cran.r-project.org/package=TSLSTMplus). Install:

```r
# Recommended — current Keras 3 stack
install.packages("keras3")
keras3::install_keras()        # provisions the Python TF runtime once
# restart R / the app afterwards

# Legacy fallback (if you already have it set up)
# install.packages("keras"); keras::install_keras()

# Univariate-only fallback, no Python required
install.packages("TSLSTMplus")
```

The Model Lab's availability panel surfaces the exact reason when the model is unavailable: missing R package, missing TensorFlow Python backend, or both. The Train button refuses with the same actionable message rather than crashing the session.

### KAN (Kolmogorov–Arnold Network) — BETA

Research-grade architecture inspired by the Kolmogorov–Arnold representation theorem; learnable univariate functions live on edges instead of fixed activations on nodes. Best for smooth low- to medium-dimensional regression and PDE-style problems where interpretability of the learned 1-D functions matters. Disabled (with an inline notice pointing to LSTM / Prophet / ARIMA) when the dataset's task type is `time_series`.

**Backend.** Python [`pykan`](https://github.com/KindXiaoming/pykan) + `torch` via `reticulate`. There is no mainstream R port — the model card is explicit about that. Install once from inside R:

```r
reticulate::py_install(c("pykan", "torch"))
```

The Studio exposes `width` (layer widths — accepts the literal token `n_features`, substituted at runtime), `grid` (spline knots), `k` (B-spline order), `seed`, the regularisation family (`lamb`, `lamb_l1`, `lamb_entropy`, `lamb_coef`, `lamb_coefdiff`), `steps`, `optimizer` (LBFGS / Adam), `learning_rate`, `prune_threshold`, plus a "Plot learned 1-D functions" toggle (best-effort — pykan's `model.plot()` is invoked during fitting).

A yellow **BETA** badge sits next to the engine pill in the Studio whenever a research-grade model is selected.

## AI configuration

The AI Verdict feature uses environment variables to find provider keys. Set any one of them before launching `shiny::runApp()`:

```bash
export OPENAI_API_KEY="sk-..."
# or
export ANTHROPIC_API_KEY="sk-ant-..."
# or
export PPLX_API_KEY="pplx-..."
```

With no keys set, the dashboard's AI tab still works — it returns a rule-based verdict using R², residual bias, heteroscedasticity, ACF lag-1, and class-balance heuristics.

## Quick start

```r
# 1) Install R dependencies (one-time)
source("install_R.R")

# 2) (Optional) Install Python deps if you want LightGBM / CatBoost / Keras
# In R:
reticulate::py_install(c("lightgbm","catboost","tensorflow","scikit-learn"))

# 3) Launch
shiny::runApp("/path/to/shiny_ml_app")
```

The first run creates `db/app.sqlite` automatically.

## Project layout

```
shiny_ml_app/
├── app.R                   # Main entry point — UI + server wiring
├── install_R.R             # One-time R package installer
├── requirements.txt        # Optional Python deps
├── README.md
├── R/
│   ├── db.R                # SQLite layer
│   ├── utils.R             # Helpers, metric calculators, file readers
│   ├── model_registry.R    # ★ All models live here — add yours easily
│   ├── python_bridge.R     # reticulate plumbing
│   ├── mod_ingest.R        # Tab 1
│   ├── mod_explore.R       # Tab 2
│   ├── mod_modellab.R      # Tab 3
│   ├── mod_predict.R       # Tab 4
│   └── mod_runs.R          # Tab 5
├── python/
│   └── py_models.py        # LightGBM / CatBoost / Keras MLP wrappers
├── www/
│   └── custom.css          # Dark theme polish
├── data/                   # (optional) place sample CSVs here
└── db/
    └── app.sqlite          # Auto-created on first run
```

## Adding a new model

Edit `R/model_registry.R`:

```r
fit_my_model <- function(df, target, params, time_col = NULL) {
  m <- my_pkg::fit(...)
  list(
    model    = m,
    predict  = function(newdata, h = NULL) as.numeric(predict(m, newdata)),
    feat_imp = NULL  # optional named numeric vector
  )
}

MODELS$my_model <- list(
  id = "my_model", label = "My Model", engine = "R",
  task_types = c("regression"),
  fn = fit_my_model,
  params = list(
    P("lr", "Learning rate", "numeric", default = 0.1, min = 0, max = 1, step = 0.01)
  )
)
```

The UI auto-generates the tuning panel from `params`.

## Author

**Malik Hebbat** — full-stack developer, data scientist, blockchain developer.

Contributions, issues, and forks welcome.

## What's intentionally left out

- Authentication / multi-user (use `shinymanager` or Posit Connect)
- Cloud deployment (works fine on `shinyapps.io`, Posit Connect, or Docker)
- Live streaming ingest — only batch upload + scheduled re-ingest stub
- Bayesian hyperparameter optimization (drop in via `tune::tune_bayes` if needed)
- SHAP / `iml` explainability panel — easy to add (basic feature importance is shown)
