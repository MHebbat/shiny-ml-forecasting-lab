# Shiny ML & Forecasting Lab

> **Author:** Malik Hebbat · **License:** MIT

A **pure R Shiny** application for end-to-end predictive modeling — upload data at any cadence (daily / weekly / monthly / quarterly / yearly), pick from a curated registry of regression, classification, count, proportion, survival, and time-series models, tune hyperparameters in the UI, and persist runs to SQLite for comparison.

R is the primary language. **Python (via [`reticulate`](https://rstudio.github.io/reticulate/)) is optional** — used only for the LightGBM / CatBoost / Keras-MLP wrappers. The app degrades gracefully if Python is not configured.

```
Ingest → Explore → Model Lab → Predict → Dashboard → Editorial Studio → Runs
```

## Quick start (Mac · Windows · Linux — identical experience)

```bash
git clone https://github.com/MHebbat/shiny-ml-forecasting-lab.git
cd shiny-ml-forecasting-lab

# macOS / Linux
./run.sh

# Windows
run.bat

# Or with make
make install && make run
```

The launcher checks for `Rscript`, installs missing R packages (idempotent), and opens the app at `http://127.0.0.1:4848`. Full instructions in [`SETUP.md`](SETUP.md).

## Features

- **Multi-format ingest**: CSV / TSV / TXT / XLSX / Parquet / JSON / RDS
- **Frequency-aware**: declare daily / weekly / monthly / quarterly / yearly
- **Auto task detection**: regression · binary / multiclass classification · count · proportion · time series
- **Recipe builder**: missingness handling, one-hot encoding, scaling, lag features, rolling means
- **Model registry** (extend in `R/model_registry.R`) — each model has a built-in description shown in the UI:
  - Regression — `lm`, `glmnet` (Lasso/Ridge/Elastic Net), `mgcv::gam`, `ranger`, `xgboost`, **LightGBM** (Py), **CatBoost** (Py), **Keras MLP** (Py)
  - Classification — `glm` logit, penalized logit (`glmnet`), `ranger`, `xgboost`, LightGBM, Keras MLP
  - Counts / proportions — Poisson GLM, Negative Binomial GLM, Beta regression
  - Time series — Auto ARIMA, ETS, TBATS, Prophet
- **Per-model tuning UI** generated from the registry's parameter spec — sliders, numeric inputs, toggles, and selects
- **Validation modes**: holdout split · K-fold CV · rolling-origin CV (time series)
- **Forecast horizon** configurable per run (steps mapped to your declared frequency)
- **🔥 Results Dashboard** auto-opens after every run — KPI tiles, fit plot, full residual diagnostics (residuals-vs-fitted, histogram, Q-Q, ACF), searchable per-row prediction table, and a flat searchable stat-cards view for every metric, hyperparameter, and diagnostic.
- **🤖 AI Verdict** — one click sends the run to OpenAI, Anthropic, or Perplexity for an objective, blunt assessment (PASS / WEAK / FAIL grade, residual critique, concrete next-step suggestions). Falls back to a deterministic heuristic verdict if no API key is set so the feature always works.
- **📰 Editorial Studio** — magazine-style presentation of the latest run: hero headline, deck, gold-accent metric grid, hero forecast plot, pull-quote, and one-click PNG / standalone-HTML export. Three layouts (Cover · Spread · Minimal) with Playfair Display + Inter typography.
- **Searchable Stats Explorer** — every metric / hyperparameter / diagnostic across every run, flattened into one DataTable with global + per-column search and CSV / Excel export. Find every `xgboost` run with `eta=0.05` in one filter.
- **Persistence**: every run, hyperparameter set, prediction, and AI analysis is stored in `db/app.sqlite`
- **Leaderboard**: compare runs across datasets / models / metrics
- **Dark theme** (`bslib` darkly + custom CSS)

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
