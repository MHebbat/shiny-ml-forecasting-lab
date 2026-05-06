# =====================================================================
# In-app documentation content
#
# Each entry: list(title=, what=, saves=, downstream=, methods=, hints=)
# All fields are markdown strings. doc_chip() (in mod_docs.R) renders
# them as a small "?" chip + offcanvas drawer.
#
# Author: Malik Hebbat
# =====================================================================

DOCS <- list(
  ingest = list(
    title = "Data Ingest",
    what = paste(
      "Reads a tabular file from disk into a workspace dataset.",
      "Supports CSV / TSV / TXT / XLSX / Parquet / JSON / RDS plus",
      "Stata `.dta`, SPSS `.sav`/`.por`, SAS `.sas7bdat`/`.xpt`.",
      "Variable and value labels are preserved via `haven` and surfaced",
      "in the codebook and survey views."),
    saves = paste(
      "Each upload creates a new entry in `state$datasets` and activates",
      "it. The active slot mirrors into shortcuts (`state$raw_data`,",
      "`state$dataset_name`, `state$labels`). A row in SQLite `datasets`",
      "is created when you click 'Save dataset'."),
    downstream = paste(
      "Explore reads `state$raw_data` for diagnostics. Data Prep builds",
      "a recipe over it. Privacy Audit scans it. Survey & Panel uses",
      "the labels. Model Lab fits to `state$prepped` (set by Data Prep)."),
    methods = paste(
      "File extension dispatch in `R/utils.R::read_uploaded`.",
      "Labels parsed via `haven::read_*` and `extract_labels()`.",
      "Survey-design column hints from naming heuristics in",
      "`detect_survey_columns()` (PHF, SOEP, ESS conventions)."),
    hints = paste(
      "Default upload limit is 5 GB. Override with",
      "env var `SHINYML_MAX_UPLOAD_GB` before launching the app.")
  ),
  explore = list(
    title = "Explore",
    what = paste(
      "Read-only diagnostics. Schema, missingness, target distribution,",
      "correlation matrix and pair table, optional preview of the",
      "preprocessed frame."),
    saves = "Nothing — Explore is purely a viewer.",
    downstream = paste(
      "Findings here motivate the recipe steps you choose in Data Prep."),
    methods = paste(
      "Correlations: `explore_correlation()` in `mod_explore.R`,",
      "method = pearson | spearman | kendall, NAs handled pairwise."),
    hints = "Wide datasets (>200 columns) — pin top-N with the picker."
  ),
  dataprep = list(
    title = "Data Prep",
    what = paste(
      "Build a JSON-serialisable recipe of preprocessing steps. Each",
      "step has an id, a human label, a target scope (selector token or",
      "explicit columns), and parameters."),
    saves = paste(
      "On Apply, sets `state$prepped`, `state$prep_log`, and writes a",
      "row into SQLite `prep_reports`."),
    downstream = paste(
      "Model Lab fits on `state$prepped`. The recipe and apply log are",
      "embedded in every project save and report export."),
    methods = paste(
      "Selector tokens: `all_predictors`, `all_numeric`,",
      "`all_categorical`, `all`. Time column is shielded from numeric",
      "transforms; only `date_features` and `parse_datetime` may touch",
      "it. See `R/recipe_builder.R` for the full vocabulary."),
    hints = paste(
      "Date parsing falls back to `lubridate::parse_date_time` with a",
      "battery of common orders. If a column refuses to parse, declare",
      "its format explicitly via `parse_datetime`.")
  ),
  privacy = list(
    title = "Privacy Audit",
    what = paste(
      "PII pattern scan, sensitive-keyword detection (EN/DE),",
      "k-anonymity probe on quasi-identifiers, deterministic",
      "pseudonymiser. The AI egress gate (`state$privacy_allow_ai`) is",
      "set here."),
    saves = "Append-only audit entries in SQLite `privacy_log`.",
    downstream = paste(
      "When the AI gate is OFF, Data Prep / Dashboard refuse to call",
      "external AI providers."),
    methods = paste(
      "Regex catalogue: email, phone, IBAN, IPv4, postal, credit card.",
      "k-anonymity computed on user-chosen quasi-identifier subset."),
    hints = "Toggle the AI gate before running AI prep / verdict."
  ),
  survey = list(
    title = "Survey & Panel",
    what = paste(
      "Declare a survey design (Standard Taylor or Replicate weights)",
      "and inspect the dataset under that design. Multi-implicate (MI)",
      "data is detected automatically; the first implicate is used for",
      "descriptives in this commit."),
    saves = paste(
      "On Declare, writes `state$survey_design` and a row into SQLite",
      "`survey_designs`. 'Send to Model Lab' materialises a derived",
      "dataset slot and activates it."),
    downstream = paste(
      "Model Lab uses `state$meta$sample_weights` to pass `weights` to",
      "compatible fitters (`lm`, `glm`, `glmnet`, `ranger`, `xgboost`,",
      "`gam`). Models that don't natively support weights surface a",
      "notice in the run log."),
    methods = paste(
      "Backed by the `survey` package. Templates: PHF (Bundesbank Panel",
      "on Household Finances) and HFCN (ECB Household Finance and",
      "Consumption Network) auto-fill the form with their respective",
      "schemas. See `docs/survey_designs.md` for column conventions."),
    hints = paste(
      "PHF: weight `wgt`, replicate weights `^wr_`, type Fay, 5",
      "implicates. HFCN: weight `hw0001`, replicate weights",
      "`^hw[0-9]{4}$`, type bootstrap, 5 wide implicates.")
  ),
  modellab = list(
    title = "Model Lab",
    what = paste(
      "Fit any model in the registry with full hyperparameter control,",
      "validation strategy, class weighting, compute-budget cap, and a",
      "live JSON view of the resolved run parameters before training."),
    saves = paste(
      "On Train: `state$last_model`, `state$last_params`,",
      "`state$last_run_id`. SQLite `runs` and `predictions` tables get",
      "rows for each completed run."),
    downstream = paste(
      "Results Dashboard auto-opens. Editorial Studio prints an",
      "issue. The Brief Report and Project Save consume this state."),
    methods = paste(
      "Validation: holdout / k-fold / repeated CV / rolling-origin.",
      "Class weighting: none | inverse-frequency | SMOTE (themis).",
      "Compute budget caps the run via `setTimeLimit`."),
    hints = paste(
      "When a survey design is declared, `state$meta$sample_weights`",
      "is honoured by every model that supports it.")
  ),
  predict = list(
    title = "Forecast / Predict",
    what = "Generate predictions on new data using the last trained model.",
    saves = "No state mutation — output is downloadable as CSV.",
    downstream = "Predictions can be exported and brought back as input.",
    methods = "Forecast horizon mapped to the declared frequency.",
    hints = "Time-series models return point + lower/upper bands."
  ),
  dashboard = list(
    title = "Results Dashboard",
    what = "Headline KPI tiles, fit plot, residual diagnostics, searchable per-row predictions.",
    saves = "Nothing — read-only.",
    downstream = "AI verdict and bootstrap CI flow into the Brief Report.",
    methods = "Diagnostics: residuals-vs-fitted, histogram, Q-Q, ACF.",
    hints = "Use the searchable stats explorer to find runs with specific hyperparameters."
  ),
  studio = list(
    title = "Editorial Studio",
    what = "Magazine-style presentation of the latest run.",
    saves = "Exports HTML / forecast CSV.",
    downstream = "—",
    methods = "Three layouts: Cover, Spread, Minimal.",
    hints = "The dark Editorial chrome is preserved here as opt-in even when the app theme is Bundesbank Light."
  ),
  runs = list(
    title = "Runs & Compare",
    what = "Cross-run leaderboard.",
    saves = "Reads SQLite `runs` and `predictions`.",
    downstream = "—",
    methods = "Sortable per-metric, filterable per-model.",
    hints = "Combine with the Searchable Stats Explorer."
  )
)

# Escape and join markdown for the body of a drawer.
.docs_render_body <- function(entry) {
  if (is.null(entry)) return("No documentation available.")
  parts <- list(
    sprintf("**What this tab does.** %s", entry$what %||% "—"),
    sprintf("**What gets saved / exported here.** %s", entry$saves %||% "—"),
    sprintf("**What this affects downstream.** %s", entry$downstream %||% "—"),
    sprintf("**Methods & references.** %s", entry$methods %||% "—"),
    sprintf("**Setup hints.** %s", entry$hints %||% "—")
  )
  paste(unlist(parts), collapse = "\n\n")
}
