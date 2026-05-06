# =====================================================================
# SQLite persistence layer
# Stores: datasets metadata, runs, predictions, hyperparameters
# =====================================================================

# Resolve DB path relative to the app root regardless of working directory
.app_root <- function() {
  # Prefer SHINYML_HOME env var; otherwise walk up from this file's dir
  env_home <- Sys.getenv("SHINYML_HOME", unset = "")
  if (nzchar(env_home) && dir.exists(env_home)) return(normalizePath(env_home, mustWork = FALSE))
  # Fall back to current working directory
  normalizePath(getwd(), mustWork = FALSE)
}

DB_DIR  <- function() file.path(.app_root(), "db")
DB_PATH <- function() file.path(DB_DIR(), "app.sqlite")

db_con <- function() {
  d <- DB_DIR()
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(RSQLite::SQLite(), DB_PATH())
}

db_init <- function() {
  con <- db_con()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS datasets (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL,
      n_rows INTEGER,
      n_cols INTEGER,
      frequency TEXT,
      target TEXT,
      task_type TEXT,
      time_col TEXT,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      meta_json TEXT
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS runs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      dataset_id INTEGER,
      model_id TEXT NOT NULL,
      task_type TEXT,
      params_json TEXT,
      metrics_json TEXT,
      horizon INTEGER,
      duration_sec REAL,
      status TEXT,
      notes TEXT,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS predictions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id INTEGER,
      idx INTEGER,
      actual REAL,
      predicted REAL,
      lower REAL,
      upper REAL,
      ts TEXT
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS analyses (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id INTEGER UNIQUE,
      provider TEXT,
      verdict TEXT,
      grade TEXT,
      analysis_md TEXT,
      suggestions_json TEXT,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    )")
}

# ---- Analyses --------------------------------------------------------
db_save_analysis <- function(run_id, provider, verdict, grade,
                              analysis_md, suggestions = list()) {
  con <- db_con(); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con,
    "INSERT OR REPLACE INTO analyses(run_id,provider,verdict,grade,analysis_md,suggestions_json)
     VALUES(?,?,?,?,?,?)",
    params = list(run_id, provider, verdict, grade, analysis_md,
                  jsonlite::toJSON(suggestions, auto_unbox = TRUE)))
}

db_get_analysis <- function(run_id) {
  con <- db_con(); on.exit(DBI::dbDisconnect(con))
  res <- DBI::dbGetQuery(con,
    "SELECT * FROM analyses WHERE run_id = ?", params = list(run_id))
  if (nrow(res) == 0) NULL else as.list(res[1, ])
}

# ---- Flattened searchable stats --------------------------------------
# Returns one row per (run, key, value) for both metrics and hyperparameters.
db_flatten_stats <- function() {
  runs <- db_get_runs()
  if (nrow(runs) == 0) return(data.frame())
  rows <- list()
  for (i in seq_len(nrow(runs))) {
    r <- runs[i, ]
    metrics <- tryCatch(jsonlite::fromJSON(r$metrics_json), error = function(e) list())
    params  <- tryCatch(jsonlite::fromJSON(r$params_json),  error = function(e) list())
    if (length(metrics) > 0) {
      for (nm in names(metrics)) {
        v <- metrics[[nm]]
        rows[[length(rows)+1]] <- data.frame(
          run_id = r$id, created_at = r$created_at,
          dataset = r$dataset %||% NA_character_,
          model = r$model_id, task = r$task_type,
          kind = "metric", key = nm, value = as.character(v),
          numeric_value = suppressWarnings(as.numeric(v)),
          stringsAsFactors = FALSE)
      }
    }
    if (length(params) > 0) {
      for (nm in names(params)) {
        v <- params[[nm]]
        rows[[length(rows)+1]] <- data.frame(
          run_id = r$id, created_at = r$created_at,
          dataset = r$dataset %||% NA_character_,
          model = r$model_id, task = r$task_type,
          kind = "param", key = nm, value = as.character(v),
          numeric_value = suppressWarnings(as.numeric(v)),
          stringsAsFactors = FALSE)
      }
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

db_save_dataset <- function(name, df, meta) {
  con <- db_con(); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con,
    "INSERT INTO datasets(name,n_rows,n_cols,frequency,target,task_type,time_col,meta_json)
     VALUES(?,?,?,?,?,?,?,?)",
    params = list(name, nrow(df), ncol(df),
                  meta$frequency %||% NA_character_,
                  meta$target %||% NA_character_,
                  meta$task_type %||% NA_character_,
                  meta$time_col %||% NA_character_,
                  jsonlite::toJSON(meta, auto_unbox = TRUE)))
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
}

db_save_run <- function(dataset_id, model_id, task_type, params, metrics,
                        horizon = NA_integer_, duration = NA_real_,
                        status = "ok", notes = "") {
  con <- db_con(); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con,
    "INSERT INTO runs(dataset_id,model_id,task_type,params_json,metrics_json,horizon,duration_sec,status,notes)
     VALUES(?,?,?,?,?,?,?,?,?)",
    params = list(dataset_id, model_id, task_type,
                  jsonlite::toJSON(params, auto_unbox = TRUE),
                  jsonlite::toJSON(metrics, auto_unbox = TRUE),
                  horizon, duration, status, notes))
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
}

db_save_predictions <- function(run_id, df) {
  if (nrow(df) == 0) return(invisible())
  df$run_id <- run_id
  if (is.null(df$lower)) df$lower <- NA_real_
  if (is.null(df$upper)) df$upper <- NA_real_
  if (is.null(df$ts))    df$ts    <- NA_character_
  if (is.null(df$actual))df$actual<- NA_real_
  df$idx <- seq_len(nrow(df))
  con <- db_con(); on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "predictions",
                    df[, c("run_id","idx","actual","predicted","lower","upper","ts")],
                    append = TRUE)
}

db_get_runs <- function() {
  con <- db_con(); on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con,
    "SELECT r.id, r.created_at, d.name AS dataset, r.model_id, r.task_type,
            r.metrics_json, r.params_json, r.horizon, r.duration_sec, r.status
     FROM runs r LEFT JOIN datasets d ON d.id = r.dataset_id
     ORDER BY r.id DESC")
}

db_get_predictions <- function(run_id) {
  con <- db_con(); on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, "SELECT * FROM predictions WHERE run_id = ? ORDER BY idx",
                  params = list(run_id))
}

db_get_datasets <- function() {
  con <- db_con(); on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, "SELECT * FROM datasets ORDER BY id DESC")
}
