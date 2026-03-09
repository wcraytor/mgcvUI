#' Settings Persistence (SQLite, keyed by input file name)
#'
#' Stores and retrieves per-file variable configuration so that
#' settings are remembered when the same data file is re-opened.
#'
#' @name settings_db
#' @keywords internal
NULL


#' Path to the settings database
#' @return Character path.
#' @noRd
settings_db_path_ <- function() {
  dir <- tools::R_user_dir("mgcvUI", "data")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, "settings.sqlite")
}


#' Open (or create) the settings database
#' @return A DBI connection, or NULL if RSQLite not available.
#' @noRd
settings_db_connect_ <- function() {
  if (!requireNamespace("RSQLite", quietly = TRUE)) return(NULL)
  if (!requireNamespace("DBI", quietly = TRUE)) return(NULL)

  con <- DBI::dbConnect(RSQLite::SQLite(), settings_db_path_())
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS settings_v2 (
      filename    TEXT PRIMARY KEY,
      response    TEXT,
      variables   TEXT,
      family      TEXT DEFAULT 'gaussian',
      method      TEXT DEFAULT 'REML',
      sel         INTEGER DEFAULT 0,
      gamma       REAL DEFAULT 1.0,
      updated_at  TEXT DEFAULT (datetime('now'))
    )
  ")

  # Add columns introduced after initial schema
  existing <- DBI::dbListFields(con, "settings_v2")
  if (!"output_folder" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN output_folder TEXT DEFAULT ''")
  }
  if (!"effective_date" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN effective_date TEXT DEFAULT ''")
  }
  if (!"purpose" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN purpose TEXT DEFAULT 'general'")
  }
  if (!"weights_col" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN weights_col TEXT DEFAULT ''")
  }
  con
}


#' Save Settings for a File
#'
#' @param filename Character — the original file name (basename).
#' @param config List with response, variables (named list of per-var
#'   settings), family, method, select, gamma.
#' @noRd
settings_db_write_ <- function(filename, config) {
  con <- settings_db_connect_()
  if (is.null(con)) return(invisible(NULL))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  vars_json <- jsonlite_encode_(config$variables)

  DBI::dbExecute(con, "
    INSERT INTO settings_v2 (filename, response, variables,
                             family, method, sel, gamma,
                             output_folder, effective_date, purpose,
                             weights_col, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))
    ON CONFLICT(filename) DO UPDATE SET
      response       = excluded.response,
      variables      = excluded.variables,
      family         = excluded.family,
      method         = excluded.method,
      sel            = excluded.sel,
      gamma          = excluded.gamma,
      output_folder  = excluded.output_folder,
      effective_date = excluded.effective_date,
      purpose        = excluded.purpose,
      weights_col    = excluded.weights_col,
      updated_at     = excluded.updated_at
  ", params = list(filename,
                   config$response %||% "",
                   vars_json,
                   config$family %||% "gaussian",
                   config$method %||% "REML",
                   as.integer(config$select %||% FALSE),
                   config$gamma %||% 1,
                   config$output_folder %||% "",
                   config$effective_date %||% "",
                   config$purpose %||% "general",
                   config$weights_col %||% ""))

  settings_db_evict_(con, max_files = 100L)
  invisible(NULL)
}


#' Load Settings for a File
#'
#' @param filename Character — the original file name (basename).
#' @return A list with the saved config, or NULL if not found.
#' @noRd
settings_db_read_ <- function(filename) {
  con <- settings_db_connect_()
  if (is.null(con)) return(NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row <- DBI::dbGetQuery(con,
    "SELECT * FROM settings_v2 WHERE filename = ? LIMIT 1",
    params = list(filename)
  )

  if (nrow(row) == 0L) return(NULL)

  variables <- jsonlite_decode_(row$variables)

  list(
    response       = if (nzchar(row$response)) row$response else NULL,
    variables      = variables,
    family         = row$family,
    method         = row$method,
    select         = as.logical(row$sel),
    gamma          = row$gamma,
    output_folder  = if ("output_folder" %in% names(row) &&
                          nzchar(row$output_folder)) row$output_folder else NULL,
    effective_date = if ("effective_date" %in% names(row) &&
                          nzchar(row$effective_date)) row$effective_date else NULL,
    purpose        = if ("purpose" %in% names(row) &&
                          nzchar(row$purpose)) row$purpose else NULL,
    weights_col    = if ("weights_col" %in% names(row) &&
                          nzchar(row$weights_col)) row$weights_col else NULL
  )
}


#' Evict old entries beyond max_files
#' @param con A DBI connection.
#' @param max_files Integer.
#' @noRd
settings_db_evict_ <- function(con, max_files = 100L) {
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM settings_v2")$n
  if (n > max_files) {
    DBI::dbExecute(con, "
      DELETE FROM settings_v2 WHERE filename NOT IN (
        SELECT filename FROM settings_v2 ORDER BY updated_at DESC LIMIT ?
      )
    ", params = list(max_files))
  }
}


#' Minimal JSON encode
#' @param x An R object.
#' @return Character JSON string.
#' @noRd
jsonlite_encode_ <- function(x) {
  if (is.null(x) || length(x) == 0L) return("{}")
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    return(jsonlite::toJSON(x, auto_unbox = TRUE))
  }
  deparse(x, control = "all")
}


#' Minimal JSON decode
#' @param json Character JSON string.
#' @return An R list.
#' @noRd
jsonlite_decode_ <- function(json) {
  if (is.null(json) || !nzchar(json) || json %in% c("{}", "[]")) {
    return(list())
  }
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    tryCatch(
      jsonlite::fromJSON(json, simplifyVector = FALSE),
      error = function(e) list()
    )
  } else {
    tryCatch(eval(parse(text = json)), error = function(e) list())
  }
}
