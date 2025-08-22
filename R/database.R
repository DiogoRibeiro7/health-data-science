# ------------------------------------------------------------------------------
# File: database.R
# Purpose: Database connectivity utilities with connection pooling and streaming
#   query support for large healthcare datasets.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Establish a database connection using a connection pool
#'
#' Creates a pooled connection to major database backends. Credentials are read
#' from the supplied configuration list or from environment variables when
#' missing to support secure storage.
#'
#' @param config List containing `dbms`, `host`, `dbname`, `user`, `password`,
#'   and other driver-specific fields.
#' @param pool_size Number of connections to maintain in the pool.
#'
#' @return A `pool::Pool` object.
#' @export
#' @examples
#' cfg <- list(dbms = "sqlite", dbname = tempfile(fileext = ".db"))
#' pool <- db_connect(cfg)
#' db_disconnect(pool)
#' 
#' @seealso `db_disconnect`, `db_query`
db_connect <- function(config, pool_size = 5) {
  stopifnot(is.list(config))
  dbms <- tolower(if (is.null(config$dbms)) "sqlite" else config$dbms)
  drv <- switch(
    dbms,
    postgres = RPostgres::Postgres(),
    mysql    = RMariaDB::MariaDB(),
    sqlserver = odbc::odbc(),
    oracle   = ROracle::Oracle(),
    sqlite   = RSQLite::SQLite(),
    stop("Unsupported dbms: ", dbms)
  )
  user <- if (is.null(config$user)) Sys.getenv("DB_USER") else config$user
  password <- if (is.null(config$password)) Sys.getenv("DB_PASSWORD") else config$password
  log_info("Connecting to {dbms} database {config$dbname}")
  pool::dbPool(
    drv,
    dbname   = config$dbname,
    host     = config$host,
    username = user,
    password = password,
    port     = config$port,
    minSize  = 1,
    maxSize  = pool_size
  )
}

#' Gracefully close a database connection or pool
#'
#' @param conn A DBI connection or `pool::Pool` object.
#' @export
db_disconnect <- function(conn) {
  if (inherits(conn, "Pool")) {
    pool::poolClose(conn)
  } else if (inherits(conn, "DBIConnection")) {
    DBI::dbDisconnect(conn)
  }
}

#' Execute a SQL query with optional streaming
#'
#' Allows incremental retrieval of large result sets by processing `chunk_size`
#' rows at a time. When `callback` is supplied, each chunk is passed to the
#' function instead of being accumulated in memory.
#'
#' @param conn Database connection or pool.
#' @param sql SQL query string.
#' @param chunk_size Number of rows per fetch. If `NULL`, all rows are returned.
#' @param callback Optional function applied to each chunk.
#'
#' @return A data frame when `callback` is `NULL`; otherwise, `NULL`.
#' @export
#' @examples
#' pool <- db_connect(list(dbms = "sqlite", dbname = tempfile()))
#' DBI::dbWriteTable(pool, "x", data.frame(a = 1:3))
#' db_query(pool, "select * from x")
#' db_disconnect(pool)
db_query <- function(conn, sql, chunk_size = NULL, callback = NULL) {
  res <- DBI::dbSendQuery(conn, sql)
  on.exit(DBI::dbClearResult(res), add = TRUE)
  if (is.null(chunk_size)) {
    DBI::dbFetch(res)
  } else {
    out <- list()
    repeat {
      chunk <- DBI::dbFetch(res, n = chunk_size)
      if (nrow(chunk) == 0) break
      if (is.null(callback)) {
        out[[length(out) + 1]] <- chunk
      } else {
        callback(chunk)
      }
    }
    if (is.null(callback)) dplyr::bind_rows(out) else invisible(NULL)
  }
}

