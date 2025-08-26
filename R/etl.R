# ------------------------------------------------------------------------------
# File: etl.R
# Purpose: Streaming ETL pipelines, change data capture, and data lineage
#   utilities for healthcare datasets.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Run a streaming ETL job between two databases
#'
#' Extracts data from `src_conn` using `query` and writes into `dest_conn`.
#' Data are processed in chunks to limit memory usage. An optional transformation
#' function can be applied to each chunk before writing. Data lineage is recorded
#' for compliance purposes.
#'
#' @param src_conn Source database connection or pool.
#' @param dest_conn Destination connection or pool.
#' @param query SQL query defining the extract.
#' @param dest_table Name of destination table to append.
#' @param transform Optional function applied to each chunk.
#' @param chunk_size Number of rows per chunk.
#' @param lineage_file Path to lineage log file.
#'
#' @return Invisibly returns `TRUE` on success.
#' @export
#' @examples
#' src <- db_connect(list(dbms = "sqlite", dbname = tempfile()))
#' dest <- db_connect(list(dbms = "sqlite", dbname = tempfile()))
#' DBI::dbWriteTable(src, "x", data.frame(a = 1:3))
#' run_etl(src, dest, "select * from x", "y")
#' db_disconnect(src); db_disconnect(dest)
run_etl <- function(src_conn, dest_conn, query, dest_table,
                    transform = NULL, chunk_size = 1000,
                    lineage_file = "data_lineage.csv", retries = 3) {
  attempt <- 1
  repeat {
    try({
      DBI::dbWithTransaction(dest_conn, {
        cb <- function(chunk) {
          if (!is.null(transform)) chunk <- transform(chunk)
          DBI::dbWriteTable(dest_conn, dest_table, chunk, append = TRUE, row.names = FALSE)
        }
        db_query(src_conn, query, chunk_size = chunk_size, callback = cb)
      })
      record_data_lineage("database", dest_table, query, lineage_file)
      break
    }, silent = TRUE)
    if (attempt >= retries) stop("ETL failed after retries")
    attempt <- attempt + 1
    Sys.sleep(1)
  }
  invisible(TRUE)
}

#' Record data lineage information
#'
#' Appends an entry describing the data source, destination, and transformation to
#' a CSV log. This facilitates audit trails for HIPAA compliance.
#'
#' @param source Description of data source.
#' @param destination Description of data destination.
#' @param transformation Description of transformation or query used.
#' @param lineage_file Path to the lineage log file.
#'
#' @export
#' @examples
#' record_data_lineage("raw.csv", "table_y", "standardize columns")
record_data_lineage <- function(source, destination, transformation,
                               lineage_file = "data_lineage.csv") {
  entry <- data.frame(
    timestamp = as.character(Sys.time()),
    source = source,
    destination = destination,
    transformation = transformation,
    stringsAsFactors = FALSE
  )
  readr::write_csv(entry, lineage_file, append = file.exists(lineage_file))
}
