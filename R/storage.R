# ------------------------------------------------------------------------------
# File: storage.R
# Purpose: Scalable storage helpers for Parquet files and cloud integration.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Write a data frame to a Parquet file
#'
#' Supports optional partitioning and compression to optimise analytics
#' workloads.
#'
#' @param data A data frame to write.
#' @param path Output directory or file path.
#' @param partition Character vector of columns used for partitioning.
#' @param compression Compression codec (e.g. "snappy", "gzip").
#' @export
#' @examples
#' tmp <- tempfile(fileext = ".parquet")
#' write_parquet_data(mtcars, tmp)
write_parquet_data <- function(data, path, partition = NULL,
                               compression = "snappy") {
  stopifnot(is.data.frame(data))
  if (length(partition)) {
    arrow::write_dataset(data, path, partitioning = partition,
                         format = "parquet", compression = compression)
  } else {
    arrow::write_parquet(data, path, compression = compression)
  }
}

#' Read a Parquet file
#'
#' @param path Path to Parquet file or directory.
#' @return A tibble.
#' @export
#' @examples
#' tmp <- tempfile(fileext = ".parquet")
#' write_parquet_data(mtcars, tmp)
#' df <- read_parquet_data(tmp)
read_parquet_data <- function(path) {
  arrow::read_parquet(path)
}

