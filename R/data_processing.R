# ------------------------------------------------------------------------------
# File: data_processing.R
# Purpose: Advanced data processing utilities including data quality assessment,
#   missing data imputation, outlier handling, feature engineering, ETL helpers,
#   and data lineage tracking for health data science workflows.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Assess data quality for a data frame
#'
#' Computes basic summaries for each column including number of missing values
#' and unique values. The returned data frame can be written to a report for
#' detailed quality assessment.
#'
#' @param data A data frame to assess.
#'
#' @return A tibble with one row per column and summary statistics.
#' @examples
#' assess_data_quality(mtcars)
assess_data_quality <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  cols <- names(data)
  res <- lapply(cols, function(col) {
    x <- data[[col]]
    list(
      variable = col,
      class = class(x)[1],
      n_missing = sum(is.na(x)),
      pct_missing = mean(is.na(x)),
      n_unique = length(unique(x))
    )
  })
  dplyr::bind_rows(res)
}

#' Impute missing values using multiple imputation
#'
#' Uses the `mice` package to perform multiple imputation if available. Falls
#' back to median/mode imputation when `mice` is not installed.
#'
#' @param data Data frame containing missing values.
#' @param m Number of multiple imputations when using `mice`.
#'
#' @return A data frame with imputed values.
#' @examples
#' impute_missing(mtcars)
impute_missing <- function(data, m = 5) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  if (requireNamespace("mice", quietly = TRUE)) {
    imp <- mice::mice(data, m = m, printFlag = FALSE)
    mice::complete(imp)
  } else {
    message("Package 'mice' not installed; using simple imputation")
    for (col in names(data)) {
      if (is.numeric(data[[col]])) {
        data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
      } else {
        mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
        data[[col]][is.na(data[[col]])] <- mode_val
      }
    }
    data
  }
}

#' Detect outliers using the IQR method
#'
#' Flags observations beyond the IQR * factor threshold for selected columns.
#'
#' @param data Data frame to evaluate.
#' @param cols Numeric columns to check for outliers.
#' @param factor Multiplicative factor for the IQR range.
#'
#' @return A logical matrix indicating outlier positions.
#' @examples
#' detect_outliers(mtcars, c("mpg", "hp"))
detect_outliers <- function(data, cols, factor = 1.5) {
  stopifnot(is.data.frame(data))
  stopifnot(all(cols %in% names(data)))
  out <- sapply(cols, function(col) {
    x <- data[[col]]
    q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
    q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    x < (q1 - factor * iqr) | x > (q3 + factor * iqr)
  })
  as.data.frame(out)
}

#' Winsorize outliers using the IQR method
#'
#' Replaces values outside the IQR * factor range with the respective
#' boundaries.
#'
#' @param data Data frame to modify.
#' @param cols Numeric columns to winsorize.
#' @param factor Multiplicative factor for the IQR range.
#'
#' @return Data frame with outliers capped.
#' @examples
#' handle_outliers(mtcars, c("mpg"))
handle_outliers <- function(data, cols, factor = 1.5) {
  stopifnot(is.data.frame(data))
  for (col in cols) {
    x <- data[[col]]
    q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
    q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - factor * iqr
    upper <- q3 + factor * iqr
    x[x < lower] <- lower
    x[x > upper] <- upper
    data[[col]] <- x
  }
  data
}

#' Generate interaction terms between two variable sets
#'
#' @param data Data frame containing the variables.
#' @param vars1 Character vector of first set of variables.
#' @param vars2 Character vector of second set of variables.
#'
#' @return Data frame with additional interaction columns appended.
#' @examples
#' add_interaction_terms(mtcars, "wt", "hp")
add_interaction_terms <- function(data, vars1, vars2) {
  stopifnot(is.data.frame(data))
  for (v1 in vars1) {
    for (v2 in vars2) {
      new_col <- paste(v1, v2, sep = "_x_")
      data[[new_col]] <- data[[v1]] * data[[v2]]
    }
  }
  data
}

#' Add polynomial features for numeric columns
#'
#' @param data Data frame containing the variables.
#' @param cols Columns to expand with polynomial terms.
#' @param degree Maximum polynomial degree.
#'
#' @return Data frame with polynomial features appended.
#' @examples
#' add_polynomial_features(mtcars, "hp", degree = 3)
add_polynomial_features <- function(data, cols, degree = 2) {
  stopifnot(is.data.frame(data))
  for (col in cols) {
    for (d in seq_len(degree)) {
      if (d == 1) next
      new_col <- paste(col, "pow", d, sep = "_")
      data[[new_col]] <- data[[col]]^d
    }
  }
  data
}

#' Read data from various sources
#'
#' Supports CSV, Excel, and SQLite database sources.
#'
#' @param path Path or connection string.
#' @param source One of "csv", "excel", or "sqlite".
#' @param table For SQLite sources, the table name to read.
#'
#' @return A data frame containing the parsed data.
#' @examples
#' read_data_source("data/puf_early.csv", "csv")
read_data_source <- function(path, source = c("csv", "excel", "sqlite"), table = NULL) {
  source <- match.arg(source)
  if (source == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else if (source == "excel") {
    readxl::read_excel(path)
  } else {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbReadTable(con, table)
  }
}

#' Export data to various formats
#'
#' @param data Data frame to export.
#' @param path Destination path.
#' @param format One of "csv", "rds", or "excel".
#'
#' @return Invisible `TRUE` when successful.
#' @examples
#' export_data(mtcars, "mtcars.csv", "csv")
export_data <- function(data, path, format = c("csv", "rds", "excel")) {
  format <- match.arg(format)
  ensure_dir(path)
  if (format == "csv") {
    readr::write_csv(data, path)
  } else if (format == "rds") {
    saveRDS(data, path)
  } else {
    writexl::write_xlsx(data, path)
  }
  invisible(TRUE)
}

#' Run a sequence of data transformations with validation
#'
#' Each function in `transformations` is applied in order. After each step, all
#' validator functions are run; if any validator returns `FALSE`, execution
#' stops with an error.
#'
#' @param data Initial data frame.
#' @param transformations List of functions transforming the data.
#' @param validators List of functions returning `TRUE` when data is valid.
#'
#' @return Transformed data frame.
#' @examples
#' steps <- list(function(d) d[complete.cases(d), ])
#' run_transform_pipeline(mtcars, steps)
run_transform_pipeline <- function(data, transformations, validators = NULL) {
  stopifnot(is.list(transformations))
  for (step in transformations) {
    data <- step(data)
    if (!is.null(validators)) {
      for (v in validators) {
        valid <- v(data)
        if (!isTRUE(valid)) {
          stop("Data validation failed during transformation pipeline", call. = FALSE)
        }
      }
    }
  }
  data
}

#' Record data version and lineage information
#'
#' Writes a log entry containing the file path, timestamp, and SHA1 hash of the
#' file contents. This log can be used to track data lineage across analyses.
#'
#' @param file Path to the data file being recorded.
#' @param log Path to the CSV log file.
#'
#' @return Invisible `TRUE` when the log is updated.
#' @examples
#' record_data_version("data/puf_early.csv")
record_data_version <- function(file, log = "data/version_log.csv") {
  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }
  ensure_dir(log)
  hash <- digest::digest(file = file, algo = "sha1")
  entry <- data.frame(
    file = file,
    timestamp = Sys.time(),
    sha1 = hash,
    stringsAsFactors = FALSE
  )
  if (file.exists(log)) {
    utils::write.table(entry, log, sep = ",", col.names = FALSE,
                       row.names = FALSE, append = TRUE)
  } else {
    utils::write.table(entry, log, sep = ",", col.names = TRUE,
                       row.names = FALSE)
  }
  invisible(TRUE)
}

