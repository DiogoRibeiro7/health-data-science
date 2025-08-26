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
#' @param parallel Logical; compute summaries in parallel.
#' @param progress Logical; display a progress bar.
#'
#' @return A tibble with one row per column and summary statistics.
#' @examples
#' assess_data_quality(mtcars)
assess_data_quality <- function(data, parallel = FALSE, progress = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  cols <- names(data)
  col_fn <- function(col) {
    x <- data[[col]]
    list(
      variable = col,
      class = class(x)[1],
      n_missing = sum(is.na(x)),
      pct_missing = mean(is.na(x)),
      n_unique = length(unique(x))
    )
  }
  if (parallel) {
    future::plan(future::multisession)
    on.exit(future::plan(future::sequential), add = TRUE)
    res <- future.apply::future_lapply(cols, col_fn)
  } else {
    pb <- NULL
    if (progress) {
      pb <- progress::progress_bar$new(total = length(cols))
    }
    res <- lapply(cols, function(col) {
      out <- col_fn(col)
      if (progress) pb$tick()
      out
    })
  }
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
    log_warn("Package 'mice' not installed; using simple imputation", component = "data_processing")
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
read_data_source <- function(path, source = c("csv", "excel", "sqlite"),
                             table = NULL, schema = NULL, retries = 1,
                             progress = TRUE) {
  source <- match.arg(source)
  if (!is.character(path) || length(path) != 1) {
    log_error("`path` must be a single character string", component = "data_processing")
    stop("`path` must be a single character string", call. = FALSE)
  }
  attempt <- 1
  last_err <- NULL
  while (attempt <= retries + 1) {
    log_info(sprintf("Reading %s data from %s (attempt %d)", source, path, attempt),
             component = "data_processing")
    res <- try({
      if (source == "csv") {
        read_csv_safely(path, schema = schema, retries = 0, progress = progress)
      } else if (source == "excel") {
        require_data_file(path)
        if (tolower(tools::file_ext(path)) %in% c("xls", "xlsx")) {
          readxl::read_excel(path)
        } else {
          stop("File extension does not appear to be Excel", call. = FALSE)
        }
      } else {
        require_data_file(path)
        if (is.null(table)) {
          log_error("`table` must be provided for sqlite sources", component = "data_processing")
          stop("`table` must be provided for sqlite sources", call. = FALSE)
        }
        con <- DBI::dbConnect(RSQLite::SQLite(), path)
        on.exit(DBI::dbDisconnect(con), add = TRUE)
        DBI::dbReadTable(con, table)
      }
    }, silent = TRUE)
    if (!inherits(res, "try-error")) {
      if (!is.null(schema) && source != "csv") validate_schema(res, schema)
      dup <- duplicated(res)
      if (any(dup)) {
        log_warn(sprintf("%d duplicate rows detected", sum(dup)), component = "data_processing")
      }
      miss <- colMeans(is.na(res))
      if (any(miss > 0)) {
        log_warn("Missing data detected", component = "data_processing",
                 context = list(missing = miss[miss > 0]))
      }
      log_audit(Sys.info()[["user"]], path, "read")
      return(res)
    }
    last_err <- res
    log_warn(sprintf("Read attempt %d failed: %s", attempt, res), component = "data_processing")
    attempt <- attempt + 1
    Sys.sleep(1)
  }
  log_error(sprintf("Failed to read data source after %d attempts", retries + 1),
            component = "data_processing")
  stop("Failed to read data source: ", last_err,
       "\nVerify the path and required parameters.", call. = FALSE)
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
export_data <- function(data, path, format = c("csv", "rds", "excel"),
                        retries = 1, overwrite = TRUE) {
  format <- match.arg(format)
  if (!is.data.frame(data)) {
    log_error("`data` must be a data frame", component = "data_processing")
    stop("`data` must be a data frame", call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1) {
    log_error("`path` must be a single character string", component = "data_processing")
    stop("`path` must be a single character string", call. = FALSE)
  }
  if (file.exists(path) && !overwrite) {
    stop("File exists and overwrite = FALSE: ", path, call. = FALSE)
  }
  ensure_dir(path)
  attempt <- 1
  last_err <- NULL
  while (attempt <= retries + 1) {
    log_info(sprintf("Exporting data to %s (%s) attempt %d", path, format, attempt),
             component = "data_processing")
    res <- try({
      if (format == "csv") {
        readr::write_csv(data, path)
      } else if (format == "rds") {
        saveRDS(data, path)
      } else {
        writexl::write_xlsx(data, path)
      }
    }, silent = TRUE)
    if (!inherits(res, "try-error")) {
      log_audit(Sys.info()[["user"]], path, "write")
      return(invisible(TRUE))
    }
    last_err <- res
    if (grepl("Permission denied|Resource busy", res)) {
      log_warn("Destination file locked, retrying", component = "data_processing")
    }
    attempt <- attempt + 1
    Sys.sleep(1)
  }
  log_error(sprintf("Failed to export data after %d attempts", retries + 1),
            component = "data_processing")
  stop("Failed to export data: ", last_err,
       "\nCheck that the destination is writable.", call. = FALSE)
}

#' Run a sequence of data transformations with validation
#'
#' Each function in `transformations` is applied in order. After each step, all
#' validator functions are run; if any validator returns `FALSE`, execution
#' stops with an error.
#'
#' @param data Initial data frame passed to the first transformation function.
#' @param transformations Ordered list of unary functions. Each function must
#'   accept and return a data frame; the output of one becomes the input to the
#'   next.
#' @param validators Optional list of predicate functions applied after each
#'   transformation. Validators should return `TRUE` for valid data and may
#'   throw informative errors otherwise.
#'
#' @return The transformed data frame produced by the final transformation.
#' @examples
#' steps <- list(function(d) d[complete.cases(d), ])
#' run_transform_pipeline(mtcars, steps)
run_transform_pipeline <- function(data, transformations, validators = NULL) {
  stopifnot(is.list(transformations))
  step_num <- 1
  for (step in transformations) {
    logger::log_info(sprintf("Running transformation step %d", step_num))
    data <- step(data)
    if (!is.null(validators)) {
      for (v in validators) {
        valid <- v(data)
        if (!isTRUE(valid)) {
          logger::log_error(sprintf("Validation failed at step %d", step_num))
          stop("Data validation failed during transformation pipeline", call. = FALSE)
        }
      }
    }
    step_num <- step_num + 1
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
