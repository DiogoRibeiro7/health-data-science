# ------------------------------------------------------------------------------
# File: utils.R
# Purpose: Shared helper functions for package installation, directory
#   management, data validation, and safe file reading used across analysis
#   scripts.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Ensure required R packages are installed and loaded
#'
#' Attempts to install any packages that are not already available and
#' loads them quietly for use. Installation failures yield descriptive
#' error messages so users can take corrective action.
#'
#' @param packages Character vector of package names to install and load.
#'
#' @return Invisible `TRUE` when all packages have been successfully loaded.
#'
#' @examples
#' ensure_packages(c("stats", "utils"))
ensure_packages <- function(packages) {
  if (!is.character(packages)) {
    logger::log_error("`packages` must be a character vector")
    stop("`packages` must be a character vector", call. = FALSE)
  }
  repos <- "https://cloud.r-project.org"
  for (pkg in packages) {
    logger::log_debug(sprintf("Checking package '%s'", pkg))
    if (!requireNamespace(pkg, quietly = TRUE)) {
      attempt <- 1
      success <- FALSE
      while (attempt <= 2 && !success) {
        tryCatch({
          install.packages(pkg, repos = repos, dependencies = TRUE)
          success <- TRUE
        }, error = function(e) {
          logger::log_warn(sprintf("Attempt %d to install '%s' failed: %s", attempt, pkg, e$message))
          attempt <<- attempt + 1
          if (attempt > 2) {
            logger::log_error(sprintf("Failed to install package '%s'", pkg))
            stop("Failed to install package '", pkg, "': ", e$message,
                 "\nPlease check your internet connection or install the package manually.",
                 call. = FALSE)
          }
        })
      }
      if (!requireNamespace(pkg, quietly = TRUE)) {
        logger::log_error(sprintf("Package '%s' could not be loaded after installation", pkg))
        stop("Package '", pkg, "' could not be loaded after installation.", call. = FALSE)
      }
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    logger::log_info(sprintf("Package '%s' loaded", pkg))
  }
  invisible(TRUE)
}

#' Check that the running R version meets a minimum requirement
#'
#' Provides a clear message when the installed R version is older than the
#' required version.
#'
#' @param required Minimum acceptable version as a character string.
#'
#' @return Invisible `TRUE` when the requirement is satisfied.
#'
#' @examples
#' check_r_version("4.0.0")
check_r_version <- function(required = "4.0.0") {
  current <- getRversion()
  if (current < required) {
    stop(sprintf(
      "R %s or higher is required; current version is %s. Please update R and re-run the script.",
      required, current
    ), call. = FALSE)
  }
  invisible(TRUE)
}

#' Ensure directory exists for a target file
#'
#' Creates the parent directory of the supplied path if it does not
#' already exist. No message is thrown when the directory is created.
#'
#' @param path File path whose parent directory should be present.
#'
#' @return Invisible `TRUE` once the directory exists.
#'
#' @examples
#' ensure_dir("output/results.txt")
ensure_dir <- function(path) {
  if (!is.character(path) || length(path) != 1) {
    stop("`path` must be a single character string", call. = FALSE)
  }
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(TRUE)
}

#' Validate the presence of a data file
#'
#' Stops execution with an informative message if the specified data
#' file cannot be found.
#'
#' @param path Path to the required data file.
#'
#' @return Invisible `TRUE` when the file exists.
#'
#' @examples
#' require_data_file("data/puf_early.csv")
require_data_file <- function(path) {
  if (!is.character(path) || length(path) != 1) {
    stop("`path` must be a single character string", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Data file not found: ", path,
         "\nPlease generate the sample data or supply the correct path.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Safely read a CSV file with informative errors
#'
#' Wraps [base::read.csv()] and checks for file existence before attempting to
#' parse the file. A transient failure triggers a configurable number of retry
#' attempts with a one second back-off between tries. This helper is intended
#' for reading user-provided inputs where clearer feedback is desirable.
#'
#' @param path Character string path to the CSV file on disk. The file must
#'   exist and be readable.
#' @param retries Integer number of additional attempts to make when parsing the
#'   file fails. Useful for network file systems where the first attempt may
#'   intermittently fail. Defaults to `1` (two total attempts).
#'
#' @return A data frame containing the parsed contents of `path`.
#'
#' @examples
#' tmp <- tempfile(fileext = ".csv"); write.csv(mtcars, tmp)
#' read_csv_safely(tmp)
#' @export
read_csv_safely <- function(path, retries = 1) {
  if (!is.character(path) || length(path) != 1) {
    logger::log_error("`path` must be a single character string")
    stop("`path` must be a single character string", call. = FALSE)
  }
  require_data_file(path)
  attempt <- 1
  while (attempt <= retries + 1) {
    logger::log_info(sprintf("Reading CSV attempt %d: %s", attempt, path))
    res <- try(read.csv(path), silent = TRUE)
    if (!inherits(res, "try-error")) {
      return(res)
    }
    logger::log_warn(sprintf("Failed to read '%s': %s", path, res))
    attempt <- attempt + 1
    Sys.sleep(1)
  }
  logger::log_error(sprintf("Unable to read CSV after %d attempts", retries + 1))
  stop("Failed to read input data '", path, "'. Ensure the file is a valid CSV and accessible.",
       call. = FALSE)
}

#' Read a large CSV file in chunks
#'
#' Streams a CSV file using [`readr::read_csv_chunked()`] so that only a
#' portion of the file is held in memory at any time. A callback function is
#' executed on each chunk allowing incremental processing of very large data
#' sets.
#'
#' @param path Path to the CSV file.
#' @param chunk_size Number of rows to read per chunk.
#' @param callback Function to invoke with each chunk; must accept a data frame
#'   argument.
#' @param progress Logical; display a progress bar.
#'
#' @return Invisible `NULL`.
#'
#' @examples
#' tmp <- tempfile(fileext = ".csv"); write.csv(mtcars, tmp, row.names = FALSE)
#' read_csv_chunked(tmp, 10, function(x) nrow(x))
#' @export
read_csv_chunked <- function(path, chunk_size = 10000, callback, progress = TRUE) {
  if (!is.function(callback)) {
    stop("`callback` must be a function", call. = FALSE)
  }
  require_data_file(path)
  pb <- NULL
  if (progress) {
    pb <- progress::progress_bar$new(total = NA,
      format = "[:bar] :current chunks")
  }
  reader_cb <- readr::SideEffectChunkCallback$new(function(x, pos) {
    callback(x)
    if (progress) pb$tick()
  })
  readr::read_csv_chunked(path, callback = reader_cb, chunk_size = chunk_size,
                          show_col_types = FALSE, progress = progress)
  invisible(NULL)
}

#' Retrieve project version from DESCRIPTION
#'
#' Reads the package version declared in the repository's DESCRIPTION file
#' so scripts can expose a `--version` flag without duplicating metadata.
#'
#' @param root Path to the project root containing DESCRIPTION.
#'
#' @return Character string of the project version.
#'
#' @examples
#' get_project_version()
get_project_version <- function(root = getwd()) {
  desc <- file.path(root, "DESCRIPTION")
  if (!file.exists(desc)) {
    stop("DESCRIPTION file not found at ", desc, call. = FALSE)
  }
  read.dcf(desc, fields = "Version")[1, 1]
}

#' Monitor execution time and memory for a step
#'
#' Wraps an arbitrary expression to record elapsed wall time and change in
#' memory usage. When a [progress::progress_bar] is supplied, the bar is ticked
#' after the step completes. This function underpins progress reporting in
#' long-running workflows.
#'
#' @param step Character label describing the step being timed.
#' @param expr Expression to evaluate. The result of `expr` is returned
#'   unchanged.
#' @param pb Optional [`progress::progress_bar`] object to tick upon completion.
#' @param verbose Logical flag; if `TRUE`, a message summarising runtime and
#'   memory usage is emitted.
#'
#' @return The evaluated result of `expr`.
#'
#' @examples
#' monitor_step("sleep", Sys.sleep(0.1), verbose = FALSE)
monitor_step <- function(step, expr, pb = NULL, verbose = TRUE) {
  start <- proc.time()[["elapsed"]]
  mem_before <- sum(gc()[, 2])
  result <- eval.parent(substitute(expr))
  elapsed <- proc.time()[["elapsed"]] - start
  mem_after <- sum(gc()[, 2])
  if (verbose) {
    message(sprintf("%s completed in %.2f sec (%.1f MB used)", step, elapsed, mem_after - mem_before))
  }
  if (!is.null(pb)) pb$tick(tokens = list(what = step))
  result
}

#' Report current R memory usage
#'
#' @return Numeric value of memory used in megabytes.
#' @examples memory_usage()
#' @export
memory_usage <- function() {
  sum(gc()[, 2])
}

#' Benchmark an expression by repeated execution
#'
#' @param expr Expression to benchmark.
#' @param times Number of repetitions.
#'
#' @return Numeric vector of elapsed times in seconds.
#'
#' @examples
#' benchmark_expr({x <- rnorm(1e3)}, times = 3)
benchmark_expr <- function(expr, times = 5) {
  replicate(times, system.time(eval.parent(substitute(expr)))[["elapsed"]])
}

#' Profile code execution using profvis
#'
#' Generates an interactive profile for the supplied expression using the
#' `profvis` package. The profile object is returned invisibly for further
#' inspection.
#'
#' @param expr Expression to profile.
#'
#' @return An object of class `profvis`.
#'
#' @examples
#' \dontrun{ profile_expr({ Sys.sleep(0.1) }) }
profile_expr <- function(expr) {
  ensure_packages("profvis")
  profvis::profvis(eval.parent(substitute(expr)))
}

#' Apply a function to data in chunks
#'
#' Splits a data frame into manageable chunks and applies the supplied
#' function to each chunk sequentially.
#'
#' @param df Data frame to process.
#' @param chunk_size Number of rows per chunk.
#' @param fn Function to apply to each chunk.
#'
#' @return Invisible `NULL`.
#'
#' @examples
#' chunk_apply(data.frame(x = 1:5), 2, function(x) print(nrow(x)))
chunk_apply <- function(df, chunk_size, fn) {
  n <- nrow(df)
  idx <- seq(1, n, by = chunk_size)
  for (i in idx) {
    chunk <- df[i:min(i + chunk_size - 1, n), , drop = FALSE]
    fn(chunk)
  }
  invisible(NULL)
}

#' Convert a data frame to a sparse matrix
#'
#' Uses the [Matrix] package to create a compressed sparse column matrix from
#' a dense data frame, which is more memory efficient for data with many zeros.
#'
#' @param df Data frame or matrix.
#'
#' @return An object of class `dgCMatrix`.
#'
#' @examples
#' to_sparse_matrix(data.frame(x = c(0, 1, 0)))
#' @export
to_sparse_matrix <- function(df) {
  if (!is.data.frame(df) && !is.matrix(df)) {
    stop("`df` must be a data frame or matrix", call. = FALSE)
  }
  Matrix::Matrix(as.matrix(df), sparse = TRUE)
}

#' Cache the result of an expression
#'
#' Evaluates an expression and stores the result as an RDS file. Subsequent
#' calls with the same cache path return the cached value instead of
#' re-evaluating the expression.
#'
#' @param path File path to the cache RDS file.
#' @param expr Expression to evaluate when cache is absent.
#' @param depends Optional vector of file paths; when any dependency has a
#'   modification time newer than the cached result, the cache is invalidated.
#' @param max_size_mb Maximum total size of the cache directory in megabytes.
#'
#' @return The cached or newly computed result.
#'
#' @examples
#' tmp <- tempfile(fileext = ".rds")
#' cache_result(tmp, { 1 + 1 })
cache_result <- function(path, expr, depends = NULL, max_size_mb = Inf) {
  if (file.exists(path)) {
    obj <- readRDS(path)
    meta <- attr(obj, "cache_meta")
    if (!is.null(depends) && !is.null(meta$mtime)) {
      current <- file.info(depends)$mtime
      if (length(current) != length(meta$mtime) || any(current != meta$mtime)) {
        file.remove(path)
      } else {
        return(obj)
      }
    } else {
      return(obj)
    }
  }
  ensure_dir(path)
  result <- eval.parent(substitute(expr))
  meta <- list(mtime = if (!is.null(depends)) file.info(depends)$mtime else NULL)
  attr(result, "cache_meta") <- meta
  saveRDS(result, path)
  cache_dir <- dirname(path)
  if (is.finite(max_size_mb)) {
    files <- list.files(cache_dir, full.names = TRUE)
    sizes <- file.info(files)$size
    while (sum(sizes) / (1024^2) > max_size_mb && length(files) > 0) {
      oldest <- files[which.min(file.info(files)$mtime)]
      file.remove(oldest)
      files <- list.files(cache_dir, full.names = TRUE)
      sizes <- file.info(files)$size
    }
  }
  result
}

