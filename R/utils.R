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
    stop("`packages` must be a character vector", call. = FALSE)
  }
  repos <- "https://cloud.r-project.org"
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      tryCatch(
        install.packages(pkg, repos = repos, dependencies = TRUE),
        error = function(e) {
          stop("Failed to install package '", pkg, "': ", e$message,
               "\nPlease check your internet connection or install the package manually.",
               call. = FALSE)
        }
      )
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop("Package '", pkg, "' could not be loaded after installation.", call. = FALSE)
      }
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
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
#' Wraps `read.csv()` and checks for file existence before attempting to
#' read. Failing to parse the file yields an actionable error message.
#'
#' @param path Path to the CSV file.
#'
#' @return A data frame containing the parsed contents of the file.
#'
#' @examples
#' tmp <- tempfile(fileext = ".csv"); write.csv(mtcars, tmp)
#' read_csv_safely(tmp)
#' @export
read_csv_safely <- function(path) {
  if (!is.character(path) || length(path) != 1) {
    stop("`path` must be a single character string", call. = FALSE)
  }
  require_data_file(path)
  tryCatch(
    read.csv(path),
    error = function(e) {
      stop("Failed to read input data '", path, "': ", e$message,
           "\nEnsure the file is a valid CSV and accessible.",
           call. = FALSE)
    }
  )
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
#' Wraps an expression to record elapsed time and change in memory usage,
#' optionally updating a progress bar.
#'
#' @param step Descriptive name of the step being timed.
#' @param expr Expression to evaluate.
#' @param pb Optional `progress` bar to tick after completion.
#' @param verbose Logical flag to emit timing messages.
#'
#' @return The result of `expr`.
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

#' Cache the result of an expression
#'
#' Evaluates an expression and stores the result as an RDS file. Subsequent
#' calls with the same cache path return the cached value instead of
#' re-evaluating the expression.
#'
#' @param path File path to the cache RDS file.
#' @param expr Expression to evaluate when cache is absent.
#'
#' @return The cached or newly computed result.
#'
#' @examples
#' tmp <- tempfile(fileext = ".rds")
#' cache_result(tmp, { 1 + 1 })
cache_result <- function(path, expr) {
  if (file.exists(path)) {
    return(readRDS(path))
  }
  ensure_dir(path)
  result <- eval.parent(substitute(expr))
  saveRDS(result, path)
  result
}

