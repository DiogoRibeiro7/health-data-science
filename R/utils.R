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

