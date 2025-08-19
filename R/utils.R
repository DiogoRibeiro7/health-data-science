ensure_packages <- function(packages) {
  repos <- "https://cloud.r-project.org"
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = repos, dependencies = TRUE)
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop("Failed to install package: ", pkg, call. = FALSE)
      }
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

ensure_dir <- function(path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
}

require_data_file <- function(path) {
  if (!file.exists(path)) {
    stop("Data file not found: ", path, call. = FALSE)
  }
}
