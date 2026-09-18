args <- commandArgs(trailingOnly = TRUE)

repos <- "https://cloud.r-project.org"

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = repos)
}

remotes::install_deps(
  dependencies = c("Depends", "Imports", "LinkingTo"),
  upgrade = "never"
)

if ("--test" %in% args || "--dev" %in% args) {
  install.packages("testthat", repos = repos)
}

if ("--dev" %in% args) {
  install.packages(c("roxygen2", "lintr"), repos = repos)
}
