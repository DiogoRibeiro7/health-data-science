args <- commandArgs(trailingOnly = TRUE)

repos <- "https://cloud.r-project.org"

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = repos)
}

dependency_types <- c("Depends", "Imports", "LinkingTo")
if ("--all" %in% args) {
  dependency_types <- c(dependency_types, "Suggests")
}

remotes::install_deps(
  dependencies = dependency_types,
  upgrade = "never"
)

if ("--test" %in% args || "--dev" %in% args) {
  install.packages("testthat", repos = repos)
}

if ("--dev" %in% args) {
  install.packages(c("roxygen2", "lintr"), repos = repos)
}
