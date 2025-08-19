#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
script_dir <- dirname(normalizePath(script_path))
project_root <- dirname(script_dir)
source(file.path(project_root, "R", "utils.R"))
source(file.path(project_root, "R", "regression.R"))

packages <- c(
  "optparse",
  "aod", "survival", "survminer", "ranger", "ggfortify", "ggplot2"
)
tryCatch(
  ensure_packages(packages),
  error = function(e) {
    message("Package setup failed: ", e$message)
    quit(status = 1)
  }
)

default_input <- file.path(project_root, "data", "lca_earlypuf.RData")

option_list <- list(
  optparse::make_option(c("-i", "--input"), default = default_input, help = "Path to latent class results RData file")
)
opts <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

tryCatch(
  run_regression(opts$input),
  error = function(e) {
    message("Regression workflow failed: ", e$message)
    quit(status = 1)
  }
)
