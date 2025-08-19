#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Script: ncdbearly_Regression.R
# Purpose: Fit logistic regression models using latent class assignments from
#   the LCA workflow.
# Inputs: --input  Path to RData file produced by ncdb_LCA.R
# Outputs: Printed model summaries for minimal treatment and optimal care.
# Usage:  Rscript scripts/ncdbearly_Regression.R --input data/lca_earlypuf.RData
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

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
