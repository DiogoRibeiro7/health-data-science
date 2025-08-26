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
source(file.path(project_root, "R", "logging.R"))
init_logging()

# Verify R version for compatibility
tryCatch(check_r_version("4.0.0"), error = function(e) {
  log_error(e$message, component = "script")
  quit(status = 1)
})

# Ensure optparse is available for parsing
tryCatch(ensure_packages("optparse"), error = function(e) {
  log_error(paste("Package setup failed:", e$message), component = "script")
  quit(status = 1)
})

version <- get_project_version(project_root)
default_input <- file.path(project_root, "data", "lca_earlypuf.RData")

option_list <- list(
  optparse::make_option(c("-i", "--input"), default = default_input,
    help = "Path to latent class results RData file [default %default]"),
  optparse::make_option("--config", default = "default",
    help = "Configuration environment (default, development, production, testing) [default %default]"),
  optparse::make_option("--version", action = "store_true", default = FALSE,
    help = "Print script version and exit"),
  optparse::make_option("--dry-run", action = "store_true", default = FALSE,
    help = "Validate arguments and required files, then exit"),
  optparse::make_option(c("-q", "--quiet"), action = "store_true", default = FALSE,
    help = "Suppress progress messages"),
  optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE,
    help = "Print additional status messages")
)
parser <- optparse::OptionParser(option_list = option_list,
  usage = "\n  Rscript scripts/ncdbearly_Regression.R [options]")
opts <- optparse::parse_args(parser)

if (opts$version) {
  log_info(sprintf("ncdbearly_Regression.R version %s", version), component = "script")
  quit(status = 0)
}

verbose <- TRUE
if (opts$quiet) verbose <- FALSE
if (opts$verbose) verbose <- TRUE

cfg <- load_config(opts$config, project_root)

if (!opts$dry_run) {
  pkgs <- c("aod", "survival", "survminer", "ranger", "ggfortify",
            "ggplot2", "progress")
  tryCatch(ensure_packages(pkgs), error = function(e) {
    log_error(paste("Package setup failed:", e$message), component = "script")
    quit(status = 1)
  })
}

tryCatch(
  run_regression(opts$input, cfg, dry_run = opts$dry_run, verbose = verbose,
                 show_progress = verbose && !opts$quiet),
  error = function(e) {
    log_error(paste("Regression workflow failed:", e$message), component = "script")
    quit(status = 1)
  }
)
