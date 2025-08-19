#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Script: ncdb_LCA.R
# Purpose: Execute latent class analysis on NCDB sample data and save the
#   fitted model along with class assignments.
# Inputs: --input  Path to NCDB CSV file (default data/puf_early.csv)
#         --output Path to write RData results (default data/lca_earlypuf.RData)
# Outputs: RData file containing `lc7` model and `lca.pufdata` data frame.
# Usage:  Rscript scripts/ncdb_LCA.R --input data/puf_early.csv --output data/lca_earlypuf.RData
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
script_dir <- dirname(normalizePath(script_path))
project_root <- dirname(script_dir)
source(file.path(project_root, "R", "utils.R"))
source(file.path(project_root, "R", "lca.R"))

set.seed(123)

# Ensure optparse is available for parsing
tryCatch(ensure_packages("optparse"), error = function(e) {
  message("Package setup failed: ", e$message)
  quit(status = 1)
})

version <- get_project_version(project_root)
default_input <- file.path(project_root, "data", "puf_early.csv")
default_output <- file.path(project_root, "data", "lca_earlypuf.RData")

option_list <- list(
  optparse::make_option(c("-i", "--input"), default = default_input,
    help = "Path to input NCDB CSV data [default %default]"),
  optparse::make_option(c("-o", "--output"), default = default_output,
    help = "File path to save latent class analysis results [default %default]"),
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
  usage = "\n  Rscript scripts/ncdb_LCA.R [options]")
opts <- optparse::parse_args(parser)

if (opts$version) {
  cat(sprintf("ncdb_LCA.R version %s\n", version))
  quit(status = 0)
}

verbose <- TRUE
if (opts$quiet) verbose <- FALSE
if (opts$verbose) verbose <- TRUE

if (!opts$dry_run) {
  pkgs <- c("reshape2", "plyr", "dplyr", "poLCA",
            "ggplot2", "ggparallel", "igraph", "tidyr", "knitr", "progress")
  tryCatch(ensure_packages(pkgs), error = function(e) {
    message("Package setup failed: ", e$message)
    quit(status = 1)
  })
}

tryCatch(
  run_lca(opts$input, opts$output, dry_run = opts$dry_run,
          verbose = verbose, show_progress = verbose && !opts$quiet),
  error = function(e) {
    message("LCA workflow failed: ", e$message)
    quit(status = 1)
  }
)
