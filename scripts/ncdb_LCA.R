#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
script_dir <- dirname(normalizePath(script_path))
project_root <- dirname(script_dir)
source(file.path(project_root, "R", "utils.R"))
source(file.path(project_root, "R", "lca.R"))

set.seed(123)

packages <- c(
  "optparse",
  "reshape2", "plyr", "dplyr", "poLCA",
  "ggplot2", "ggparallel", "igraph", "tidyr", "knitr"
)
tryCatch(
  ensure_packages(packages),
  error = function(e) {
    message("Package setup failed: ", e$message)
    quit(status = 1)
  }
)

default_input <- file.path(project_root, "data", "puf_early.csv")
default_output <- file.path(project_root, "data", "lca_earlypuf.RData")

option_list <- list(
  optparse::make_option(c("-i", "--input"), default = default_input, help = "Path to input NCDB data"),
  optparse::make_option(c("-o", "--output"), default = default_output, help = "File path to save latent class analysis results")
)
opts <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

tryCatch(
  run_lca(opts$input, opts$output),
  error = function(e) {
    message("LCA workflow failed: ", e$message)
    quit(status = 1)
  }
)
