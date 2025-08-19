#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Script: generate_sample_data.R
# Purpose: Create a synthetic NCDB-like dataset for examples and testing.
# Inputs: None; generates data deterministically.
# Outputs: CSV file at data/puf_early.csv.
# Usage:  Rscript scripts/generate_sample_data.R
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
script_dir <- dirname(normalizePath(script_path))
project_root <- dirname(script_dir)
output_path <- file.path(project_root, "data", "puf_early.csv")
set.seed(0)
n <- 100
df <- data.frame(
  PUF_CASE_ID = 1:n,
  optcare = sample(0:1, n, replace = TRUE),
  SEX = sample(1:2, n, replace = TRUE),
  FACILITY_TYPE_CD = sample(c(1:4,9), n, replace = TRUE),
  DX_RX_STARTED_DAYS = sample(0:99, n, replace = TRUE),
  CROWFLY = round(runif(n, 0, 100), 1),
  CDCC_TOTAL_BEST = sample(0:3, n, replace = TRUE),
  race3 = sample(1:3, n, replace = TRUE),
  hispanic3 = sample(1:3, n, replace = TRUE),
  urbandwell = sample(1:3, n, replace = TRUE),
  age_bin = sample(1:4, n, replace = TRUE),
  SES = sample(1:3, n, replace = TRUE),
  insurancetype = sample(1:6, n, replace = TRUE),
  mintreat = sample(0:1, n, replace = TRUE),
  Gender = sample(1:2, n, replace = TRUE),
  ANALYTIC_STAGE_GROUP = sample(1:2, n, replace = TRUE),
  PUF_VITAL_STATUS = sample(0:1, n, replace = TRUE),
  FACILITY_LOCATION_CD = sample(1:9, n, replace = TRUE),
  YEAR_OF_DIAGNOSIS = sample(2010:2015, n, replace = TRUE),
  DX_LASTCONTACT_DEATH_MONTHS = sample(0:120, n, replace = TRUE)
)
df$age4 <- df$age_bin
write.csv(df, output_path, row.names = FALSE)
message("Sample data written to ", output_path)
