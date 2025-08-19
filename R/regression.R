# ------------------------------------------------------------------------------
# File: regression.R
# Purpose: Logistic regression helpers for analyzing latent class profiles.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

# Helper and modeling functions for regression analyses

load_lca_results <- function(data_path) {
  if (!is.character(data_path) || length(data_path) != 1) {
    stop("`data_path` must be a single character string", call. = FALSE)
  }
  message("Loading LCA results from: ", data_path)
  require_data_file(data_path)
  tryCatch(
    {
      load(data_path)
    },
    error = function(e) {
      stop("Failed to load data from '", data_path, "': ", e$message,
           "\nEnsure the file was created by the LCA workflow.",
           call. = FALSE)
    }
  )
  if (!exists("lca.pufdata")) {
    stop("Object 'lca.pufdata' not found in ", data_path,
         "\nVerify the input file is a valid LCA results file.",
         call. = FALSE)
  }
  lca.pufdata
}

fit_min_treatment_model <- function(df) {
  # Logistic regression estimating odds of receiving minimal treatment
  glm(
    mintreat ~ LCAprofile + ANALYTIC_STAGE_GROUP + facility +
      CDCC_TOTAL_BEST + FACILITY_LOCATION_CD + YEAR_OF_DIAGNOSIS,
    data = df, family = "binomial"
  )
}

fit_optimal_care_model <- function(df) {
  # Logistic regression estimating probability of optimal care
  glm(
    optcare ~ LCAprofile + ANALYTIC_STAGE_GROUP + facility +
      CDCC_TOTAL_BEST + FACILITY_LOCATION_CD + YEAR_OF_DIAGNOSIS,
    data = df, family = "binomial"
  )
}

run_regression <- function(data_path) {
  df <- load_lca_results(data_path)
  list(
    mintreat = fit_min_treatment_model(df),
    optcare = fit_optimal_care_model(df)
  )
}
