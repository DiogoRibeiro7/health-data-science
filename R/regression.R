# ------------------------------------------------------------------------------
# File: regression.R
# Purpose: Logistic regression helpers for analyzing latent class profiles.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

# Helper and modeling functions for regression analyses

load_lca_results <- function(data_path, verbose = TRUE) {
  if (!is.character(data_path) || length(data_path) != 1) {
    stop("`data_path` must be a single character string", call. = FALSE)
  }
  require_data_file(data_path)
  if (verbose) message("Loading LCA results from: ", data_path)
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

fit_min_treatment_model <- function(df, family = "binomial") {
  # Logistic regression estimating odds of receiving minimal treatment
  glm(
    mintreat ~ LCAprofile + ANALYTIC_STAGE_GROUP + facility +
      CDCC_TOTAL_BEST + FACILITY_LOCATION_CD + YEAR_OF_DIAGNOSIS,
    data = df, family = family
  )
}

fit_optimal_care_model <- function(df, family = "binomial") {
  # Logistic regression estimating probability of optimal care
  glm(
    optcare ~ LCAprofile + ANALYTIC_STAGE_GROUP + facility +
      CDCC_TOTAL_BEST + FACILITY_LOCATION_CD + YEAR_OF_DIAGNOSIS,
    data = df, family = family
  )
}

#' Run logistic regression analyses on latent class assignments
#'
#' @param data_path Path to the `.RData` file produced by the LCA workflow.
#' @param config Configuration list (typically from `load_config()`).
#' @param dry_run If `TRUE`, validate inputs and exit without running the models.
#' @param verbose Logical flag to print status messages.
#' @param show_progress Display a progress bar for major steps when `TRUE`.
#'
#' @return List containing fitted minimal treatment and optimal care models, or
#'   an empty list when `dry_run` is `TRUE`.
#' @export
#'
#' @examples
#' cfg <- load_config()
#' models <- run_regression("data/lca_earlypuf.RData", cfg)
run_regression <- function(data_path, config, dry_run = FALSE, verbose = TRUE,
                           show_progress = TRUE) {
  require_data_file(data_path)
  if (dry_run) {
    if (verbose) message("Dry run: inputs validated. Would load ", data_path)
    return(invisible(list()))
  }

  steps <- c("Loading data", "Fitting minimal treatment model",
             "Fitting optimal care model")
  pb <- NULL
  if (show_progress) {
    pb <- progress::progress_bar$new(
      total = length(steps),
      format = "[:bar] :percent eta::eta :what"
    )
  }
  tick <- function(msg) {
    if (verbose) message(msg)
    if (!is.null(pb)) pb$tick(tokens = list(what = msg))
  }

  tick(steps[1])
  df <- load_lca_results(data_path, verbose = verbose)
  tick(steps[2])
  mintreat <- fit_min_treatment_model(df, family = config$regression$family)
  tick(steps[3])
  optcare <- fit_optimal_care_model(df, family = config$regression$family)
  if (verbose) message("Regression analysis complete")
  list(mintreat = mintreat, optcare = optcare)
}
