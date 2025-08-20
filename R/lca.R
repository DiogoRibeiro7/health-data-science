# ------------------------------------------------------------------------------
# File: lca.R
# Purpose: Data preparation and latent class analysis utilities for the health
#   data science toolkit.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Prepare NCDB data for latent class analysis
#'
#' Encodes categorical variables as factors with human-readable labels so
#' they are suitable for latent class analysis.
#'
#' @param df Data frame containing raw NCDB fields.
#'
#' @return Data frame with factors encoded for LCA.
#'
#' @examples
#' df <- prepare_lca_data(raw_df)
prepare_lca_data <- function(df) {
  df$race3 <- factor(df$race3, levels = 1:3, labels = c("White", "Black", "Other"))
  df$urbandwell <- factor(df$urbandwell, levels = 1:3, labels = c("Metro", "Urban", "Rural"))
  df$hispanic3 <- factor(df$hispanic3, levels = 1:3, labels = c("Non-Hispanic", "Hispanic", "Unknown"))
  df$insurancetype <- factor(
    df$insurancetype,
    levels = 1:6,
    labels = c("Not Insured", "Private", "Medicaid", "Medicare", "Other Govt", "Unknown")
  )
  df$age4 <- factor(df$age4, levels = 1:4, labels = c("18-49", "50-64", "65-74", "75+"))
  df$SES <- factor(df$SES, levels = 1:3, labels = c("Low SES", "Med SES", "High SES"))
  df$facility <- factor(
    df$FACILITY_TYPE_CD,
    levels = c(1:4, 9),
    labels = c("Community Cancer", "Comprehensive Cancer", "Academic/Research", "Integrated Network", "Other")
  )
  df
}

#' Read and prepare LCA input data
#'
#' @param path Path to the NCDB CSV file.
#'
#' @return Prepared data frame ready for feature selection and modelling.
#'
#' @examples
#' early <- read_lca_input("data/puf_early.csv")
read_lca_input <- function(path, cache = TRUE) {
  if (cache) {
    cache_file <- file.path("cache", paste0(basename(path), ".rds"))
    cache_result(cache_file, {
      df <- read_csv_safely(path)
      prepare_lca_data(df)
    })
  } else {
    df <- read_csv_safely(path)
    prepare_lca_data(df)
  }
}

#' Select variables required for LCA modelling
#'
#' @param df Prepared dataset returned by `read_lca_input()`.
#'
#' @return Tibble with the subset of variables used in the latent class model.
select_lca_variables <- function(df) {
  df %>% dplyr::select(
    PUF_CASE_ID, optcare, SEX, facility, DX_RX_STARTED_DAYS, CROWFLY,
    CDCC_TOTAL_BEST, race3, hispanic3, urbandwell, age4, SES, insurancetype
  )
}

#' Fit latent class model
#'
#' Uses configuration parameters for model fitting. When `cfg$auto_classes` is
#' `TRUE`, the optimal number of classes is determined using
#' `optimal_lca_classes()`.
#'
#' @param df_full Prepared dataset with all variables.
#' @param df_subset Subset returned by `select_lca_variables()`.
#' @param cfg List of LCA configuration parameters.
#'
#' @return Fitted `poLCA` model object.
#'
#' @examples
#' cfg <- load_config()
#' lca_model <- fit_lca_model(early, lca_vars, cfg$lca)
fit_lca_model <- function(df_full, df_subset, cfg) {
  eff <- with(df_subset, cbind(race3, hispanic3, urbandwell, age4, SES, insurancetype) ~ 1)
  params <- cfg
  if (isTRUE(cfg$auto_classes)) {
    opt <- optimal_lca_classes(df_full, df_subset, k_range = 2:10)
    params$nclass <- opt$best_k
  }
  poLCA(eff, df_full,
        nclass = params$nclass,
        maxiter = params$maxiter,
        tol = params$tol,
        na.rm = TRUE,
        nrep = params$nrep,
        verbose = params$verbose,
        calc.se = TRUE)
}

#' Summarize latent class membership
#'
#' Generates a boxplot of class membership probabilities and returns the most
#' likely class for each individual.
#'
#' @param lc_model Fitted `poLCA` model.
#'
#' @return Integer vector of class assignments.
summarize_lca <- function(lc_model) {
  prob <- lc_model$posterior
  class <- lc_model$predclass
  idx <- seq_along(class)
  memclass.prob <- prob[cbind(idx, class)]
  lcaprob <- data.frame(class, memclass.prob)
  mem.lcaplot <- ggplot(lcaprob, aes(x = factor(class), y = memclass.prob)) +
    geom_boxplot() + ggtitle("LCA Membership Probability")
  print(mem.lcaplot)
  class
}

#' Merge class assignments with original data
#'
#' @param df_full Prepared dataset returned by `read_lca_input()`.
#' @param df_subset Subset used in model fitting.
#' @param classes Vector of class assignments from `summarize_lca()`.
#'
#' @return Data frame combining the original data with class labels.
merge_lca_results <- function(df_full, df_subset, classes) {
  complete <- na.omit(df_subset[, c(1, 8:13)])
  lcacomplete <- cbind(complete, class7 = classes)
  merge(df_full, lcacomplete, by = "PUF_CASE_ID")
}

#' Run the complete latent class analysis workflow
#'
#' @param data_path Path to the input CSV file.
#' @param output_path File path to save the fitted model and merged data.
#' @param config Configuration list (typically from `load_config()`).
#' @param dry_run If `TRUE`, validate inputs and exit without running the
#'   analysis.
#' @param verbose Logical flag to print status messages.
#' @param show_progress Display a progress bar for major steps when `TRUE`.
#'
#' @return Invisibly returns the fitted `poLCA` model or `NULL` when
#'   `dry_run` is enabled.
#'
#' @examples
#' run_lca("data/puf_early.csv", "data/lca_earlypuf.RData")
run_lca <- function(data_path, output_path, config, dry_run = FALSE,
                    verbose = TRUE, show_progress = TRUE) {
  if (!is.character(data_path) || length(data_path) != 1) {
    stop("`data_path` must be a single character string", call. = FALSE)
  }
  if (!is.character(output_path) || length(output_path) != 1) {
    stop("`output_path` must be a single character string", call. = FALSE)
  }

  require_data_file(data_path)
  ensure_dir(output_path)

  if (dry_run) {
    if (verbose) {
      message("Dry run: inputs validated. Results would be saved to ",
              output_path)
    }
    return(invisible(NULL))
  }

  steps <- c("Reading data", "Selecting variables", "Fitting model",
             "Merging results", "Saving output")
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

  early.puf <- monitor_step(steps[1], read_lca_input(data_path), pb, verbose)
  lca.earlydata <- monitor_step(steps[2], select_lca_variables(early.puf), pb, verbose)
  lc7 <- monitor_step(steps[3], fit_lca_model(early.puf, lca.earlydata, config$lca), pb, verbose)
  class7 <- monitor_step(steps[4], summarize_lca(lc7), pb, verbose)
  monitor_step(steps[5], {
    lca.pufdata <- merge_lca_results(early.puf, lca.earlydata, class7)
    save(lc7, lca.pufdata, file = output_path)
  }, pb, verbose)

  if (verbose) message("LCA analysis complete")
  invisible(lc7)
}

