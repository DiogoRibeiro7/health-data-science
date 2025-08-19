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
read_lca_input <- function(path) {
  df <- read_csv_safely(path)
  prepare_lca_data(df)
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
#' The number of classes defaults to seven to mirror prior NCDB analyses and
#' maintain a balance between model fit and interpretability. Higher numbers of
#' classes produced unstable solutions in exploratory runs.
#'
#' @param df_full Prepared dataset with all variables.
#' @param df_subset Subset returned by `select_lca_variables()`.
#' @param nclass Number of latent classes to extract. Defaults to 7.
#'
#' @return Fitted `poLCA` model object.
#'
#' @examples
#' lca_model <- fit_lca_model(early, lca_vars)
fit_lca_model <- function(df_full, df_subset, nclass = 7) {
  eff <- with(df_subset, cbind(race3, hispanic3, urbandwell, age4, SES, insurancetype) ~ 1)
  poLCA(eff, df_full, nclass = nclass, maxiter = 5000,
        tol = 1e-5, na.rm = TRUE, nrep = 20, verbose = TRUE, calc.se = TRUE)
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
#'
#' @return Invisibly returns the fitted `poLCA` model.
#'
#' @examples
#' run_lca("data/puf_early.csv", "data/lca_earlypuf.RData")
run_lca <- function(data_path, output_path) {
  if (!is.character(data_path) || length(data_path) != 1) {
    stop("`data_path` must be a single character string", call. = FALSE)
  }
  if (!is.character(output_path) || length(output_path) != 1) {
    stop("`output_path` must be a single character string", call. = FALSE)
  }

  message("Reading data from: ", data_path)
  message("Saving results to: ", output_path)
  ensure_dir(output_path)

  # --- Read and prepare data ---
  early.puf <- read_lca_input(data_path)
  lca.earlydata <- select_lca_variables(early.puf)

  # --- Fit latent class model ---
  lc7 <- fit_lca_model(early.puf, lca.earlydata)

  # --- Summarize membership and merge with source data ---
  class7 <- summarize_lca(lc7)
  lca.pufdata <- merge_lca_results(early.puf, lca.earlydata, class7)

  save(lc7, lca.pufdata, file = output_path)
  invisible(lc7)
}

