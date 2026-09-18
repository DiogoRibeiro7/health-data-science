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
    }, depends = path, max_size_mb = 500)
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
  dplyr::select(
    df,
    PUF_CASE_ID, optcare, SEX, facility, DX_RX_STARTED_DAYS, CROWFLY,
    CDCC_TOTAL_BEST, race3, hispanic3, urbandwell, age4, SES, insurancetype
  )
}

#' Validate data before latent class modelling
#'
#' Performs a series of checks to identify common data quality issues that can
#' hinder model convergence. Warnings are logged when potential problems are
#' detected and a fatal error is raised when the sample size is clearly
#' insufficient for the requested number of classes.
#'
#' @param df_sub Data frame containing the variables used for LCA.
#' @param nclass Number of classes to be fitted.
#'
#' @return List of detected issues (invisible) for further inspection.
#' @export
validate_lca_data <- function(df_sub, nclass) {
  issues <- list()

  miss <- sapply(df_sub, function(x) sum(is.na(x)))
  if (any(miss > 0)) {
    issues$missing <- miss[miss > 0]
    logger::log_warn(
      "Missing data detected in LCA variables",
      context = list(missing = issues$missing)
    )
  }

  low_freq <- lapply(df_sub[sapply(df_sub, is.factor)], function(col) {
    tbl <- table(col)
    tbl[tbl < 5]
  })
  low_freq <- low_freq[sapply(low_freq, length) > 0]
  if (length(low_freq) > 0) {
    issues$low_frequency <- low_freq
    logger::log_warn(
      "Low-frequency categories found in factors",
      context = list(low_frequency = low_freq)
    )
  }

  if (nrow(df_sub) < nclass * 5) {
    logger::log_error(
      sprintf(
        "Sample size (%d) is too small for %d classes",
        nrow(df_sub), nclass
      )
    )
    stop(
      "Sample size is likely inadequate for the chosen number of classes.\n",
      "Consider reducing `nclass` or increasing the dataset size.",
      call. = FALSE
    )
  }

  fac_cols <- names(df_sub)[sapply(df_sub, is.factor)]
  if (length(fac_cols) > 1) {
    pairs <- combn(fac_cols, 2, simplify = FALSE)
    collinear <- list()
    for (p in pairs) {
      tab <- table(df_sub[[p[1]]], df_sub[[p[2]]])
      if (any(prop.table(tab) %in% c(0, 1))) {
        collinear[[paste(p, collapse = "_")]] <- tab
      }
    }
    if (length(collinear) > 0) {
      issues$collinearity <- collinear
      logger::log_warn(
        "Potential collinearity detected among categorical variables",
        context = list(collinearity = names(collinear))
      )
    }
  }

  invisible(issues)
}

#' Fit latent class model
#'
#' Fits a [poLCA::poLCA] model using supplied configuration parameters and
#' implements robust convergence monitoring with retry and fallback strategies.
#' When `cfg$auto_classes` is `TRUE`, the optimal number of classes is
#' determined via [`optimal_lca_classes()`] before fitting.
#'
#' @param df_full Data frame containing the full set of variables required by
#'   the latent class model.
#' @param df_subset Data frame produced by [`select_lca_variables()`] containing
#'   only the variables used in model estimation.
#' @param cfg Named list of LCA configuration parameters including `nclass`,
#'   `maxiter`, `tol`, `nrep`, `verbose`, and `parallel`. When `auto_classes`
#'   is `TRUE`, these defaults are augmented with the optimal number of classes.
#'
#' @return Fitted [`poLCA::poLCA`] model object.
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

  validate_lca_data(df_subset, params$nclass)
  logger::log_info(sprintf("Fitting LCA model with %d classes", params$nclass))

  seeds <- sample.int(1e5, max(1, params$nrep))
  pb <- NULL
  if (isTRUE(params$verbose) && requireNamespace("progress", quietly = TRUE) && length(seeds) > 1) {
    pb <- progress::progress_bar$new(total = length(seeds), format = "[:bar] :current/:total (:elapsed)")
  }

  fit_once <- function(seed) {
    set.seed(seed)
    poLCA::poLCA(
      eff,
      df_full,
      nclass = params$nclass,
      maxiter = params$maxiter,
      tol = params$tol,
      na.rm = TRUE,
      nrep = 1,
      verbose = FALSE,
      calc.se = TRUE
    )
  }

  attempt_fit <- function() {
    best <- NULL
    for (s in seeds) {
      if (!is.null(pb)) pb$tick()
      res <- try(fit_once(s), silent = TRUE)
      if (inherits(res, "try-error")) {
        logger::log_warn(sprintf("Convergence failed for seed %d: %s", s, attr(res, "condition")$message))
        next
      }
      if (!is.null(res$iter) && res$iter >= params$maxiter) {
        logger::log_warn(sprintf("Seed %d reached maximum iterations without convergence", s))
        next
      }
      if (is.null(best) || res$llik > best$llik) {
        best <- res
      }
    }
    best
  }

  model <- attempt_fit()
  if (is.null(model)) {
    if (params$nclass > 2) {
      logger::log_warn("Retrying LCA with fewer classes due to convergence failures")
      params$nclass <- params$nclass - 1
      return(fit_lca_model(df_full, df_subset, params))
    }
    logger::log_error("LCA model failed to converge after multiple attempts")
    stop(
      "LCA model failed to converge even after retries.\n",
      "Check data quality, reduce the number of classes, or adjust `maxiter`/`tol`.\n",
      "See documentation: docs/troubleshooting.md#lca", call. = FALSE
    )
  }
  model
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
  mem.lcaplot <- ggplot2::ggplot(
    lcaprob,
    ggplot2::aes(x = factor(class), y = memclass.prob)
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::ggtitle("LCA Membership Probability")
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
#' @param data_path Character string path to the NCDB-style CSV input file.
#'   The file must exist and contain all variables required by
#'   `prepare_lca_data()`.
#' @param output_path Destination file path (typically ending in `.RData`) used
#'   to save the fitted model object and merged data set.
#' @param config Named list of configuration parameters, usually read from
#'   `load_config()`. Must contain an `lca` element specifying model settings
#'   such as `nclass`, `maxiter`, and `tol`.
#' @param dry_run Logical flag; when `TRUE` the function validates inputs and
#'   required directories but does not execute the analysis.
#' @param verbose Logical flag to emit progress messages through the logger.
#' @param show_progress Logical flag indicating whether a progress bar should be
#'   displayed for major workflow steps. Requires the `progress` package.
#'
#' @return Invisibly returns the fitted [`poLCA::poLCA`] model. When
#'   `dry_run = TRUE`, `NULL` is returned.
#'
#' @examples
#' cfg <- load_config()
#' run_lca("data/puf_early.csv", "data/lca_earlypuf.RData", cfg)
run_lca <- function(data_path, output_path, config, dry_run = FALSE,
                    verbose = TRUE, show_progress = TRUE) {
  if (!is.character(data_path) || length(data_path) != 1) {
    logger::log_error("`data_path` must be a single character string")
    stop("`data_path` must be a single character string", call. = FALSE)
  }
  if (!is.character(output_path) || length(output_path) != 1) {
    logger::log_error("`output_path` must be a single character string")
    stop("`output_path` must be a single character string", call. = FALSE)
  }

  require_data_file(data_path)
  ensure_dir(output_path)

  if (dry_run) {
    if (verbose) {
      logger::log_info(sprintf("Dry run: inputs validated. Results would be saved to %s", output_path))
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
    if (verbose) logger::log_info(msg)
    if (!is.null(pb)) pb$tick(tokens = list(what = msg))
  }

  tryCatch({
    early.puf <- monitor_step(steps[1], read_lca_input(data_path), pb, verbose)
    lca.earlydata <- monitor_step(steps[2], select_lca_variables(early.puf), pb, verbose)
    lc7 <- monitor_step(steps[3], fit_lca_model(early.puf, lca.earlydata, config$lca), pb, verbose)
    class7 <- monitor_step(steps[4], summarize_lca(lc7), pb, verbose)
    monitor_step(steps[5], {
      lca.pufdata <- merge_lca_results(early.puf, lca.earlydata, class7)
      save(lc7, lca.pufdata, file = output_path)
    }, pb, verbose)
    if (verbose) logger::log_info("LCA analysis complete")
    invisible(lc7)
  }, interrupt = function(e) {
    logger::log_warn("LCA workflow interrupted by user")
    stop("LCA workflow interrupted", call. = FALSE)
  }, error = function(e) {
    logger::log_error(sprintf("LCA workflow failed: %s", e$message))
    diag <- collect_diagnostics()
    stop("LCA workflow failed: ", e$message,
         "\nCheck input data and configuration.", call. = FALSE)
  })
}
