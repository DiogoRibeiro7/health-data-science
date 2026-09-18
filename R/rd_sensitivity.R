# ------------------------------------------------------------------------------
# File: rd_sensitivity.R
# Purpose: Sensitivity utilities for regression-discontinuity designs.
# ------------------------------------------------------------------------------

.extract_rd_robust_row <- function(fit, cutoff, label = NULL) {
  required_row <- "Robust"
  coef <- as.matrix(fit$coef)
  se <- as.matrix(fit$se)
  pv <- as.matrix(fit$pv)
  ci <- as.matrix(fit$ci)

  matrices <- list(coef = coef, se = se, pv = pv, ci = ci)
  missing_row <- vapply(
    matrices,
    function(x) is.null(rownames(x)) || !required_row %in% rownames(x),
    logical(1)
  )
  if (any(missing_row)) {
    stop(
      paste(
        "The `rdrobust` result does not expose the named Robust inference row",
        "in all required components"
      ),
      call. = FALSE
    )
  }

  estimate <- unname(coef[required_row, 1L])
  std_error <- unname(se[required_row, 1L])
  p_value <- unname(pv[required_row, 1L])
  conf_low <- unname(ci[required_row, 1L])
  conf_high <- unname(ci[required_row, 2L])

  values <- c(estimate, std_error, p_value, conf_low, conf_high)
  if (any(!is.finite(values))) {
    stop("The Robust `rdrobust` inference row contains non-finite values",
         call. = FALSE)
  }

  data.frame(
    label = if (is.null(label)) NA_character_ else as.character(label),
    cutoff = cutoff,
    estimate = estimate,
    std_error = std_error,
    p_value = p_value,
    conf_low = conf_low,
    conf_high = conf_high,
    inference = "robust_bias_corrected",
    row.names = NULL,
    check.names = FALSE
  )
}

#' Evaluate regression-discontinuity estimates across bandwidths
#'
#' Re-fits the same local-polynomial RD specification across a prespecified grid
#' of common left/right bandwidths. The grid is supplied by the analyst and no
#' bandwidth is selected from the sensitivity results.
#'
#' @param model Object returned by [fit_regression_discontinuity].
#' @param bandwidths Positive numeric vector of bandwidths to evaluate.
#'
#' @return Data frame with one robust-bias-corrected RD estimate per bandwidth.
#' @export
rd_bandwidth_sensitivity <- function(model, bandwidths) {
  if (!inherits(model, "hds_rd_model")) {
    stop("`model` must be created by `fit_regression_discontinuity()`", call. = FALSE)
  }
  if (!is.numeric(bandwidths) || length(bandwidths) == 0L ||
      anyNA(bandwidths) || any(!is.finite(bandwidths)) || any(bandwidths <= 0)) {
    stop("`bandwidths` must be a non-empty vector of positive finite numbers", call. = FALSE)
  }
  if (anyDuplicated(bandwidths)) {
    stop("`bandwidths` cannot contain duplicates", call. = FALSE)
  }

  ensure_packages("rdrobust")
  data <- model$data
  meta <- model$metadata
  x <- data[[meta$running]]
  y <- data[[meta$outcome]]
  covs <- if (length(meta$covariates) > 0L) as.matrix(data[meta$covariates]) else NULL

  rows <- lapply(sort(bandwidths), function(h) {
    fit <- rdrobust::rdrobust(
      y = y,
      x = x,
      c = meta$cutoff,
      p = meta$p,
      q = meta$q,
      h = h,
      kernel = meta$kernel,
      masspoints = meta$masspoints,
      covs = covs
    )
    row <- .extract_rd_robust_row(fit, cutoff = meta$cutoff, label = paste0("h=", h))
    row$bandwidth <- h
    row$n_left_within_bandwidth <- sum(x >= meta$cutoff - h & x < meta$cutoff)
    row$n_right_within_bandwidth <- sum(x >= meta$cutoff & x <= meta$cutoff + h)
    row
  })

  out <- do.call(rbind, rows)
  out <- out[, c(
    "bandwidth", "n_left_within_bandwidth", "n_right_within_bandwidth",
    "estimate", "std_error", "p_value", "conf_low", "conf_high", "cutoff"
  )]
  rownames(out) <- NULL
  attr(out, "healthdatascience") <- list(
    analysis = "rd-bandwidth-sensitivity",
    cutoff = meta$cutoff,
    p = meta$p,
    q = meta$q,
    kernel = meta$kernel,
    estimand = meta$estimand,
    interpretation = paste(
      "Stability across prespecified bandwidths is a sensitivity diagnostic;",
      "it does not identify a uniquely correct bandwidth or validate the RD design."
    )
  )
  out
}

#' Evaluate placebo cutoffs in a regression-discontinuity design
#'
#' Fits the same RD specification at prespecified placebo cutoffs where no true
#' treatment discontinuity is expected. Placebo cutoffs must lie inside the
#' observed running-variable support and cannot equal the true cutoff.
#'
#' @param model Object returned by [fit_regression_discontinuity].
#' @param cutoffs Numeric vector of placebo cutoffs.
#' @param bandwidth Optional common bandwidth used at every placebo cutoff. When
#'   NULL, `rdrobust` selects bandwidths separately at each placebo cutoff.
#'
#' @return Data frame with one robust-bias-corrected placebo estimate per cutoff.
#' @export
rd_placebo_cutoffs <- function(model, cutoffs, bandwidth = NULL) {
  if (!inherits(model, "hds_rd_model")) {
    stop("`model` must be created by `fit_regression_discontinuity()`", call. = FALSE)
  }
  if (!is.numeric(cutoffs) || length(cutoffs) == 0L ||
      anyNA(cutoffs) || any(!is.finite(cutoffs))) {
    stop("`cutoffs` must be a non-empty vector of finite numbers", call. = FALSE)
  }
  if (anyDuplicated(cutoffs)) {
    stop("`cutoffs` cannot contain duplicates", call. = FALSE)
  }
  if (any(cutoffs == model$metadata$cutoff)) {
    stop("Placebo cutoffs cannot equal the true RD cutoff", call. = FALSE)
  }
  if (!is.null(bandwidth)) {
    if (!is.numeric(bandwidth) || length(bandwidth) != 1L ||
        !is.finite(bandwidth) || bandwidth <= 0) {
      stop("`bandwidth` must be NULL or one positive finite number", call. = FALSE)
    }
    true_cutoff <- model$metadata$cutoff
    crosses_true_cutoff <- abs(cutoffs - true_cutoff) <= bandwidth
    if (any(crosses_true_cutoff)) {
      stop(
        paste(
          "Fixed placebo bandwidth windows cannot touch or cross the true",
          "RD cutoff"
        ),
        call. = FALSE
      )
    }
  }

  ensure_packages("rdrobust")
  data <- model$data
  meta <- model$metadata
  x <- data[[meta$running]]
  y <- data[[meta$outcome]]
  covs <- if (length(meta$covariates) > 0L) as.matrix(data[meta$covariates]) else NULL
  x_range <- range(x)
  if (any(cutoffs <= x_range[1] | cutoffs >= x_range[2])) {
    stop("Every placebo cutoff must lie strictly inside the running-variable support", call. = FALSE)
  }

  rows <- lapply(sort(cutoffs), function(placebo_cutoff) {
    if (!any(x < placebo_cutoff) || !any(x >= placebo_cutoff)) {
      stop("Each placebo cutoff requires observations on both sides", call. = FALSE)
    }

    args <- list(
      y = y,
      x = x,
      c = placebo_cutoff,
      p = meta$p,
      q = meta$q,
      kernel = meta$kernel,
      bwselect = meta$bwselect,
      masspoints = meta$masspoints,
      covs = covs
    )
    if (!is.null(bandwidth)) {
      args$h <- bandwidth
    }
    fit <- do.call(rdrobust::rdrobust, args)
    row <- .extract_rd_robust_row(
      fit,
      cutoff = placebo_cutoff,
      label = paste0("placebo=", placebo_cutoff)
    )
    row$distance_from_true_cutoff <- placebo_cutoff - meta$cutoff
    row$bandwidth <- if (is.null(bandwidth)) NA_real_ else bandwidth
    row
  })

  out <- do.call(rbind, rows)
  out <- out[, c(
    "cutoff", "distance_from_true_cutoff", "bandwidth", "estimate",
    "std_error", "p_value", "conf_low", "conf_high"
  )]
  rownames(out) <- NULL
  attr(out, "healthdatascience") <- list(
    analysis = "rd-placebo-cutoffs",
    true_cutoff = meta$cutoff,
    estimand = meta$estimand,
    interpretation = paste(
      "Placebo discontinuities can reveal local specification concerns or other",
      "threshold effects; isolated significant placebo estimates are diagnostics,",
      "not an automatic falsification rule."
    )
  )
  out
}
