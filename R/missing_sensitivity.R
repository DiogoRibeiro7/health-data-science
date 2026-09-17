# ------------------------------------------------------------------------------
# File: missing_sensitivity.R
# Purpose: Delta-adjusted pattern-mixture sensitivity analysis for missing data.
# ------------------------------------------------------------------------------

.extract_scalar_analysis <- function(result) {
  if (is.numeric(result) && !is.null(names(result)) &&
      all(c("estimate", "std_error") %in% names(result))) {
    estimate <- result[["estimate"]]
    std_error <- result[["std_error"]]
  } else if (is.list(result) &&
             all(c("estimate", "std_error") %in% names(result))) {
    estimate <- result$estimate
    std_error <- result$std_error
  } else {
    stop(
      "`analysis` must return named `estimate` and `std_error` scalars",
      call. = FALSE
    )
  }

  if (!is.numeric(estimate) || length(estimate) != 1L ||
      !is.finite(estimate)) {
    stop("`analysis` returned an invalid estimate", call. = FALSE)
  }
  if (!is.numeric(std_error) || length(std_error) != 1L ||
      !is.finite(std_error) || std_error <= 0) {
    stop("`analysis` returned an invalid standard error", call. = FALSE)
  }

  c(estimate = estimate, std_error = std_error)
}

.pool_delta_scalar <- function(estimates, std_errors, conf_level) {
  m <- length(estimates)
  within_variance <- mean(std_errors^2)
  between_variance <- if (m > 1L) stats::var(estimates) else 0
  total_variance <- within_variance + (1 + 1 / m) * between_variance
  pooled_estimate <- mean(estimates)
  pooled_se <- sqrt(total_variance)

  if (between_variance <= .Machine$double.eps) {
    degrees_freedom <- Inf
  } else {
    degrees_freedom <- (m - 1) *
      (1 + within_variance / ((1 + 1 / m) * between_variance))^2
  }

  alpha <- 1 - conf_level
  critical_value <- if (is.finite(degrees_freedom)) {
    stats::qt(1 - alpha / 2, df = degrees_freedom)
  } else {
    stats::qnorm(1 - alpha / 2)
  }

  statistic <- pooled_estimate / pooled_se
  p_value <- if (is.finite(degrees_freedom)) {
    2 * stats::pt(abs(statistic), df = degrees_freedom, lower.tail = FALSE)
  } else {
    2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  }

  relative_increase <- if (within_variance > 0) {
    ((1 + 1 / m) * between_variance) / within_variance
  } else if (between_variance > 0) {
    Inf
  } else {
    0
  }
  fraction_missing_information <- if (is.finite(relative_increase)) {
    (relative_increase + 2 / (degrees_freedom + 3)) /
      (relative_increase + 1)
  } else {
    1
  }

  c(
    estimate = pooled_estimate,
    std_error = pooled_se,
    statistic = statistic,
    p_value = p_value,
    conf_low = pooled_estimate - critical_value * pooled_se,
    conf_high = pooled_estimate + critical_value * pooled_se,
    within_variance = within_variance,
    between_variance = between_variance,
    total_variance = total_variance,
    degrees_freedom = degrees_freedom,
    fraction_missing_information = fraction_missing_information
  )
}

#' Delta-adjusted missing-data sensitivity analysis
#'
#' Performs a pattern-mixture sensitivity analysis for a numeric outcome. First,
#' multiple imputation is performed under a missing-at-random (MAR) model using
#' [mice::mice]. For each supplied delta, only outcome values that were missing
#' in the original data are shifted by that amount. The user-supplied analysis
#' is then fitted to every completed data set and pooled with Rubin's rules.
#'
#' Delta is expressed on the original outcome scale. A negative delta encodes an
#' assumption that unobserved outcomes are systematically lower than their MAR
#' imputations; a positive delta encodes the opposite assumption. This is a
#' sensitivity analysis, not an identified MNAR model.
#'
#' @param data Data frame containing the analysis variables.
#' @param outcome Name of the numeric outcome whose missing values are shifted.
#' @param deltas Finite numeric vector of delta assumptions to evaluate.
#' @param analysis Function taking one completed data frame and returning named
#'   numeric scalars `estimate` and `std_error`.
#' @param m Number of multiple imputations. Must be at least 2.
#' @param maxit Number of MICE iterations.
#' @param seed Integer random seed used by MICE.
#' @param conf_level Confidence level used after Rubin pooling.
#' @param group Optional column restricting delta adjustment to a subgroup.
#' @param group_value Value or values of `group` to receive the delta shift.
#' @param method Optional MICE method vector passed to [mice::mice].
#' @param predictor_matrix Optional predictor matrix passed to [mice::mice].
#'
#' @return An object of class `hds_delta_sensitivity` with a pooled results table
#'   and metadata describing the MAR imputation and delta-targeted observations.
#' @export
run_delta_sensitivity <- function(
    data,
    outcome,
    deltas,
    analysis,
    m = 20,
    maxit = 5,
    seed = 1,
    conf_level = 0.95,
    group = NULL,
    group_value = NULL,
    method = NULL,
    predictor_matrix = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  if (!is.character(outcome) || length(outcome) != 1L ||
      is.na(outcome) || !nzchar(outcome) || !outcome %in% names(data)) {
    stop("`outcome` must name one column in `data`", call. = FALSE)
  }
  if (!is.numeric(data[[outcome]])) {
    stop("Delta adjustment currently requires a numeric outcome", call. = FALSE)
  }
  if (!is.numeric(deltas) || length(deltas) == 0L ||
      anyNA(deltas) || any(!is.finite(deltas))) {
    stop("`deltas` must be a non-empty vector of finite numbers", call. = FALSE)
  }
  if (anyDuplicated(deltas)) {
    stop("`deltas` cannot contain duplicates", call. = FALSE)
  }
  if (!is.function(analysis)) {
    stop("`analysis` must be a function", call. = FALSE)
  }
  if (!is.numeric(m) || length(m) != 1L || !is.finite(m) ||
      m < 2 || m != floor(m)) {
    stop("`m` must be an integer of at least 2", call. = FALSE)
  }
  if (!is.numeric(maxit) || length(maxit) != 1L || !is.finite(maxit) ||
      maxit < 1 || maxit != floor(maxit)) {
    stop("`maxit` must be a positive integer", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed != floor(seed)) {
    stop("`seed` must be a finite integer", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be strictly between 0 and 1", call. = FALSE)
  }

  missing_outcome <- is.na(data[[outcome]])
  n_missing_outcome <- sum(missing_outcome)
  if (n_missing_outcome == 0L) {
    stop("The selected outcome has no missing values to perturb", call. = FALSE)
  }
  if (all(missing_outcome)) {
    stop("The selected outcome cannot be entirely missing", call. = FALSE)
  }

  if (is.null(group)) {
    delta_target <- missing_outcome
    group_description <- NULL
  } else {
    if (!is.character(group) || length(group) != 1L ||
        is.na(group) || !nzchar(group) || !group %in% names(data)) {
      stop("`group` must name one column in `data`", call. = FALSE)
    }
    if (is.null(group_value) || length(group_value) == 0L) {
      stop("`group_value` is required when `group` is supplied", call. = FALSE)
    }
    if (anyNA(data[[group]])) {
      stop(
        "The delta-defining group cannot contain missing values",
        call. = FALSE
      )
    }
    delta_target <- missing_outcome & data[[group]] %in% group_value
    group_description <- list(column = group, values = group_value)
  }

  n_delta_target <- sum(delta_target)
  if (n_delta_target == 0L) {
    stop("No originally missing outcomes match the delta target", call. = FALSE)
  }

  if (!is.null(method) && !is.character(method)) {
    stop("`method` must be NULL or a MICE method vector", call. = FALSE)
  }
  if (!is.null(predictor_matrix) && !is.matrix(predictor_matrix)) {
    stop("`predictor_matrix` must be NULL or a matrix", call. = FALSE)
  }

  ensure_packages("mice")
  mice_args <- list(
    data = data,
    m = as.integer(m),
    maxit = as.integer(maxit),
    seed = as.integer(seed),
    printFlag = FALSE
  )
  if (!is.null(method)) {
    mice_args$method <- method
  }
  if (!is.null(predictor_matrix)) {
    mice_args$predictorMatrix <- predictor_matrix
  }
  imputation <- do.call(mice::mice, mice_args)

  results <- lapply(deltas, function(delta) {
    estimates <- numeric(m)
    std_errors <- numeric(m)

    for (i in seq_len(m)) {
      completed <- mice::complete(imputation, action = i)
      completed[[outcome]][delta_target] <-
        completed[[outcome]][delta_target] + delta

      analysis_result <- .extract_scalar_analysis(analysis(completed))
      estimates[i] <- analysis_result[["estimate"]]
      std_errors[i] <- analysis_result[["std_error"]]
    }

    pooled <- .pool_delta_scalar(estimates, std_errors, conf_level)
    data.frame(
      delta = delta,
      estimate = pooled[["estimate"]],
      std_error = pooled[["std_error"]],
      statistic = pooled[["statistic"]],
      p_value = pooled[["p_value"]],
      conf_low = pooled[["conf_low"]],
      conf_high = pooled[["conf_high"]],
      within_variance = pooled[["within_variance"]],
      between_variance = pooled[["between_variance"]],
      total_variance = pooled[["total_variance"]],
      degrees_freedom = pooled[["degrees_freedom"]],
      fraction_missing_information =
        pooled[["fraction_missing_information"]],
      row.names = NULL,
      check.names = FALSE
    )
  })

  results <- do.call(rbind, results)
  results <- results[order(results$delta), , drop = FALSE]
  rownames(results) <- NULL

  structure(
    list(
      results = results,
      metadata = list(
        outcome = outcome,
        m = as.integer(m),
        maxit = as.integer(maxit),
        seed = as.integer(seed),
        conf_level = conf_level,
        n_rows = nrow(data),
        n_missing_outcome = n_missing_outcome,
        n_delta_target = n_delta_target,
        missing_fraction = n_missing_outcome / nrow(data),
        delta_target_fraction = n_delta_target / nrow(data),
        group = group_description,
        method = imputation$method
      )
    ),
    class = "hds_delta_sensitivity"
  )
}

#' Find a grid-based tipping point in delta sensitivity analysis
#'
#' Compares sensitivity results with the MAR scenario (`delta = 0`) and returns
#' the nearest evaluated delta at which the chosen inferential state changes.
#' This is a grid tipping point: it does not interpolate between evaluated delta
#' values.
#'
#' @param sensitivity Object returned by [run_delta_sensitivity].
#' @param null Null value for the scalar estimand.
#' @param direction Search direction from delta zero: `"both"`, `"lower"`, or
#'   `"higher"`.
#' @param criterion Tipping criterion. `"confidence"` detects a change among
#'   confidence interval entirely below, containing, or entirely above `null`.
#'   `"estimate"` detects a change in the sign of the point estimate relative to
#'   `null`.
#'
#' @return A one-row data frame describing the baseline state and nearest
#'   evaluated tipping point. Tipping fields are `NA` when no evaluated delta
#'   changes the inferential state.
#' @export
find_delta_tipping_point <- function(
    sensitivity,
    null = 0,
    direction = c("both", "lower", "higher"),
    criterion = c("confidence", "estimate")) {
  direction <- match.arg(direction)
  criterion <- match.arg(criterion)

  if (!inherits(sensitivity, "hds_delta_sensitivity")) {
    stop("`sensitivity` must be created by `run_delta_sensitivity()`", call. = FALSE)
  }
  if (!is.numeric(null) || length(null) != 1L || !is.finite(null)) {
    stop("`null` must be a finite numeric scalar", call. = FALSE)
  }

  results <- sensitivity$results
  zero_rows <- which(results$delta == 0)
  if (length(zero_rows) != 1L) {
    stop("A unique `delta = 0` scenario is required for tipping analysis", call. = FALSE)
  }

  state <- if (criterion == "confidence") {
    ifelse(
      results$conf_low > null,
      "above",
      ifelse(results$conf_high < null, "below", "includes")
    )
  } else {
    ifelse(
      results$estimate > null,
      "above",
      ifelse(results$estimate < null, "below", "equal")
    )
  }

  baseline_index <- zero_rows[[1]]
  baseline_state <- state[baseline_index]
  candidate <- switch(
    direction,
    both = which(results$delta != 0),
    lower = which(results$delta < 0),
    higher = which(results$delta > 0)
  )

  candidate <- candidate[state[candidate] != baseline_state]
  if (length(candidate) > 0L) {
    distance <- abs(results$delta[candidate])
    tipping_index <- candidate[which.min(distance)]
    tipping <- results[tipping_index, , drop = FALSE]
    tipping_state <- state[tipping_index]
  } else {
    tipping <- data.frame(
      delta = NA_real_,
      estimate = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_
    )
    tipping_state <- NA_character_
  }

  data.frame(
    criterion = criterion,
    direction = direction,
    null = null,
    baseline_state = baseline_state,
    tipping_delta = tipping$delta[[1]],
    tipping_state = tipping_state,
    estimate = tipping$estimate[[1]],
    conf_low = tipping$conf_low[[1]],
    conf_high = tipping$conf_high[[1]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
