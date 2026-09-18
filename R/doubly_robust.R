# ------------------------------------------------------------------------------
# File: doubly_robust.R
# Purpose: Doubly robust ATE and ATT estimation from propensity designs.
# ------------------------------------------------------------------------------

#' Estimate a doubly robust treatment effect
#'
#' Uses augmentation with outcome regression on top of a propensity-score design.
#' For `ATE`, the estimator is the augmented inverse-probability weighted (AIPW)
#' mean contrast. For `ATT`, the estimator targets the treated population and
#' augments the reweighted control outcome using a control outcome model.
#'
#' Double robustness refers to consistency when either the propensity model or
#' the outcome regression is correctly specified, under standard causal
#' identification assumptions and regularity conditions. It does not protect
#' against unmeasured confounding or positivity violations.
#'
#' @param design Object returned by [fit_propensity_design]. The design estimand
#'   must be `"ATE"` or `"ATT"`.
#' @param outcome Name of a numeric outcome column in the retained design data.
#' @param outcome_covariates Optional character vector of baseline covariates for
#'   the outcome regressions. Defaults to the propensity-design covariates.
#' @param conf_level Confidence level for the influence-function Wald interval.
#' @param na_action Missing-value policy for the outcome and outcome-model
#'   covariates: `"fail"` or `"omit"`.
#'
#' @return A one-row data frame containing the target estimand, estimate,
#'   influence-function standard error, Wald statistic, p-value, confidence
#'   interval, and nuisance-model metadata.
#' @export
estimate_doubly_robust <- function(
    design,
    outcome,
    outcome_covariates = NULL,
    conf_level = 0.95,
    na_action = c("fail", "omit")) {
  na_action <- match.arg(na_action)

  if (!inherits(design, "hds_propensity_design")) {
    stop("`design` must be created by `fit_propensity_design()`", call. = FALSE)
  }
  estimand <- design$metadata$estimand
  if (!estimand %in% c("ATE", "ATT")) {
    stop("Doubly robust estimation currently supports only ATE and ATT", call. = FALSE)
  }
  if (!is.character(outcome) || length(outcome) != 1L ||
      is.na(outcome) || !nzchar(outcome) || !outcome %in% names(design$data)) {
    stop("`outcome` must name one column in the design data", call. = FALSE)
  }
  if (!is.numeric(design$data[[outcome]])) {
    stop("`outcome` must be numeric", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be strictly between 0 and 1", call. = FALSE)
  }

  if (is.null(outcome_covariates)) {
    outcome_covariates <- design$metadata$covariates
  }
  if (!is.character(outcome_covariates) || length(outcome_covariates) == 0L ||
      anyNA(outcome_covariates) || any(!nzchar(outcome_covariates))) {
    stop("`outcome_covariates` must be a non-empty character vector", call. = FALSE)
  }
  if (anyDuplicated(outcome_covariates)) {
    stop("`outcome_covariates` cannot contain duplicates", call. = FALSE)
  }
  missing_columns <- setdiff(outcome_covariates, names(design$data))
  if (length(missing_columns) > 0L) {
    stop(
      paste("Missing outcome-model covariates:", paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }
  if (outcome %in% outcome_covariates) {
    stop("The outcome cannot also be an outcome-model covariate", call. = FALSE)
  }

  unsupported <- vapply(
    design$data[outcome_covariates],
    function(x) !(is.numeric(x) || is.logical(x) || is.factor(x)),
    logical(1)
  )
  if (any(unsupported)) {
    stop("Outcome-model covariates must be numeric, logical, or factor variables", call. = FALSE)
  }

  analysis_columns <- unique(c(outcome, outcome_covariates))
  missing_rows <- !stats::complete.cases(design$data[analysis_columns])
  omitted_rows <- sum(missing_rows)
  if (omitted_rows > 0L) {
    if (na_action == "fail") {
      stop(
        "Missing values detected; use `na_action = \"omit\"` to drop rows",
        call. = FALSE
      )
    }
  }

  keep <- !missing_rows
  data <- design$data[keep, , drop = FALSE]
  treatment <- design$treatment[keep]
  propensity <- design$propensity_score[keep]
  y <- data[[outcome]]

  if (nrow(data) == 0L) {
    stop("No complete observations remain after missing-value handling", call. = FALSE)
  }
  if (!any(treatment == 1L) || !any(treatment == 0L)) {
    stop("Both treatment groups must remain in the analysis", call. = FALSE)
  }
  if (!all(is.finite(y))) {
    stop("`outcome` must contain only finite values or NA", call. = FALSE)
  }

  quote_name <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
  rhs <- paste(vapply(outcome_covariates, quote_name, character(1)), collapse = " + ")
  formula <- stats::as.formula(paste(quote_name(outcome), "~", rhs))

  treated_data <- data[treatment == 1L, , drop = FALSE]
  control_data <- data[treatment == 0L, , drop = FALSE]
  outcome_model_1 <- stats::lm(formula, data = treated_data)
  outcome_model_0 <- stats::lm(formula, data = control_data)

  mu1 <- as.numeric(stats::predict(outcome_model_1, newdata = data))
  mu0 <- as.numeric(stats::predict(outcome_model_0, newdata = data))
  if (any(!is.finite(mu1)) || any(!is.finite(mu0))) {
    stop("Outcome-model predictions are non-finite", call. = FALSE)
  }

  a <- treatment
  p <- propensity
  n <- length(y)

  if (estimand == "ATE") {
    pseudo_outcome <-
      (mu1 - mu0) +
      a * (y - mu1) / p -
      (1 - a) * (y - mu0) / (1 - p)
    estimate <- mean(pseudo_outcome)
    influence <- pseudo_outcome - estimate
    target_fraction <- 1
  } else {
    treated_fraction <- mean(a)
    if (treated_fraction <= 0) {
      stop("ATT requires at least one treated observation", call. = FALSE)
    }

    # ATT augmentation:
    # E[Y1 - m0(X) | A=1] - E[(1-A) p(X)/(1-p(X)) (Y-m0(X))] / P(A=1)
    treated_component <- a * (y - mu0)
    control_augmentation <-
      (1 - a) * p / (1 - p) * (y - mu0)
    estimating_numerator <- treated_component - control_augmentation
    estimate <- mean(estimating_numerator) / treated_fraction

    # Ratio-estimator influence function for theta = E[g(O)] / E[A]:
    # IF(O) = {g(O) - theta A} / E[A].
    influence <-
      (estimating_numerator - estimate * a) / treated_fraction
    target_fraction <- treated_fraction
  }

  std_error <- sqrt(stats::var(influence) / n)
  statistic <- estimate / std_error
  critical_value <- stats::qnorm(1 - (1 - conf_level) / 2)

  data.frame(
    estimand = estimand,
    outcome = outcome,
    estimate = estimate,
    std_error = std_error,
    statistic = statistic,
    p_value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
    conf_low = estimate - critical_value * std_error,
    conf_high = estimate + critical_value * std_error,
    n = n,
    omitted_rows = omitted_rows,
    target_fraction = target_fraction,
    propensity_model = "logistic regression from hds_propensity_design",
    outcome_model = "separate linear regressions by treatment group",
    variance = "empirical influence-function variance with fitted nuisances",
    row.names = NULL,
    check.names = FALSE
  )
}
