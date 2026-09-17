# ------------------------------------------------------------------------------
# File: propensity.R
# Purpose: Propensity-score design, weighting, overlap, and balance diagnostics.
# ------------------------------------------------------------------------------

.weighted_mean <- function(x, w) {
  sum(w * x) / sum(w)
}

.weighted_variance <- function(x, w) {
  mu <- .weighted_mean(x, w)
  sum(w * (x - mu)^2) / sum(w)
}

.effective_sample_size <- function(w) {
  if (length(w) == 0L || sum(w^2) == 0) {
    return(0)
  }
  sum(w)^2 / sum(w^2)
}

#' Build a propensity-score weighting design
#'
#' Estimates a binary-treatment propensity model with logistic regression and
#' constructs inverse-probability or overlap weights for an explicit target
#' estimand. The function separates study design from outcome analysis.
#'
#' @param data Data frame containing treatment and baseline covariates.
#' @param treatment Name of the treatment column.
#' @param covariates Character vector of baseline covariate column names.
#' @param estimand Target population: average treatment effect (`"ATE"`),
#'   average treatment effect among the treated (`"ATT"`), or average treatment
#'   effect in the overlap population (`"ATO"`).
#' @param treated_value Value of `treatment` defining the treated group.
#' @param na_action Missing-value policy for treatment/covariates: `"fail"` or
#'   `"omit"`.
#' @param positivity_threshold Threshold used only for positivity diagnostics.
#'   Scores below this value or above `1 - positivity_threshold` trigger a
#'   warning but are not silently trimmed.
#'
#' @return An object of class `hds_propensity_design` containing the fitted
#'   propensity model, retained data, scores, weights, model matrix, and
#'   diagnostics.
#' @export
fit_propensity_design <- function(
    data,
    treatment,
    covariates,
    estimand = c("ATE", "ATT", "ATO"),
    treated_value = 1,
    na_action = c("fail", "omit"),
    positivity_threshold = 0.01) {
  estimand <- match.arg(estimand)
  na_action <- match.arg(na_action)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  if (!is.character(treatment) || length(treatment) != 1L ||
      is.na(treatment) || !nzchar(treatment) || !treatment %in% names(data)) {
    stop("`treatment` must name one column in `data`", call. = FALSE)
  }
  if (!is.character(covariates) || length(covariates) == 0L ||
      anyNA(covariates) || any(!nzchar(covariates))) {
    stop("`covariates` must be a non-empty character vector", call. = FALSE)
  }
  if (anyDuplicated(covariates)) {
    stop("`covariates` cannot contain duplicates", call. = FALSE)
  }
  if (treatment %in% covariates) {
    stop("The treatment column cannot also be a covariate", call. = FALSE)
  }
  missing_columns <- setdiff(covariates, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      paste("Missing covariate columns:", paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }
  if (!is.numeric(positivity_threshold) ||
      length(positivity_threshold) != 1L ||
      !is.finite(positivity_threshold) ||
      positivity_threshold <= 0 || positivity_threshold >= 0.5) {
    stop("`positivity_threshold` must be strictly between 0 and 0.5", call. = FALSE)
  }

  unsupported <- vapply(
    data[covariates],
    function(x) !(is.numeric(x) || is.logical(x) || is.factor(x)),
    logical(1)
  )
  if (any(unsupported)) {
    stop("Covariates must be numeric, logical, or factor variables", call. = FALSE)
  }

  model_columns <- c(treatment, covariates)
  missing_rows <- !stats::complete.cases(data[model_columns])
  omitted_rows <- sum(missing_rows)
  if (omitted_rows > 0L) {
    if (na_action == "fail") {
      stop(
        "Missing values detected; use `na_action = \"omit\"` to drop rows",
        call. = FALSE
      )
    }
    data <- data[!missing_rows, , drop = FALSE]
  }
  if (nrow(data) == 0L) {
    stop("No complete observations remain after missing-value handling", call. = FALSE)
  }

  treated <- data[[treatment]] == treated_value
  if (anyNA(treated)) {
    stop("`treated_value` cannot be matched unambiguously", call. = FALSE)
  }
  if (!any(treated) || all(treated)) {
    stop("Both treated and untreated observations are required", call. = FALSE)
  }

  quote_name <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
  rhs <- paste(vapply(covariates, quote_name, character(1)), collapse = " + ")
  propensity_data <- data
  propensity_data$.hds_treated <- as.integer(treated)
  formula <- stats::as.formula(paste(".hds_treated ~", rhs))
  fit <- stats::glm(
    formula,
    data = propensity_data,
    family = stats::binomial(),
    x = TRUE,
    y = TRUE,
    model = TRUE
  )

  scores <- as.numeric(stats::fitted(fit))
  if (any(!is.finite(scores)) || any(scores <= 0) || any(scores >= 1)) {
    stop(
      "Estimated propensity scores reached 0 or 1; the fitted design violates positivity",
      call. = FALSE
    )
  }

  weights <- switch(
    estimand,
    ATE = ifelse(treated, 1 / scores, 1 / (1 - scores)),
    ATT = ifelse(treated, 1, scores / (1 - scores)),
    ATO = ifelse(treated, 1 - scores, scores)
  )

  extreme_scores <- scores < positivity_threshold |
    scores > (1 - positivity_threshold)
  if (any(extreme_scores)) {
    warning(
      sprintf(
        "%d observations have propensity scores outside [%.3f, %.3f]",
        sum(extreme_scores), positivity_threshold, 1 - positivity_threshold
      ),
      call. = FALSE
    )
  }

  treated_scores <- scores[treated]
  control_scores <- scores[!treated]
  common_support <- c(
    lower = max(min(treated_scores), min(control_scores)),
    upper = min(max(treated_scores), max(control_scores))
  )
  outside_support <- if (common_support[["lower"]] <= common_support[["upper"]]) {
    scores < common_support[["lower"]] | scores > common_support[["upper"]]
  } else {
    rep(TRUE, length(scores))
  }

  design_matrix <- stats::model.matrix(fit)
  if ("(Intercept)" %in% colnames(design_matrix)) {
    design_matrix <- design_matrix[, colnames(design_matrix) != "(Intercept)", drop = FALSE]
  }

  structure(
    list(
      model = fit,
      data = data,
      treatment = as.integer(treated),
      propensity_score = scores,
      weights = as.numeric(weights),
      design_matrix = design_matrix,
      metadata = list(
        treatment = treatment,
        treated_value = treated_value,
        covariates = covariates,
        estimand = estimand,
        n = nrow(data),
        n_treated = sum(treated),
        n_control = sum(!treated),
        omitted_rows = omitted_rows,
        positivity_threshold = positivity_threshold,
        n_extreme_scores = sum(extreme_scores),
        common_support = common_support,
        n_outside_common_support = sum(outside_support),
        treated_ess = .effective_sample_size(weights[treated]),
        control_ess = .effective_sample_size(weights[!treated]),
        max_weight = max(weights)
      )
    ),
    class = "hds_propensity_design"
  )
}

#' Summarise propensity-score overlap and weight diagnostics
#'
#' @param design Object returned by [fit_propensity_design].
#'
#' @return One-row data frame containing score ranges, common support,
#'   effective sample sizes, and extreme-weight diagnostics.
#' @export
propensity_overlap <- function(design) {
  if (!inherits(design, "hds_propensity_design")) {
    stop("`design` must be created by `fit_propensity_design()`", call. = FALSE)
  }

  treated <- design$treatment == 1L
  scores <- design$propensity_score
  weights <- design$weights
  meta <- design$metadata

  data.frame(
    estimand = meta$estimand,
    treated_score_min = min(scores[treated]),
    treated_score_max = max(scores[treated]),
    control_score_min = min(scores[!treated]),
    control_score_max = max(scores[!treated]),
    common_support_lower = meta$common_support[["lower"]],
    common_support_upper = meta$common_support[["upper"]],
    outside_common_support = meta$n_outside_common_support,
    extreme_scores = meta$n_extreme_scores,
    max_weight = max(weights),
    treated_ess = .effective_sample_size(weights[treated]),
    control_ess = .effective_sample_size(weights[!treated]),
    row.names = NULL,
    check.names = FALSE
  )
}

#' Assess covariate balance for a propensity-score design
#'
#' Computes standardized mean differences (SMDs) before and after weighting.
#' Factor covariates are represented by their model-matrix indicator columns.
#' The same unweighted pooled standard deviation is used as the denominator for
#' both SMDs so weighting changes are directly comparable.
#'
#' @param design Object returned by [fit_propensity_design].
#' @param threshold Absolute weighted SMD threshold used for the `balanced`
#'   indicator. A conventional descriptive threshold is 0.1.
#'
#' @return Data frame with unweighted and weighted SMDs for each design-matrix
#'   covariate column.
#' @export
propensity_balance <- function(design, threshold = 0.1) {
  if (!inherits(design, "hds_propensity_design")) {
    stop("`design` must be created by `fit_propensity_design()`", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      !is.finite(threshold) || threshold <= 0) {
    stop("`threshold` must be a positive finite number", call. = FALSE)
  }

  x <- design$design_matrix
  treated <- design$treatment == 1L
  weights <- design$weights

  rows <- lapply(seq_len(ncol(x)), function(j) {
    values <- x[, j]
    treated_values <- values[treated]
    control_values <- values[!treated]
    treated_weights <- weights[treated]
    control_weights <- weights[!treated]

    mean_treated <- mean(treated_values)
    mean_control <- mean(control_values)
    var_treated <- stats::var(treated_values)
    var_control <- stats::var(control_values)
    pooled_sd <- sqrt((var_treated + var_control) / 2)

    weighted_mean_treated <- .weighted_mean(treated_values, treated_weights)
    weighted_mean_control <- .weighted_mean(control_values, control_weights)

    if (!is.finite(pooled_sd) || pooled_sd == 0) {
      unweighted_smd <- if (isTRUE(all.equal(mean_treated, mean_control))) 0 else NA_real_
      weighted_smd <- if (isTRUE(all.equal(
        weighted_mean_treated,
        weighted_mean_control
      ))) 0 else NA_real_
    } else {
      unweighted_smd <- (mean_treated - mean_control) / pooled_sd
      weighted_smd <- (weighted_mean_treated - weighted_mean_control) / pooled_sd
    }

    data.frame(
      term = colnames(x)[j],
      unweighted_smd = unweighted_smd,
      weighted_smd = weighted_smd,
      abs_unweighted_smd = abs(unweighted_smd),
      abs_weighted_smd = abs(weighted_smd),
      balanced = !is.na(weighted_smd) && abs(weighted_smd) <= threshold,
      row.names = NULL,
      check.names = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Estimate a weighted marginal treatment effect
#'
#' Estimates the difference in weighted outcome means for the target population
#' encoded by a propensity design. For continuous outcomes this is a mean
#' difference; for a binary 0/1 outcome it is a risk difference. Standard errors
#' use a fixed-weight Hájek variance approximation and therefore do not include
#' uncertainty from estimation of the propensity model.
#'
#' @param design Object returned by [fit_propensity_design].
#' @param outcome Name of a numeric outcome column in the retained design data.
#' @param conf_level Confidence level for the Wald interval.
#' @param na_action Outcome missing-value policy: `"fail"` or `"omit"`.
#'
#' @return One-row data frame with weighted treated/control means, marginal
#'   treatment-effect estimate, fixed-weight standard error, Wald statistic,
#'   p-value, and confidence interval.
#' @export
estimate_propensity_effect <- function(
    design,
    outcome,
    conf_level = 0.95,
    na_action = c("fail", "omit")) {
  na_action <- match.arg(na_action)
  if (!inherits(design, "hds_propensity_design")) {
    stop("`design` must be created by `fit_propensity_design()`", call. = FALSE)
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

  y <- design$data[[outcome]]
  treatment <- design$treatment
  weights <- design$weights
  missing <- is.na(y)
  if (any(missing)) {
    if (na_action == "fail") {
      stop(
        "Missing outcome values detected; use `na_action = \"omit\"` to drop them",
        call. = FALSE
      )
    }
    keep <- !missing
    y <- y[keep]
    treatment <- treatment[keep]
    weights <- weights[keep]
  }
  if (!all(is.finite(y))) {
    stop("`outcome` must contain only finite values or NA", call. = FALSE)
  }
  if (!any(treatment == 1L) || !any(treatment == 0L)) {
    stop("Both treatment groups must remain in the outcome analysis", call. = FALSE)
  }

  treated <- treatment == 1L
  mu1 <- .weighted_mean(y[treated], weights[treated])
  mu0 <- .weighted_mean(y[!treated], weights[!treated])
  estimate <- mu1 - mu0

  variance1 <- sum(
    weights[treated]^2 * (y[treated] - mu1)^2
  ) / sum(weights[treated])^2
  variance0 <- sum(
    weights[!treated]^2 * (y[!treated] - mu0)^2
  ) / sum(weights[!treated])^2
  std_error <- sqrt(variance1 + variance0)
  statistic <- estimate / std_error
  critical_value <- stats::qnorm(1 - (1 - conf_level) / 2)

  data.frame(
    estimand = design$metadata$estimand,
    outcome = outcome,
    treated_mean = mu1,
    control_mean = mu0,
    estimate = estimate,
    std_error = std_error,
    statistic = statistic,
    p_value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
    conf_low = estimate - critical_value * std_error,
    conf_high = estimate + critical_value * std_error,
    variance_assumption = "propensity weights treated as fixed",
    row.names = NULL,
    check.names = FALSE
  )
}
