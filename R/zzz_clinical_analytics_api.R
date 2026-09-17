# ------------------------------------------------------------------------------
# File: zzz_clinical_analytics_api.R
# Purpose: Validated pharmacovigilance and clinical prediction APIs.
# ------------------------------------------------------------------------------

.validate_pv_counts <- function(a, b, c, d) {
  values <- c(a = a, b = b, c = c, d = d)
  if (!is.numeric(values) || length(values) != 4L || anyNA(values) ||
      any(!is.finite(values)) || any(values < 0) || any(values != floor(values))) {
    stop("`a`, `b`, `c`, and `d` must be non-negative finite integer counts", call. = FALSE)
  }
  if ((a + b) == 0 || (c + d) == 0) {
    stop("Each exposure row must contain at least one report", call. = FALSE)
  }
  if ((a + c) == 0 || (b + d) == 0) {
    stop("Each event column must contain at least one report", call. = FALSE)
  }
  values
}

#' Detailed pharmacovigilance disproportionality analysis
#'
#' Computes the proportional reporting ratio (PRR), reporting odds ratio (ROR),
#' approximate log-scale confidence intervals, and Pearson chi-square statistic
#' for a 2 x 2 spontaneous-reporting table.
#'
#' A Haldane-Anscombe correction is applied to all four cells only when at least
#' one observed cell is zero. The common Evans-style rule (`a >= 3`, PRR >= 2,
#' chi-square >= 4) is returned as a descriptive screening flag, not as proof of
#' causality or a universal regulatory decision rule.
#'
#' @param a Reports containing both the exposure/drug and event.
#' @param b Reports containing the exposure/drug without the event.
#' @param c Reports containing the event without the exposure/drug.
#' @param d Reports containing neither the exposure/drug nor event.
#' @param conf_level Confidence level for approximate PRR/ROR intervals.
#' @param zero_cell_correction Positive correction added to all cells when any
#'   observed cell is zero.
#'
#' @return One-row data frame with disproportionality metrics and screening flag.
#' @export
pharmacovigilance_signal <- function(
    a,
    b,
    c,
    d,
    conf_level = 0.95,
    zero_cell_correction = 0.5) {
  .validate_pv_counts(a, b, c, d)
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be strictly between 0 and 1", call. = FALSE)
  }
  if (!is.numeric(zero_cell_correction) || length(zero_cell_correction) != 1L ||
      !is.finite(zero_cell_correction) || zero_cell_correction <= 0) {
    stop("`zero_cell_correction` must be a positive finite number", call. = FALSE)
  }

  observed <- c(a = a, b = b, c = c, d = d)
  corrected <- any(observed == 0)
  cells <- if (corrected) observed + zero_cell_correction else observed
  aa <- cells[["a"]]
  bb <- cells[["b"]]
  cc <- cells[["c"]]
  dd <- cells[["d"]]

  prr <- (aa / (aa + bb)) / (cc / (cc + dd))
  ror <- (aa * dd) / (bb * cc)

  prr_se_log <- sqrt(
    1 / aa - 1 / (aa + bb) + 1 / cc - 1 / (cc + dd)
  )
  ror_se_log <- sqrt(1 / aa + 1 / bb + 1 / cc + 1 / dd)
  critical <- stats::qnorm(1 - (1 - conf_level) / 2)

  table <- matrix(observed, nrow = 2L, byrow = TRUE)
  chi_square <- unname(stats::chisq.test(table, correct = FALSE)$statistic)
  chi_square_p <- unname(stats::chisq.test(table, correct = FALSE)$p.value)

  data.frame(
    a = a,
    b = b,
    c = c,
    d = d,
    prr = prr,
    prr_conf_low = exp(log(prr) - critical * prr_se_log),
    prr_conf_high = exp(log(prr) + critical * prr_se_log),
    ror = ror,
    ror_conf_low = exp(log(ror) - critical * ror_se_log),
    ror_conf_high = exp(log(ror) + critical * ror_se_log),
    chi_square = chi_square,
    chi_square_p_value = chi_square_p,
    corrected_zero_cells = corrected,
    zero_cell_correction = if (corrected) zero_cell_correction else 0,
    evans_screening_flag = (a >= 3 && prr >= 2 && chi_square >= 4),
    row.names = NULL,
    check.names = FALSE
  )
}

#' Pharmacovigilance proportional reporting ratio
#'
#' Backward-compatible scalar PRR helper. For uncertainty, ROR, zero-cell
#' handling, and screening diagnostics use [pharmacovigilance_signal].
#'
#' @param a Count of drug and event.
#' @param b Count of drug without event.
#' @param c Count of event without drug.
#' @param d Count of neither drug nor event.
#' @return Numeric PRR value.
#' @export
detect_pharmacovigilance <- function(a, b, c, d) {
  .validate_pv_counts(a, b, c, d)
  (a / (a + b)) / (c / (c + d))
}

.hds_binary_outcome <- function(y) {
  if (is.logical(y)) {
    return(list(y = as.integer(y), event_level = "TRUE"))
  }
  if (is.factor(y)) {
    if (nlevels(y) != 2L) {
      stop("The clinical prediction outcome factor must have exactly two levels", call. = FALSE)
    }
    return(list(y = as.integer(y == levels(y)[2L]), event_level = levels(y)[2L]))
  }
  if (is.numeric(y)) {
    observed <- sort(unique(y))
    if (!identical(observed, c(0, 1)) && !identical(observed, c(0L, 1L))) {
      stop("Numeric clinical prediction outcomes must be coded 0/1", call. = FALSE)
    }
    return(list(y = as.integer(y), event_level = "1"))
  }
  stop("Clinical prediction outcomes must be logical, a two-level factor, or numeric 0/1", call. = FALSE)
}

.hds_auc <- function(y, probability) {
  positives <- sum(y == 1L)
  negatives <- sum(y == 0L)
  if (positives == 0L || negatives == 0L) return(NA_real_)
  ranks <- rank(probability, ties.method = "average")
  (sum(ranks[y == 1L]) - positives * (positives + 1) / 2) /
    (positives * negatives)
}

.safe_rate <- function(numerator, denominator) {
  if (denominator == 0) NA_real_ else numerator / denominator
}

#' Fit a binary clinical prediction rule
#'
#' Fits a logistic regression and retains the full fitted model together with
#' transparent apparent-performance summaries. These metrics are evaluated on
#' the development sample and are not a substitute for internal or external
#' validation.
#'
#' @param formula Binary-outcome logistic-regression formula.
#' @param data Data frame.
#' @param threshold Probability threshold used for apparent classification
#'   sensitivity/specificity summaries.
#' @param na_action Missing-value policy: `"fail"` or `"omit"`.
#'
#' @return Object of class `hds_clinical_prediction_rule`.
#' @export
fit_clinical_prediction_rule <- function(
    formula,
    data,
    threshold = 0.5,
    na_action = c("fail", "omit")) {
  na_action <- match.arg(na_action)
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a model formula", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      !is.finite(threshold) || threshold <= 0 || threshold >= 1) {
    stop("`threshold` must be strictly between 0 and 1", call. = FALSE)
  }

  required <- all.vars(formula)
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      paste("Missing required columns:", paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }
  missing_rows <- !stats::complete.cases(data[required])
  omitted_rows <- sum(missing_rows)
  if (omitted_rows > 0L) {
    if (na_action == "fail") {
      stop("Missing values detected; use `na_action = \"omit\"` to drop rows", call. = FALSE)
    }
    data <- data[!missing_rows, , drop = FALSE]
  }
  if (nrow(data) == 0L) {
    stop("No complete observations remain after missing-value handling", call. = FALSE)
  }

  outcome_name <- all.vars(formula[[2L]])
  if (length(outcome_name) != 1L) {
    stop("The prediction formula must have one observed binary outcome", call. = FALSE)
  }
  encoded <- .hds_binary_outcome(data[[outcome_name]])
  if (length(unique(encoded$y)) != 2L) {
    stop("Both outcome classes are required in the analysis sample", call. = FALSE)
  }

  fit <- stats::glm(
    formula,
    data = data,
    family = stats::binomial(),
    na.action = stats::na.fail,
    model = TRUE,
    x = TRUE,
    y = TRUE
  )
  probability <- as.numeric(stats::fitted(fit))
  if (any(!is.finite(stats::coef(fit)))) {
    warning("The logistic model contains non-finite coefficients; separation may be present", call. = FALSE)
  }

  predicted <- as.integer(probability >= threshold)
  y <- encoded$y
  tp <- sum(predicted == 1L & y == 1L)
  tn <- sum(predicted == 0L & y == 0L)
  fp <- sum(predicted == 1L & y == 0L)
  fn <- sum(predicted == 0L & y == 1L)

  performance <- data.frame(
    n = length(y),
    events = sum(y == 1L),
    event_rate = mean(y),
    auc = .hds_auc(y, probability),
    brier_score = mean((y - probability)^2),
    mean_predicted_risk = mean(probability),
    calibration_mean_error = mean(probability) - mean(y),
    threshold = threshold,
    sensitivity = .safe_rate(tp, tp + fn),
    specificity = .safe_rate(tn, tn + fp),
    positive_predictive_value = .safe_rate(tp, tp + fp),
    negative_predictive_value = .safe_rate(tn, tn + fn),
    row.names = NULL,
    check.names = FALSE
  )

  structure(
    list(
      model = fit,
      performance = performance,
      metadata = list(
        outcome = outcome_name,
        event_level = encoded$event_level,
        threshold = threshold,
        n = length(y),
        omitted_rows = omitted_rows,
        assessment = "apparent development-sample performance"
      )
    ),
    class = "hds_clinical_prediction_rule"
  )
}

#' Tidy a clinical prediction rule
#'
#' @param model Object returned by [fit_clinical_prediction_rule].
#' @param conf_level Confidence level for coefficient intervals.
#' @return Data frame of logistic-regression coefficients and odds ratios.
#' @export
tidy_clinical_prediction_rule <- function(model, conf_level = 0.95) {
  if (!inherits(model, "hds_clinical_prediction_rule")) {
    stop("`model` must be created by `fit_clinical_prediction_rule()`", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be strictly between 0 and 1", call. = FALSE)
  }

  fit <- model$model
  coefficients <- stats::coef(fit)
  covariance <- stats::vcov(fit)
  std_error <- sqrt(diag(covariance))
  statistic <- coefficients / std_error
  critical <- stats::qnorm(1 - (1 - conf_level) / 2)

  data.frame(
    term = names(coefficients),
    estimate = unname(coefficients),
    std_error = unname(std_error),
    statistic = unname(statistic),
    p_value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
    odds_ratio = exp(unname(coefficients)),
    conf_low = exp(unname(coefficients) - critical * unname(std_error)),
    conf_high = exp(unname(coefficients) + critical * unname(std_error)),
    row.names = NULL,
    check.names = FALSE
  )
}

#' Apparent performance of a clinical prediction rule
#'
#' @param model Object returned by [fit_clinical_prediction_rule].
#' @return One-row data frame of apparent development-sample performance metrics.
#' @export
clinical_prediction_performance <- function(model) {
  if (!inherits(model, "hds_clinical_prediction_rule")) {
    stop("`model` must be created by `fit_clinical_prediction_rule()`", call. = FALSE)
  }
  model$performance
}

#' Develop a clinical prediction rule
#'
#' Backward-compatible coefficient-vector helper. New analyses should prefer
#' [fit_clinical_prediction_rule] so the fitted model and performance diagnostics
#' are retained.
#'
#' @param formula Formula for logistic regression.
#' @param data Data frame.
#' @return Named coefficient vector.
#' @export
develop_clinical_prediction_rule <- function(formula, data) {
  rule <- fit_clinical_prediction_rule(
    formula = formula,
    data = data,
    na_action = "omit"
  )
  stats::coef(rule$model)
}
