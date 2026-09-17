# ------------------------------------------------------------------------------
# File: advanced_survival.R
# Purpose: Advanced survival-analysis helpers.
# ------------------------------------------------------------------------------

#' Fit a Fine-Gray competing-risks model
#'
#' Fits a Fine-Gray subdistribution hazards model via [cmprsk::crr]. Event and
#' censoring codes are explicit so status variables do not need to use the
#' conventional 0/1/2 coding. Covariates must be numeric; categorical predictors
#' should be expanded with [stats::model.matrix] before fitting.
#'
#' @param time Numeric follow-up times.
#' @param status Numeric event-status codes.
#' @param covariates Numeric vector, matrix, or data frame of model covariates.
#' @param event_code Status code for the event of interest.
#' @param censor_code Status code for censoring.
#' @param na_action How to handle rows with missing values: `"fail"` or `"omit"`.
#'
#' @return A fitted `crr` object. The `healthdatascience` attribute stores event
#'   coding, sample counts, competing-event codes, and omitted-row information.
#' @export
fit_competing_risks <- function(
    time,
    status,
    covariates,
    event_code = 1,
    censor_code = 0,
    na_action = c("fail", "omit")) {
  na_action <- match.arg(na_action)

  if (!is.numeric(time)) stop("`time` must be numeric", call. = FALSE)
  if (!is.numeric(status)) stop("`status` must be numeric", call. = FALSE)
  if (length(time) != length(status)) stop("`time` and `status` must have the same length", call. = FALSE)
  if (length(time) == 0L) stop("At least one observation is required", call. = FALSE)
  if (any(!is.finite(time), na.rm = TRUE)) stop("`time` must contain only finite values or NA", call. = FALSE)
  if (any(time < 0, na.rm = TRUE)) stop("`time` cannot contain negative values", call. = FALSE)
  if (any(!is.finite(status), na.rm = TRUE)) stop("`status` must contain only finite values or NA", call. = FALSE)

  valid_code <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
  if (!valid_code(event_code) || !valid_code(censor_code)) {
    stop("Event and censoring codes must be finite numeric scalars", call. = FALSE)
  }
  if (event_code == censor_code) stop("`event_code` and `censor_code` must be different", call. = FALSE)

  if (is.numeric(covariates) && is.null(dim(covariates))) {
    covariates <- matrix(covariates, ncol = 1L)
    colnames(covariates) <- "covariate"
  } else if (is.data.frame(covariates)) {
    numeric_columns <- vapply(covariates, is.numeric, logical(1))
    if (!all(numeric_columns)) stop("All covariate columns must be numeric", call. = FALSE)
    covariates <- as.matrix(covariates)
  } else if (!is.matrix(covariates) || !is.numeric(covariates)) {
    stop("`covariates` must be a numeric vector, matrix, or data frame", call. = FALSE)
  }

  if (nrow(covariates) != length(time)) stop("Covariates must have one row per observation", call. = FALSE)
  if (ncol(covariates) == 0L) stop("At least one covariate is required", call. = FALSE)
  if (any(!is.finite(covariates), na.rm = TRUE)) stop("Covariates must contain only finite values or NA", call. = FALSE)
  if (is.null(colnames(covariates))) colnames(covariates) <- paste0("covariate_", seq_len(ncol(covariates)))

  missing_rows <- is.na(time) | is.na(status) | !stats::complete.cases(covariates)
  omitted_rows <- sum(missing_rows)
  if (omitted_rows > 0L) {
    if (na_action == "fail") {
      stop("Missing values detected; use `na_action = \"omit\"` to drop rows", call. = FALSE)
    }
    keep <- !missing_rows
    time <- time[keep]
    status <- status[keep]
    covariates <- covariates[keep, , drop = FALSE]
  }

  if (length(time) == 0L) stop("No complete observations remain after missing-value handling", call. = FALSE)
  if (!event_code %in% status) stop("`event_code` is not observed in `status`", call. = FALSE)

  competing_codes <- sort(setdiff(unique(status), c(censor_code, event_code)))
  if (length(competing_codes) == 0L) warning("No competing-event code is present in `status`", call. = FALSE)

  ensure_packages("cmprsk")
  fit <- cmprsk::crr(
    ftime = time,
    fstatus = status,
    cov1 = covariates,
    failcode = event_code,
    cencode = censor_code
  )

  attr(fit, "healthdatascience") <- list(
    event_code = event_code,
    censor_code = censor_code,
    competing_codes = competing_codes,
    n = length(time),
    event_count = sum(status == event_code),
    competing_event_count = sum(status %in% competing_codes),
    censored_count = sum(status == censor_code),
    omitted_rows = omitted_rows,
    covariates = colnames(covariates)
  )
  fit
}

#' Summarise a Fine-Gray competing-risks model
#'
#' @param model A fitted `crr` object.
#' @param conf_level Confidence level for Wald intervals.
#' @return Data frame of coefficient summaries.
#' @export
tidy_competing_risks <- function(model, conf_level = 0.95) {
  if (!inherits(model, "crr")) stop("`model` must inherit from `crr`", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a number strictly between 0 and 1", call. = FALSE)
  }

  coefficients <- model$coef
  if (is.null(coefficients) || length(coefficients) == 0L) stop("The fitted model contains no coefficients", call. = FALSE)
  if (is.null(model$var)) stop("The fitted model does not contain a variance matrix", call. = FALSE)

  std_error <- sqrt(diag(model$var))
  statistic <- coefficients / std_error
  critical_value <- stats::qnorm(1 - (1 - conf_level) / 2)
  terms <- names(coefficients)
  if (is.null(terms) || any(!nzchar(terms))) terms <- paste0("covariate_", seq_along(coefficients))

  data.frame(
    term = terms,
    estimate = unname(coefficients),
    std_error = unname(std_error),
    statistic = unname(statistic),
    p_value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
    subdistribution_hazard_ratio = exp(unname(coefficients)),
    conf_low = exp(unname(coefficients) - critical_value * unname(std_error)),
    conf_high = exp(unname(coefficients) + critical_value * unname(std_error)),
    row.names = NULL,
    check.names = FALSE
  )
}

#' Time-varying coefficient Cox model
#' @param formula Cox model formula using `tt()` for terms.
#' @param data Data frame.
#' @return Fitted coxph object.
#' @export
fit_time_varying_cox <- function(formula, data) {
  ensure_packages("survival")
  survival::coxph(formula, data = data)
}

#' Frailty Cox model
#' @param formula Cox model formula with `frailty()`.
#' @param data Data frame.
#' @return Fitted coxph object.
#' @export
fit_frailty_cox <- function(formula, data) {
  ensure_packages("survival")
  survival::coxph(formula, data = data)
}

#' Landmark analysis
#' @param formula Cox model formula.
#' @param data Data frame with `time` variable.
#' @param landmark Landmark time point.
#' @return Fitted coxph object on truncated data.
#' @export
landmark_cox <- function(formula, data, landmark) {
  ensure_packages("survival")
  d <- data[data$time >= landmark, ]
  survival::coxph(formula, data = d)
}
