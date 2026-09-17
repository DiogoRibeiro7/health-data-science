# ------------------------------------------------------------------------------
# File: zzz_advanced_survival_api.R
# Purpose: Hardened public APIs for advanced Cox survival models.
# ------------------------------------------------------------------------------

.hds_prepare_cox_data <- function(formula, data, na_action, extra_required = character()) {
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a model formula", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }

  required <- unique(c(all.vars(formula), extra_required))
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

  list(data = data, omitted_rows = omitted_rows)
}

.hds_validate_cox_ties <- function(ties) {
  match.arg(ties, c("efron", "breslow", "exact"))
}

#' Fit a Cox model with time-varying coefficients
#'
#' Fits a Cox model containing at least one `tt()` term. Missing-value handling
#' and tie handling are explicit, and fitted-sample metadata are attached to the
#' returned model.
#'
#' @param formula Cox formula containing one or more `tt()` terms.
#' @param data Data frame.
#' @param tt Optional time-transform function or list of functions passed to
#'   [survival::coxph].
#' @param ties Tie-handling method.
#' @param na_action Missing-value policy: `"fail"` or `"omit"`.
#' @return Fitted `coxph` object with `healthdatascience` metadata.
#' @export
fit_time_varying_cox <- function(
    formula,
    data,
    tt = NULL,
    ties = c("efron", "breslow", "exact"),
    na_action = c("fail", "omit")) {
  ties <- .hds_validate_cox_ties(ties)
  na_action <- match.arg(na_action)
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a model formula", call. = FALSE)
  }
  formula_text <- paste(deparse(formula), collapse = " ")
  if (!grepl("tt\\s*\\(", formula_text)) {
    stop("`formula` must contain at least one `tt()` term", call. = FALSE)
  }
  if (!is.null(tt) && !(is.function(tt) || is.list(tt))) {
    stop("`tt` must be NULL, a function, or a list of functions", call. = FALSE)
  }

  prepared <- .hds_prepare_cox_data(formula, data, na_action)
  ensure_packages("survival")
  args <- list(
    formula = formula,
    data = prepared$data,
    ties = ties,
    na.action = stats::na.fail,
    x = TRUE
  )
  if (!is.null(tt)) args$tt <- tt
  fit <- do.call(survival::coxph, args)

  attr(fit, "healthdatascience") <- list(
    model_type = "time-varying-cox",
    n = fit$n,
    event_count = fit$nevent,
    omitted_rows = prepared$omitted_rows,
    ties = ties,
    tt_supplied = !is.null(tt),
    formula = formula_text
  )
  fit
}

#' Fit a frailty Cox model
#'
#' Fits a Cox proportional-hazards model containing at least one `frailty()`
#' term, with explicit missing-value and tie handling.
#'
#' @param formula Cox formula containing a `frailty()` term.
#' @param data Data frame.
#' @param ties Tie-handling method.
#' @param na_action Missing-value policy: `"fail"` or `"omit"`.
#' @return Fitted `coxph` object with `healthdatascience` metadata.
#' @export
fit_frailty_cox <- function(
    formula,
    data,
    ties = c("efron", "breslow", "exact"),
    na_action = c("fail", "omit")) {
  ties <- .hds_validate_cox_ties(ties)
  na_action <- match.arg(na_action)
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a model formula", call. = FALSE)
  }
  formula_text <- paste(deparse(formula), collapse = " ")
  if (!grepl("frailty\\s*\\(", formula_text)) {
    stop("`formula` must contain at least one `frailty()` term", call. = FALSE)
  }

  prepared <- .hds_prepare_cox_data(formula, data, na_action)
  ensure_packages("survival")
  fit <- survival::coxph(
    formula,
    data = prepared$data,
    ties = ties,
    na.action = stats::na.fail,
    x = TRUE
  )

  attr(fit, "healthdatascience") <- list(
    model_type = "frailty-cox",
    n = fit$n,
    event_count = fit$nevent,
    omitted_rows = prepared$omitted_rows,
    ties = ties,
    formula = formula_text
  )
  fit
}

#' Fit a landmark Cox model
#'
#' Restricts a right-censored survival cohort to subjects still under observation
#' at a prespecified landmark and fits a Cox model conditional on survival to that
#' landmark. By default follow-up time is reset to zero at the landmark.
#'
#' Counting-process responses are intentionally rejected because their risk-set
#' construction requires explicit start-stop handling.
#'
#' @param formula Cox model formula with a two-argument `Surv(time, status)`
#'   response.
#' @param data Data frame.
#' @param landmark Non-negative landmark time.
#' @param time Name of the follow-up-time column. Defaults to `"time"` for
#'   backward compatibility.
#' @param reset_time Logical; subtract `landmark` from retained follow-up times.
#' @param ties Tie-handling method.
#' @param na_action Missing-value policy: `"fail"` or `"omit"`.
#' @return Fitted `coxph` object with landmark metadata.
#' @export
landmark_cox <- function(
    formula,
    data,
    landmark,
    time = "time",
    reset_time = TRUE,
    ties = c("efron", "breslow", "exact"),
    na_action = c("fail", "omit")) {
  ties <- .hds_validate_cox_ties(ties)
  na_action <- match.arg(na_action)
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a model formula", call. = FALSE)
  }
  if (!is.character(time) || length(time) != 1L || is.na(time) || !nzchar(time)) {
    stop("`time` must be one column name", call. = FALSE)
  }
  if (!is.numeric(landmark) || length(landmark) != 1L ||
      !is.finite(landmark) || landmark < 0) {
    stop("`landmark` must be a non-negative finite number", call. = FALSE)
  }
  if (!is.logical(reset_time) || length(reset_time) != 1L || is.na(reset_time)) {
    stop("`reset_time` must be TRUE or FALSE", call. = FALSE)
  }

  response <- formula[[2L]]
  if (!is.call(response)) {
    stop("`formula` must have a `Surv()` response", call. = FALSE)
  }
  response_name <- paste(deparse(response[[1L]]), collapse = "")
  if (!grepl("(^|::)Surv$", response_name) || length(response) != 3L) {
    stop(
      "`landmark_cox()` requires a two-argument `Surv(time, status)` response",
      call. = FALSE
    )
  }
  response_time <- gsub("`", "", paste(deparse(response[[2L]]), collapse = ""), fixed = TRUE)
  if (response_time != time) {
    stop("`time` must match the first argument of the `Surv()` response", call. = FALSE)
  }

  prepared <- .hds_prepare_cox_data(formula, data, na_action, extra_required = time)
  if (!is.numeric(prepared$data[[time]]) ||
      any(!is.finite(prepared$data[[time]])) ||
      any(prepared$data[[time]] < 0)) {
    stop("The landmark time column must contain non-negative finite numbers", call. = FALSE)
  }

  n_before <- nrow(prepared$data)
  at_risk <- prepared$data[[time]] >= landmark
  landmark_data <- prepared$data[at_risk, , drop = FALSE]
  if (nrow(landmark_data) == 0L) {
    stop("No subjects remain under observation at the landmark", call. = FALSE)
  }
  if (reset_time) {
    landmark_data[[time]] <- landmark_data[[time]] - landmark
  }

  ensure_packages("survival")
  fit <- survival::coxph(
    formula,
    data = landmark_data,
    ties = ties,
    na.action = stats::na.fail,
    x = TRUE,
    model = TRUE
  )

  attr(fit, "healthdatascience") <- list(
    model_type = "landmark-cox",
    landmark = landmark,
    time_column = time,
    time_reset = reset_time,
    n_before_landmark = n_before,
    n_at_landmark = nrow(landmark_data),
    excluded_before_landmark = sum(!at_risk),
    event_count = fit$nevent,
    omitted_rows = prepared$omitted_rows,
    ties = ties,
    formula = paste(deparse(formula), collapse = " ")
  )
  fit
}

#' Tidy fixed-effect coefficients from a Cox model
#'
#' Produces a common Wald summary for `coxph` models, including models returned by
#' [fit_time_varying_cox], [fit_frailty_cox], and [landmark_cox].
#'
#' @param model A fitted object inheriting from `coxph`.
#' @param conf_level Confidence level for Wald intervals.
#' @return Data frame with log-hazard coefficients, standard errors, z statistics,
#'   p-values, hazard ratios, confidence limits, and model type.
#' @export
tidy_cox_model <- function(model, conf_level = 0.95) {
  if (!inherits(model, "coxph")) {
    stop("`model` must inherit from `coxph`", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be strictly between 0 and 1", call. = FALSE)
  }

  coefficients <- stats::coef(model)
  if (is.null(coefficients) || length(coefficients) == 0L) {
    stop("The fitted Cox model contains no fixed-effect coefficients", call. = FALSE)
  }
  covariance <- stats::vcov(model)
  std_error <- sqrt(diag(covariance))
  statistic <- coefficients / std_error
  critical_value <- stats::qnorm(1 - (1 - conf_level) / 2)
  metadata <- attr(model, "healthdatascience")
  model_type <- if (!is.null(metadata$model_type)) metadata$model_type else "coxph"

  data.frame(
    model_type = model_type,
    term = names(coefficients),
    estimate = unname(coefficients),
    std_error = unname(std_error),
    statistic = unname(statistic),
    p_value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
    hazard_ratio = exp(unname(coefficients)),
    conf_low = exp(unname(coefficients) - critical_value * unname(std_error)),
    conf_high = exp(unname(coefficients) + critical_value * unname(std_error)),
    row.names = NULL,
    check.names = FALSE
  )
}
