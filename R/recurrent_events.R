# ------------------------------------------------------------------------------
# File: recurrent_events.R
# Purpose: Recurrent-event survival models for counting-process data.
# ------------------------------------------------------------------------------

#' Fit a recurrent-event Cox model
#'
#' Fits recurrent-event models to start-stop counting-process data using
#' [survival::coxph]. Andersen-Gill uses a common baseline hazard with robust
#' subject-level clustering. PWP total-time stratifies the baseline hazard by
#' event order and also uses subject-level clustering.
#'
#' @param data Data frame containing counting-process intervals.
#' @param start Name of the interval-start column.
#' @param stop Name of the interval-stop column.
#' @param event Name of the binary event indicator column.
#' @param id Name of the subject identifier column.
#' @param covariates Character vector of covariate column names.
#' @param model Recurrent-event model: `"andersen-gill"` or `"pwp-total-time"`.
#' @param event_order Name of the event-order column required for PWP total-time.
#' @param na_action Missing-value policy: `"fail"` or `"omit"`.
#'
#' @return A fitted `coxph` object with a `healthdatascience` attribute describing
#'   the recurrent-event specification and fitted sample.
#' @export
fit_recurrent_events <- function(
    data,
    start,
    stop,
    event,
    id,
    covariates = character(),
    model = c("andersen-gill", "pwp-total-time"),
    event_order = NULL,
    na_action = c("fail", "omit")) {
  model <- match.arg(model)
  na_action <- match.arg(na_action)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }

  scalar_name <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }
  required_names <- list(start = start, stop = stop, event = event, id = id)
  if (!all(vapply(required_names, scalar_name, logical(1)))) {
    stop("`start`, `stop`, `event`, and `id` must be single column names", call. = FALSE)
  }
  if (!is.character(covariates) || anyNA(covariates) || any(!nzchar(covariates))) {
    stop("`covariates` must be a character vector of column names", call. = FALSE)
  }
  if (anyDuplicated(covariates)) {
    stop("`covariates` cannot contain duplicate column names", call. = FALSE)
  }

  required <- unique(c(start, stop, event, id, covariates))
  if (model == "pwp-total-time") {
    if (!scalar_name(event_order)) {
      stop("`event_order` is required for PWP total-time models", call. = FALSE)
    }
    required <- unique(c(required, event_order))
  }

  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      paste("Missing required columns:", paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }

  if (!is.numeric(data[[start]]) || !is.numeric(data[[stop]])) {
    stop("Start and stop times must be numeric", call. = FALSE)
  }
  if (!is.numeric(data[[event]]) && !is.logical(data[[event]])) {
    stop("The event indicator must be numeric or logical", call. = FALSE)
  }

  event_values <- unique(data[[event]][!is.na(data[[event]])])
  if (!all(event_values %in% c(0, 1, FALSE, TRUE))) {
    stop("The event indicator must contain only 0 and 1", call. = FALSE)
  }

  if (any(!is.finite(data[[start]]), na.rm = TRUE) ||
      any(!is.finite(data[[stop]]), na.rm = TRUE)) {
    stop("Start and stop times must contain only finite values or NA", call. = FALSE)
  }
  if (any(data[[start]] < 0, na.rm = TRUE)) {
    stop("Start times cannot be negative", call. = FALSE)
  }
  invalid_intervals <- !is.na(data[[start]]) & !is.na(data[[stop]]) &
    data[[stop]] <= data[[start]]
  if (any(invalid_intervals)) {
    stop("Every interval must satisfy `stop > start`", call. = FALSE)
  }

  if (anyNA(data[[id]])) {
    stop("Subject identifiers cannot be missing", call. = FALSE)
  }

  if (length(covariates) > 0L) {
    unsupported <- vapply(
      data[covariates],
      function(x) !(is.numeric(x) || is.logical(x) || is.factor(x)),
      logical(1)
    )
    if (any(unsupported)) {
      stop(
        "Covariates must be numeric, logical, or factor variables",
        call. = FALSE
      )
    }
  }

  if (model == "pwp-total-time") {
    order_values <- data[[event_order]]
    if (!is.numeric(order_values)) {
      stop("`event_order` must be numeric", call. = FALSE)
    }
    nonmissing_order <- order_values[!is.na(order_values)]
    if (any(nonmissing_order < 1) || any(nonmissing_order != floor(nonmissing_order))) {
      stop("`event_order` must contain positive integers", call. = FALSE)
    }
  }

  model_columns <- required
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

  subject_intervals <- split(
    data.frame(start = data[[start]], stop = data[[stop]]),
    data[[id]]
  )
  overlap <- vapply(subject_intervals, function(x) {
    x <- x[order(x$start, x$stop), , drop = FALSE]
    nrow(x) > 1L && any(x$start[-1L] < x$stop[-nrow(x)])
  }, logical(1))
  if (any(overlap)) {
    stop("Intervals cannot overlap within a subject", call. = FALSE)
  }

  if (sum(as.integer(data[[event]])) == 0L) {
    stop("At least one recurrent event is required", call. = FALSE)
  }

  quote_name <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
  response <- paste0(
    "survival::Surv(", quote_name(start), ", ", quote_name(stop), ", ",
    quote_name(event), ")"
  )
  rhs <- if (length(covariates) > 0L) {
    paste(vapply(covariates, quote_name, character(1)), collapse = " + ")
  } else {
    "1"
  }

  if (model == "pwp-total-time") {
    rhs <- paste0(rhs, " + strata(", quote_name(event_order), ")")
  }
  rhs <- paste0(rhs, " + cluster(", quote_name(id), ")")
  formula <- stats::as.formula(paste(response, "~", rhs))

  ensure_packages("survival")
  fit <- survival::coxph(formula, data = data, ties = "efron", model = TRUE)

  attr(fit, "healthdatascience") <- list(
    model = model,
    n_rows = nrow(data),
    n_subjects = length(unique(data[[id]])),
    event_count = sum(as.integer(data[[event]])),
    omitted_rows = omitted_rows,
    start = start,
    stop = stop,
    event = event,
    id = id,
    event_order = if (model == "pwp-total-time") event_order else NULL,
    covariates = covariates
  )
  fit
}

#' Summarise a recurrent-event Cox model
#'
#' Produces a compact Wald table from a recurrent-event `coxph` model. Because
#' the fitted models use subject-level clustering, the variance matrix used here
#' is the robust covariance matrix stored by [survival::coxph].
#'
#' @param model A recurrent-event model returned by [fit_recurrent_events].
#' @param conf_level Confidence level for Wald intervals.
#'
#' @return Data frame with log hazard ratios, robust standard errors, z tests,
#'   hazard ratios, and confidence limits.
#' @export
tidy_recurrent_events <- function(model, conf_level = 0.95) {
  if (!inherits(model, "coxph")) {
    stop("`model` must inherit from `coxph`", call. = FALSE)
  }
  metadata <- attr(model, "healthdatascience")
  if (is.null(metadata) || is.null(metadata$model) ||
      !metadata$model %in% c("andersen-gill", "pwp-total-time")) {
    stop("`model` was not created by `fit_recurrent_events()`", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a number strictly between 0 and 1", call. = FALSE)
  }

  coefficients <- stats::coef(model)
  if (length(coefficients) == 0L) {
    return(data.frame(
      term = character(), estimate = numeric(), std_error = numeric(),
      statistic = numeric(), p_value = numeric(), hazard_ratio = numeric(),
      conf_low = numeric(), conf_high = numeric()
    ))
  }

  std_error <- sqrt(diag(stats::vcov(model)))
  statistic <- coefficients / std_error
  critical_value <- stats::qnorm(1 - (1 - conf_level) / 2)

  data.frame(
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
