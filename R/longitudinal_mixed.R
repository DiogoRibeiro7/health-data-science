# ------------------------------------------------------------------------------
# File: longitudinal_mixed.R
# Purpose: Linear mixed-effects models for longitudinal continuous outcomes.
# ------------------------------------------------------------------------------

#' Fit a longitudinal linear mixed-effects model
#'
#' Fits a Gaussian linear mixed-effects model to repeated measurements using
#' [lme4::lmer]. The fixed-effects trajectory contains time plus optional
#' covariates. Random effects can contain a subject-specific intercept only or
#' both intercept and time slope.
#'
#' @param data Data frame containing repeated measurements.
#' @param outcome Name of the continuous outcome column.
#' @param time Name of the numeric time column.
#' @param id Name of the subject identifier column.
#' @param covariates Character vector of additional fixed-effect covariates.
#' @param random Random-effects structure: `"intercept"` or
#'   `"intercept-slope"`.
#' @param correlated Whether random intercept and slope are correlated when
#'   `random = "intercept-slope"`.
#' @param estimation Estimation method: restricted maximum likelihood (`"REML"`)
#'   or maximum likelihood (`"ML"`).
#' @param na_action Missing-value policy: `"fail"` or `"omit"`.
#' @param singular_tol Tolerance passed to [lme4::isSingular].
#'
#' @return An object of class `hds_longitudinal_mixed` containing the fitted
#'   `lmerMod` object and model metadata, including singularity and convergence
#'   diagnostics.
#' @export
fit_longitudinal_mixed <- function(
    data,
    outcome,
    time,
    id,
    covariates = character(),
    random = c("intercept", "intercept-slope"),
    correlated = TRUE,
    estimation = c("REML", "ML"),
    na_action = c("fail", "omit"),
    singular_tol = 1e-4) {
  random <- match.arg(random)
  estimation <- match.arg(estimation)
  na_action <- match.arg(na_action)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }

  scalar_name <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }
  core_names <- list(outcome = outcome, time = time, id = id)
  if (!all(vapply(core_names, scalar_name, logical(1)))) {
    stop("`outcome`, `time`, and `id` must be single column names", call. = FALSE)
  }
  if (!is.character(covariates) || anyNA(covariates) || any(!nzchar(covariates))) {
    stop("`covariates` must be a character vector of column names", call. = FALSE)
  }
  if (anyDuplicated(covariates)) {
    stop("`covariates` cannot contain duplicate column names", call. = FALSE)
  }
  if (any(covariates %in% c(outcome, time, id))) {
    stop("`covariates` cannot repeat outcome, time, or subject ID columns", call. = FALSE)
  }
  if (!is.logical(correlated) || length(correlated) != 1L || is.na(correlated)) {
    stop("`correlated` must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.numeric(singular_tol) || length(singular_tol) != 1L ||
      !is.finite(singular_tol) || singular_tol <= 0) {
    stop("`singular_tol` must be a positive finite number", call. = FALSE)
  }

  required <- unique(c(outcome, time, id, covariates))
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      paste("Missing required columns:", paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }

  if (!is.numeric(data[[outcome]])) {
    stop("The longitudinal outcome must be numeric", call. = FALSE)
  }
  if (!is.numeric(data[[time]])) {
    stop("Time must be numeric", call. = FALSE)
  }
  if (any(!is.finite(data[[outcome]]), na.rm = TRUE)) {
    stop("The outcome must contain only finite values or NA", call. = FALSE)
  }
  if (any(!is.finite(data[[time]]), na.rm = TRUE)) {
    stop("Time must contain only finite values or NA", call. = FALSE)
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
      stop("Covariates must be numeric, logical, or factor variables", call. = FALSE)
    }
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

  subject_ids <- unique(data[[id]])
  if (length(subject_ids) < 2L) {
    stop("At least two subjects are required for a mixed-effects model", call. = FALSE)
  }

  observations_per_subject <- table(data[[id]])
  repeated_subjects <- sum(observations_per_subject >= 2L)
  if (repeated_subjects < 2L) {
    stop(
      "At least two subjects must have repeated measurements",
      call. = FALSE
    )
  }

  if (random == "intercept-slope") {
    unique_times_per_subject <- vapply(
      split(data[[time]], data[[id]]),
      function(x) length(unique(x)),
      integer(1)
    )
    if (sum(unique_times_per_subject >= 2L) < 2L) {
      stop(
        "Random slopes require at least two subjects with multiple distinct time points",
        call. = FALSE
      )
    }
  }

  quote_name <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
  fixed_terms <- c(time, covariates)
  fixed_rhs <- paste(vapply(fixed_terms, quote_name, character(1)), collapse = " + ")

  random_term <- if (random == "intercept") {
    paste0("(1 | ", quote_name(id), ")")
  } else if (correlated) {
    paste0("(1 + ", quote_name(time), " | ", quote_name(id), ")")
  } else {
    paste0("(1 + ", quote_name(time), " || ", quote_name(id), ")")
  }

  formula <- stats::as.formula(
    paste(quote_name(outcome), "~", fixed_rhs, "+", random_term)
  )

  ensure_packages("lme4")
  fit <- lme4::lmer(
    formula,
    data = data,
    REML = identical(estimation, "REML"),
    na.action = stats::na.fail
  )

  singular <- lme4::isSingular(fit, tol = singular_tol)
  convergence_messages <- fit@optinfo$conv$lme4$messages
  if (is.null(convergence_messages)) {
    convergence_messages <- character()
  } else {
    convergence_messages <- as.character(convergence_messages)
  }

  if (singular) {
    warning(
      "The fitted random-effects structure is singular; inspect variance components",
      call. = FALSE
    )
  }

  structure(
    list(
      model = fit,
      metadata = list(
        outcome = outcome,
        time = time,
        id = id,
        covariates = covariates,
        random = random,
        correlated = if (random == "intercept-slope") correlated else NA,
        estimation = estimation,
        n_rows = nrow(data),
        n_subjects = length(subject_ids),
        repeated_subjects = repeated_subjects,
        omitted_rows = omitted_rows,
        singular = singular,
        singular_tol = singular_tol,
        convergence_messages = convergence_messages
      )
    ),
    class = "hds_longitudinal_mixed"
  )
}

#' Summarise fixed effects from a longitudinal mixed model
#'
#' Returns fixed-effect estimates with Wald standard errors and confidence
#' intervals. P-values are intentionally not generated because denominator
#' degrees of freedom for linear mixed models depend on an additional inferential
#' approximation such as Satterthwaite or Kenward-Roger.
#'
#' @param model Object returned by [fit_longitudinal_mixed].
#' @param conf_level Confidence level for Wald intervals.
#'
#' @return Data frame with fixed-effect estimates, standard errors, Wald
#'   statistics, and confidence limits.
#' @export
tidy_longitudinal_mixed <- function(model, conf_level = 0.95) {
  if (!inherits(model, "hds_longitudinal_mixed")) {
    stop("`model` must be created by `fit_longitudinal_mixed()`", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a number strictly between 0 and 1", call. = FALSE)
  }

  fit <- model$model
  coefficients <- lme4::fixef(fit)
  covariance <- as.matrix(stats::vcov(fit))
  std_error <- sqrt(diag(covariance))
  statistic <- coefficients / std_error
  critical_value <- stats::qnorm(1 - (1 - conf_level) / 2)

  data.frame(
    term = names(coefficients),
    estimate = unname(coefficients),
    std_error = unname(std_error),
    statistic = unname(statistic),
    conf_low = unname(coefficients) - critical_value * unname(std_error),
    conf_high = unname(coefficients) + critical_value * unname(std_error),
    row.names = NULL,
    check.names = FALSE
  )
}

#' Extract variance components from a longitudinal mixed model
#'
#' Returns random-effect variances, covariances or correlations, and residual
#' variance from the fitted linear mixed model.
#'
#' @param model Object returned by [fit_longitudinal_mixed].
#'
#' @return Data frame with grouping factor, component names, variance or
#'   covariance, and standard deviation or correlation.
#' @export
longitudinal_variance_components <- function(model) {
  if (!inherits(model, "hds_longitudinal_mixed")) {
    stop("`model` must be created by `fit_longitudinal_mixed()`", call. = FALSE)
  }

  components <- as.data.frame(lme4::VarCorr(model$model))
  data.frame(
    group = components$grp,
    term1 = components$var1,
    term2 = components$var2,
    variance_or_covariance = components$vcov,
    standard_deviation_or_correlation = components$sdcor,
    row.names = NULL,
    check.names = FALSE
  )
}
