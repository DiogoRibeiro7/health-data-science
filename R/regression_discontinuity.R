# ------------------------------------------------------------------------------
# File: regression_discontinuity.R
# Purpose: Explicit local-polynomial regression-discontinuity analysis.
# ------------------------------------------------------------------------------

#' Fit a sharp regression-discontinuity design
#'
#' Fits a local-polynomial sharp RD estimand at an explicit cutoff using
#' [rdrobust::rdrobust]. The treatment side is determined by whether the running
#' variable is greater than or equal to the cutoff. Bandwidths may be selected by
#' `rdrobust` or supplied explicitly.
#'
#' @param data Data frame containing the outcome and running variable.
#' @param outcome Name of the numeric outcome column.
#' @param running Name of the numeric running-variable column.
#' @param cutoff Cutoff defining treatment assignment.
#' @param covariates Optional numeric covariates for precision adjustment.
#' @param p Local-polynomial order for point estimation.
#' @param q Bias-correction polynomial order. Defaults to `p + 1`.
#' @param kernel Kernel used by `rdrobust`: `"triangular"`, `"uniform"`, or
#'   `"epanechnikov"`.
#' @param bandwidth Optional scalar or two-element bandwidth vector. When NULL,
#'   `rdrobust` selects bandwidths.
#' @param bwselect Bandwidth-selection method passed to `rdrobust` when
#'   `bandwidth` is NULL.
#' @param masspoints Mass-point handling passed to `rdrobust`.
#' @param na_action Missing-value policy: `"fail"` or `"omit"`.
#'
#' @return An object of class `hds_rd_model` containing the `rdrobust` fit and
#'   explicit design metadata.
#' @export
fit_regression_discontinuity <- function(
    data,
    outcome,
    running,
    cutoff = 0,
    covariates = character(),
    p = 1,
    q = p + 1,
    kernel = c("triangular", "uniform", "epanechnikov"),
    bandwidth = NULL,
    bwselect = "mserd",
    masspoints = c("adjust", "check", "off"),
    na_action = c("fail", "omit")) {
  kernel <- match.arg(kernel)
  masspoints <- match.arg(masspoints)
  na_action <- match.arg(na_action)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  scalar_name <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }
  if (!scalar_name(outcome) || !scalar_name(running)) {
    stop("`outcome` and `running` must be single column names", call. = FALSE)
  }
  if (!outcome %in% names(data) || !running %in% names(data)) {
    stop("Outcome and running-variable columns must exist in `data`", call. = FALSE)
  }
  if (outcome == running) {
    stop("Outcome and running variable must be different columns", call. = FALSE)
  }
  if (!is.character(covariates) || anyNA(covariates) || any(!nzchar(covariates))) {
    stop("`covariates` must be a character vector", call. = FALSE)
  }
  if (anyDuplicated(covariates) || outcome %in% covariates || running %in% covariates) {
    stop("Covariates must be unique and distinct from outcome/running columns", call. = FALSE)
  }
  missing_covariates <- setdiff(covariates, names(data))
  if (length(missing_covariates) > 0L) {
    stop(
      paste("Missing covariate columns:", paste(missing_covariates, collapse = ", ")),
      call. = FALSE
    )
  }
  if (!is.numeric(data[[outcome]]) || !is.numeric(data[[running]])) {
    stop("Outcome and running variable must be numeric", call. = FALSE)
  }
  if (length(covariates) > 0L && any(!vapply(data[covariates], is.numeric, logical(1)))) {
    stop("RD adjustment covariates must be numeric", call. = FALSE)
  }
  if (!is.numeric(cutoff) || length(cutoff) != 1L || !is.finite(cutoff)) {
    stop("`cutoff` must be a finite numeric scalar", call. = FALSE)
  }
  integer_scalar <- function(x, minimum = 0L) {
    is.numeric(x) && length(x) == 1L && is.finite(x) &&
      x == floor(x) && x >= minimum
  }
  if (!integer_scalar(p, 0L) || !integer_scalar(q, 1L) || q <= p) {
    stop("`p` must be a non-negative integer and `q` an integer greater than `p`", call. = FALSE)
  }
  if (!is.null(bandwidth)) {
    if (!is.numeric(bandwidth) || !length(bandwidth) %in% c(1L, 2L) ||
        anyNA(bandwidth) || any(!is.finite(bandwidth)) || any(bandwidth <= 0)) {
      stop("`bandwidth` must be NULL or one/two positive finite numbers", call. = FALSE)
    }
  }
  if (!is.character(bwselect) || length(bwselect) != 1L || !nzchar(bwselect)) {
    stop("`bwselect` must be one bandwidth-selection method", call. = FALSE)
  }

  required <- c(outcome, running, covariates)
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
  if (any(!is.finite(data[[outcome]])) || any(!is.finite(data[[running]]))) {
    stop("Outcome and running variable must contain only finite values", call. = FALSE)
  }
  if (length(covariates) > 0L && any(!is.finite(as.matrix(data[covariates])))) {
    stop("Covariates must contain only finite values", call. = FALSE)
  }

  x <- data[[running]]
  y <- data[[outcome]]
  if (!any(x < cutoff) || !any(x >= cutoff)) {
    stop("Observations are required on both sides of the cutoff", call. = FALSE)
  }
  if (length(unique(x)) < 4L) {
    stop("The running variable has too few distinct values for RD estimation", call. = FALSE)
  }

  covs <- if (length(covariates) > 0L) as.matrix(data[covariates]) else NULL
  ensure_packages("rdrobust")
  args <- list(
    y = y,
    x = x,
    c = cutoff,
    p = as.integer(p),
    q = as.integer(q),
    kernel = kernel,
    bwselect = bwselect,
    masspoints = masspoints,
    covs = covs
  )
  if (!is.null(bandwidth)) {
    args$h <- bandwidth
  }
  fit <- do.call(rdrobust::rdrobust, args)

  structure(
    list(
      model = fit,
      data = data,
      metadata = list(
        outcome = outcome,
        running = running,
        cutoff = cutoff,
        covariates = covariates,
        p = as.integer(p),
        q = as.integer(q),
        kernel = kernel,
        bandwidth = bandwidth,
        bwselect = bwselect,
        masspoints = masspoints,
        n = nrow(data),
        n_left = sum(x < cutoff),
        n_right = sum(x >= cutoff),
        omitted_rows = omitted_rows,
        estimand = "sharp RD treatment effect at the cutoff"
      )
    ),
    class = "hds_rd_model"
  )
}

#' Summarise a regression-discontinuity estimate
#'
#' Returns conventional, bias-corrected, and robust-bias-corrected rows when
#' available from `rdrobust`.
#'
#' @param model Object returned by [fit_regression_discontinuity].
#'
#' @return Data frame with estimate, standard error, p-value, and confidence
#'   interval for each inference row exposed by `rdrobust`.
#' @export
tidy_regression_discontinuity <- function(model) {
  if (!inherits(model, "hds_rd_model")) {
    stop("`model` must be created by `fit_regression_discontinuity()`", call. = FALSE)
  }
  fit <- model$model
  coef <- as.numeric(fit$coef)
  se <- as.numeric(fit$se)
  pv <- as.numeric(fit$pv)
  ci <- as.matrix(fit$ci)

  n_rows <- min(length(coef), length(se), length(pv), nrow(ci))
  if (n_rows == 0L) {
    stop("The `rdrobust` object contains no extractable inference rows", call. = FALSE)
  }
  labels <- rownames(fit$coef)
  if (is.null(labels) || length(labels) < n_rows) {
    labels <- c("conventional", "bias_corrected", "robust")[seq_len(n_rows)]
  }

  data.frame(
    inference = labels[seq_len(n_rows)],
    estimate = coef[seq_len(n_rows)],
    std_error = se[seq_len(n_rows)],
    p_value = pv[seq_len(n_rows)],
    conf_low = ci[seq_len(n_rows), 1],
    conf_high = ci[seq_len(n_rows), 2],
    cutoff = model$metadata$cutoff,
    estimand = model$metadata$estimand,
    row.names = NULL,
    check.names = FALSE
  )
}

#' Test continuity of baseline covariates at the RD cutoff
#'
#' Fits the same local-polynomial RD specification separately to each numeric
#' baseline covariate. Large discontinuities can indicate local imbalance, but
#' non-significant tests do not prove RD identification.
#'
#' @param model Object returned by [fit_regression_discontinuity].
#' @param covariates Numeric baseline covariates to test. Defaults to covariates
#'   recorded in the RD fit.
#'
#' @return Data frame containing the robust-bias-corrected discontinuity estimate
#'   and p-value for each covariate.
#' @export
rd_covariate_balance <- function(model, covariates = NULL) {
  if (!inherits(model, "hds_rd_model")) {
    stop("`model` must be created by `fit_regression_discontinuity()`", call. = FALSE)
  }
  if (is.null(covariates)) {
    covariates <- model$metadata$covariates
  }
  if (!is.character(covariates) || length(covariates) == 0L ||
      anyNA(covariates) || any(!nzchar(covariates))) {
    stop("Provide at least one numeric covariate for continuity testing", call. = FALSE)
  }
  if (any(!covariates %in% names(model$data))) {
    stop("All requested covariates must exist in the fitted RD data", call. = FALSE)
  }
  if (any(!vapply(model$data[covariates], is.numeric, logical(1)))) {
    stop("Covariate continuity tests require numeric variables", call. = FALSE)
  }

  ensure_packages("rdrobust")
  x <- model$data[[model$metadata$running]]
  rows <- lapply(covariates, function(variable) {
    fit <- rdrobust::rdrobust(
      y = model$data[[variable]],
      x = x,
      c = model$metadata$cutoff,
      p = model$metadata$p,
      q = model$metadata$q,
      kernel = model$metadata$kernel,
      bwselect = model$metadata$bwselect,
      masspoints = model$metadata$masspoints
    )
    coef <- as.numeric(fit$coef)
    se <- as.numeric(fit$se)
    pv <- as.numeric(fit$pv)
    index <- length(coef)
    data.frame(
      variable = variable,
      estimate = coef[index],
      std_error = se[index],
      p_value = pv[index],
      row.names = NULL,
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Diagnose running-variable density continuity at the RD cutoff
#'
#' Applies [rddensity::rddensity] to the running variable. A density discontinuity
#' can be consistent with sorting or manipulation near the threshold, but this is
#' a diagnostic rather than a definitive validity test.
#'
#' @param model Object returned by [fit_regression_discontinuity].
#'
#' @return A list containing the raw `rddensity` object and its available test
#'   statistic/p-value components.
#' @export
rd_density_diagnostic <- function(model) {
  if (!inherits(model, "hds_rd_model")) {
    stop("`model` must be created by `fit_regression_discontinuity()`", call. = FALSE)
  }
  ensure_packages("rddensity")
  density_fit <- rddensity::rddensity(
    X = model$data[[model$metadata$running]],
    c = model$metadata$cutoff
  )
  test <- density_fit$test

  get_component <- function(name) {
    if (is.null(test) || is.null(test[[name]])) NA_real_ else as.numeric(test[[name]])[1]
  }

  structure(
    list(
      statistic = get_component("t_jk"),
      p_value = get_component("p_jk"),
      asymptotic_statistic = get_component("t_asy"),
      asymptotic_p_value = get_component("p_asy"),
      raw = density_fit,
      interpretation = paste(
        "A density discontinuity may indicate sorting/manipulation around the cutoff;",
        "absence of evidence is not proof of no manipulation."
      )
    ),
    class = "hds_rd_density_diagnostic"
  )
}
