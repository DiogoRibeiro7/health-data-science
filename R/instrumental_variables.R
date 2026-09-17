# ------------------------------------------------------------------------------
# File: instrumental_variables.R
# Purpose: Explicit two-stage least-squares estimation and IV diagnostics.
# ------------------------------------------------------------------------------

#' Fit an instrumental-variable model with first-stage diagnostics
#'
#' Fits a linear two-stage least-squares model with one endogenous treatment and
#' one or more excluded instruments using [AER::ivreg]. Instrument relevance is
#' diagnosed with the partial first-stage F statistic and partial R-squared from
#' nested linear models. These diagnostics measure instrument strength only; they
#' do not establish instrument independence or the exclusion restriction.
#'
#' @param data Data frame containing outcome, treatment, instruments, and
#'   optional baseline covariates.
#' @param outcome Name of the numeric outcome column.
#' @param treatment Name of the numeric endogenous treatment column.
#' @param instruments Character vector of excluded instrument columns.
#' @param covariates Character vector of included exogenous covariate columns.
#' @param na_action Missing-value policy: `"fail"` or `"omit"`.
#' @param weak_f_threshold Descriptive threshold for flagging a low first-stage
#'   partial F statistic.
#'
#' @return An object of class `hds_iv_model` containing the IV model, first-stage
#'   models, diagnostics, and interpretation metadata.
#' @export
fit_instrumental_variable <- function(
    data, outcome, treatment, instruments, covariates = character(),
    na_action = c("fail", "omit"), weak_f_threshold = 10) {
  na_action <- match.arg(na_action)
  if (!is.data.frame(data)) stop("`data` must be a data frame", call. = FALSE)
  scalar_name <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!scalar_name(outcome) || !scalar_name(treatment)) {
    stop("`outcome` and `treatment` must be single column names", call. = FALSE)
  }
  if (!is.character(instruments) || length(instruments) == 0L || anyNA(instruments) || any(!nzchar(instruments))) {
    stop("`instruments` must be a non-empty character vector", call. = FALSE)
  }
  if (!is.character(covariates) || anyNA(covariates) || any(!nzchar(covariates))) {
    stop("`covariates` must be a character vector", call. = FALSE)
  }
  if (anyDuplicated(instruments) || anyDuplicated(covariates)) {
    stop("Instrument and covariate names cannot contain duplicates", call. = FALSE)
  }
  if (outcome == treatment || outcome %in% instruments || treatment %in% instruments ||
      outcome %in% covariates || treatment %in% covariates || any(instruments %in% covariates)) {
    stop("Outcome, treatment, instruments, and covariates must have distinct roles", call. = FALSE)
  }
  if (!is.numeric(weak_f_threshold) || length(weak_f_threshold) != 1L || !is.finite(weak_f_threshold) || weak_f_threshold <= 0) {
    stop("`weak_f_threshold` must be a positive finite number", call. = FALSE)
  }

  required <- c(outcome, treatment, instruments, covariates)
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")), call. = FALSE)
  }
  if (!is.numeric(data[[outcome]]) || !is.numeric(data[[treatment]])) {
    stop("Outcome and treatment must be numeric", call. = FALSE)
  }
  supported <- function(x) is.numeric(x) || is.logical(x) || is.factor(x)
  if (any(!vapply(data[instruments], supported, logical(1)))) {
    stop("Instruments must be numeric, logical, or factor variables", call. = FALSE)
  }
  if (length(covariates) > 0L && any(!vapply(data[covariates], supported, logical(1)))) {
    stop("Covariates must be numeric, logical, or factor variables", call. = FALSE)
  }

  missing_rows <- !stats::complete.cases(data[required])
  omitted_rows <- sum(missing_rows)
  if (omitted_rows > 0L) {
    if (na_action == "fail") {
      stop("Missing values detected; use `na_action = \"omit\"` to drop rows", call. = FALSE)
    }
    data <- data[!missing_rows, , drop = FALSE]
  }
  if (nrow(data) == 0L) stop("No complete observations remain", call. = FALSE)
  if (length(unique(data[[treatment]])) < 2L) stop("`treatment` must vary", call. = FALSE)

  quote_name <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
  inst_rhs <- paste(vapply(instruments, quote_name, character(1)), collapse = " + ")
  cov_rhs <- if (length(covariates) > 0L) paste(vapply(covariates, quote_name, character(1)), collapse = " + ") else NULL
  full_rhs <- paste(c(inst_rhs, cov_rhs), collapse = " + ")
  restricted_rhs <- if (length(covariates) > 0L) cov_rhs else "1"
  first_formula <- stats::as.formula(paste(quote_name(treatment), "~", full_rhs))
  restricted_formula <- stats::as.formula(paste(quote_name(treatment), "~", restricted_rhs))
  first_stage <- stats::lm(first_formula, data = data)
  restricted_first_stage <- stats::lm(restricted_formula, data = data)
  comp <- stats::anova(restricted_first_stage, first_stage)
  first_stage_f <- unname(comp$F[2])
  if (is.na(first_stage_f)) stop("Excluded instruments add no estimable first-stage information", call. = FALSE)
  first_stage_df_num <- unname(comp$Df[2])
  first_stage_df_den <- stats::df.residual(first_stage)
  first_stage_p <- unname(comp$`Pr(>F)`[2])
  rss_restricted <- stats::deviance(restricted_first_stage)
  rss_full <- stats::deviance(first_stage)
  partial_r2 <- (rss_restricted - rss_full) / rss_restricted
  weak_flag <- first_stage_f < weak_f_threshold
  if (weak_flag) {
    warning(sprintf("First-stage partial F statistic is %.2f, below %.2f", first_stage_f, weak_f_threshold), call. = FALSE)
  }

  ensure_packages("AER")
  structural_rhs <- paste(c(quote_name(treatment), cov_rhs), collapse = " + ")
  instrument_rhs <- paste(c(inst_rhs, cov_rhs), collapse = " + ")
  iv_formula <- stats::as.formula(paste(quote_name(outcome), "~", structural_rhs, "|", instrument_rhs))
  iv_fit <- AER::ivreg(iv_formula, data = data, model = TRUE, x = TRUE, y = TRUE)

  inst_matrix <- stats::model.matrix(stats::as.formula(paste("~", inst_rhs)), data = data)
  if ("(Intercept)" %in% colnames(inst_matrix)) inst_matrix <- inst_matrix[, -1, drop = FALSE]
  instrument_rank <- qr(inst_matrix)$rank
  if (instrument_rank == 0L) stop("Excluded instruments have zero rank", call. = FALSE)

  overidentified <- instrument_rank > 1L
  sargan_statistic <- sargan_df <- sargan_p_value <- NA_real_
  if (overidentified) {
    overid_data <- data
    overid_data$.hds_iv_residual <- stats::residuals(iv_fit)
    overid_rhs <- paste(c(inst_rhs, cov_rhs), collapse = " + ")
    overid_fit <- stats::lm(stats::as.formula(paste(".hds_iv_residual ~", overid_rhs)), data = overid_data)
    sargan_statistic <- nrow(overid_data) * summary(overid_fit)$r.squared
    sargan_df <- instrument_rank - 1L
    sargan_p_value <- stats::pchisq(sargan_statistic, df = sargan_df, lower.tail = FALSE)
  }

  treatment_binary <- length(unique(data[[treatment]])) == 2L
  instrument_binary <- length(instruments) == 1L && length(unique(data[[instruments[[1]]]])) == 2L
  late_compatible <- treatment_binary && instrument_binary
  interpretation <- if (late_compatible) {
    "With one binary instrument and binary treatment, the coefficient may be interpreted as a LATE only under independence, exclusion, relevance, and monotonicity."
  } else {
    "The treatment coefficient is a linear 2SLS effect parameter; causal interpretation requires valid instruments and an appropriate structural model."
  }

  structure(list(
    model = iv_fit,
    first_stage = first_stage,
    restricted_first_stage = restricted_first_stage,
    diagnostics = list(
      first_stage_f = first_stage_f,
      first_stage_df_num = first_stage_df_num,
      first_stage_df_den = first_stage_df_den,
      first_stage_p_value = first_stage_p,
      partial_r_squared = partial_r2,
      weak_f_threshold = weak_f_threshold,
      weak_instrument_flag = weak_flag,
      excluded_instrument_rank = instrument_rank,
      overidentified = overidentified,
      sargan_statistic = sargan_statistic,
      sargan_df = sargan_df,
      sargan_p_value = sargan_p_value
    ),
    metadata = list(
      outcome = outcome, treatment = treatment, instruments = instruments,
      covariates = covariates, n = nrow(data), omitted_rows = omitted_rows,
      late_compatible_design = late_compatible, interpretation = interpretation,
      variance = "conventional homoskedastic 2SLS covariance"
    )
  ), class = "hds_iv_model")
}

#' Summarise IV diagnostics
#' @param model Object returned by [fit_instrumental_variable].
#' @return One-row diagnostic table.
#' @export
instrumental_variable_diagnostics <- function(model) {
  if (!inherits(model, "hds_iv_model")) stop("Invalid IV model", call. = FALSE)
  d <- model$diagnostics
  data.frame(
    first_stage_f = d$first_stage_f,
    first_stage_df_num = d$first_stage_df_num,
    first_stage_df_den = d$first_stage_df_den,
    first_stage_p_value = d$first_stage_p_value,
    partial_r_squared = d$partial_r_squared,
    weak_f_threshold = d$weak_f_threshold,
    weak_instrument_flag = d$weak_instrument_flag,
    excluded_instrument_rank = d$excluded_instrument_rank,
    overidentified = d$overidentified,
    sargan_statistic = d$sargan_statistic,
    sargan_df = d$sargan_df,
    sargan_p_value = d$sargan_p_value,
    check.names = FALSE
  )
}

#' Summarise the IV treatment coefficient
#' @param model Object returned by [fit_instrumental_variable].
#' @param conf_level Confidence level.
#' @return One-row coefficient summary.
#' @export
tidy_instrumental_variable <- function(model, conf_level = 0.95) {
  if (!inherits(model, "hds_iv_model")) stop("Invalid IV model", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1L || !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be strictly between 0 and 1", call. = FALSE)
  }
  fit <- model$model
  treatment <- model$metadata$treatment
  coef_names <- names(stats::coef(fit))
  idx <- match(treatment, coef_names)
  if (is.na(idx)) idx <- match(paste0("`", treatment, "`"), coef_names)
  if (is.na(idx)) stop("Treatment coefficient not found", call. = FALSE)
  estimate <- unname(stats::coef(fit)[idx])
  se <- sqrt(stats::vcov(fit)[idx, idx])
  stat <- estimate / se
  df <- stats::df.residual(fit)
  crit <- stats::qt(1 - (1 - conf_level) / 2, df = df)
  data.frame(
    treatment = treatment,
    estimate = estimate,
    std_error = se,
    statistic = stat,
    degrees_freedom = df,
    p_value = 2 * stats::pt(abs(stat), df = df, lower.tail = FALSE),
    conf_low = estimate - crit * se,
    conf_high = estimate + crit * se,
    late_compatible_design = model$metadata$late_compatible_design,
    interpretation = model$metadata$interpretation,
    variance = model$metadata$variance,
    check.names = FALSE
  )
}
