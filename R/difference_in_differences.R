# ------------------------------------------------------------------------------
# File: difference_in_differences.R
# Purpose: Group-time difference-in-differences for staggered treatment timing.
# ------------------------------------------------------------------------------

#' Fit group-time difference-in-differences effects
#'
#' Estimates cohort-by-period average treatment effects using [did::att_gt].
#' Treatment timing is represented by the first-treated period for each unit;
#' never-treated units must use cohort value `0`. This avoids collapsing a
#' staggered-adoption design into a single two-way fixed-effects coefficient.
#'
#' @param data Panel data frame with one row per unit-period.
#' @param outcome Name of the numeric outcome column.
#' @param period Name of the numeric time-period column.
#' @param id Name of the unit identifier column.
#' @param first_treated Name of the first-treated-period column. Never-treated
#'   units must be coded `0`.
#' @param covariates Character vector of pre-treatment covariates used in the
#'   conditional parallel-trends adjustment.
#' @param control_group Control group definition: `"nevertreated"` or
#'   `"notyettreated"`.
#' @param anticipation Number of periods before treatment during which treatment
#'   may already affect outcomes.
#' @param est_method Estimation method passed to [did::att_gt]: `"dr"`, `"ipw"`,
#'   or `"reg"`.
#' @param base_period Base-period convention: `"varying"` or `"universal"`.
#' @param bstrap Whether to use the multiplier bootstrap for standard errors.
#' @param biters Number of bootstrap iterations when `bstrap = TRUE`.
#' @param cband Whether to compute simultaneous confidence bands.
#' @param alpha Significance level for confidence intervals/bands.
#' @param na_action Missing-value policy: `"fail"` or `"omit"`.
#'
#' @return An object of class `hds_group_time_did` containing the `MP` object
#'   returned by `did`, retained data, and design metadata.
#' @export
fit_group_time_did <- function(
    data,
    outcome,
    period,
    id,
    first_treated,
    covariates = character(),
    control_group = c("nevertreated", "notyettreated"),
    anticipation = 0,
    est_method = c("dr", "ipw", "reg"),
    base_period = c("varying", "universal"),
    bstrap = TRUE,
    biters = 1000,
    cband = TRUE,
    alpha = 0.05,
    na_action = c("fail", "omit")) {
  control_group <- match.arg(control_group)
  est_method <- match.arg(est_method)
  base_period <- match.arg(base_period)
  na_action <- match.arg(na_action)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }

  scalar_name <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }
  core_names <- list(outcome = outcome, period = period, id = id,
                     first_treated = first_treated)
  if (!all(vapply(core_names, scalar_name, logical(1)))) {
    stop(
      "`outcome`, `period`, `id`, and `first_treated` must be single column names",
      call. = FALSE
    )
  }
  if (!is.character(covariates) || anyNA(covariates) || any(!nzchar(covariates))) {
    stop("`covariates` must be a character vector", call. = FALSE)
  }
  if (anyDuplicated(covariates)) {
    stop("`covariates` cannot contain duplicates", call. = FALSE)
  }
  if (any(covariates %in% c(outcome, period, id, first_treated))) {
    stop("Covariates must have roles distinct from outcome, time, ID, and cohort", call. = FALSE)
  }

  required <- c(outcome, period, id, first_treated, covariates)
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      paste("Missing required columns:", paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }

  if (!is.numeric(data[[outcome]])) {
    stop("`outcome` must be numeric", call. = FALSE)
  }
  if (!is.numeric(data[[period]])) {
    stop("`period` must be numeric", call. = FALSE)
  }
  if (!is.numeric(data[[first_treated]])) {
    stop("`first_treated` must be numeric with 0 for never-treated units", call. = FALSE)
  }
  if (any(!is.finite(data[[outcome]]), na.rm = TRUE) ||
      any(!is.finite(data[[period]]), na.rm = TRUE) ||
      any(!is.finite(data[[first_treated]]), na.rm = TRUE)) {
    stop("Outcome, period, and treatment timing must contain finite values or NA", call. = FALSE)
  }
  if (any(data[[first_treated]] < 0, na.rm = TRUE)) {
    stop("`first_treated` must be 0 or a non-negative treatment period", call. = FALSE)
  }
  if (!is.numeric(anticipation) || length(anticipation) != 1L ||
      !is.finite(anticipation) || anticipation < 0 || anticipation != floor(anticipation)) {
    stop("`anticipation` must be a non-negative integer", call. = FALSE)
  }
  if (!is.logical(bstrap) || length(bstrap) != 1L || is.na(bstrap) ||
      !is.logical(cband) || length(cband) != 1L || is.na(cband)) {
    stop("`bstrap` and `cband` must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.numeric(biters) || length(biters) != 1L || !is.finite(biters) ||
      biters < 1 || biters != floor(biters)) {
    stop("`biters` must be a positive integer", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be strictly between 0 and 1", call. = FALSE)
  }

  supported <- function(x) is.numeric(x) || is.logical(x) || is.factor(x)
  if (length(covariates) > 0L &&
      any(!vapply(data[covariates], supported, logical(1)))) {
    stop("Covariates must be numeric, logical, or factor variables", call. = FALSE)
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

  if (anyNA(data[[id]])) {
    stop("Unit identifiers cannot be missing", call. = FALSE)
  }
  panel_key <- paste(data[[id]], data[[period]], sep = "\r")
  if (anyDuplicated(panel_key)) {
    stop("Panel data must contain at most one row per unit-period", call. = FALSE)
  }

  cohort_by_id <- split(data[[first_treated]], data[[id]])
  varying_cohort <- vapply(
    cohort_by_id,
    function(x) length(unique(x)) != 1L,
    logical(1)
  )
  if (any(varying_cohort)) {
    stop("`first_treated` must be constant within each unit", call. = FALSE)
  }

  observed_periods <- sort(unique(data[[period]]))
  treated_cohorts <- sort(unique(data[[first_treated]][data[[first_treated]] > 0]))
  if (length(treated_cohorts) == 0L) {
    stop("At least one treated cohort is required", call. = FALSE)
  }
  if (any(!treated_cohorts %in% observed_periods)) {
    stop("Every positive `first_treated` value must be an observed panel period", call. = FALSE)
  }
  if (control_group == "nevertreated" && !any(data[[first_treated]] == 0)) {
    stop("`control_group = \"nevertreated\"` requires units with `first_treated = 0`", call. = FALSE)
  }

  periods_by_id <- split(data[[period]], data[[id]])
  duplicate_periods <- vapply(periods_by_id, anyDuplicated, integer(1)) > 0L
  if (any(duplicate_periods)) {
    stop("Panel periods cannot repeat within a unit", call. = FALSE)
  }

  quote_name <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
  xformla <- if (length(covariates) > 0L) {
    stats::as.formula(
      paste("~", paste(vapply(covariates, quote_name, character(1)), collapse = " + "))
    )
  } else {
    ~1
  }

  ensure_packages("did")
  fit <- did::att_gt(
    yname = outcome,
    tname = period,
    idname = id,
    gname = first_treated,
    xformla = xformla,
    data = data,
    panel = TRUE,
    control_group = control_group,
    anticipation = anticipation,
    est_method = est_method,
    base_period = base_period,
    bstrap = bstrap,
    biters = as.integer(biters),
    cband = cband,
    alp = alpha,
    clustervars = id
  )

  structure(
    list(
      model = fit,
      data = data,
      metadata = list(
        outcome = outcome,
        period = period,
        id = id,
        first_treated = first_treated,
        covariates = covariates,
        control_group = control_group,
        anticipation = anticipation,
        est_method = est_method,
        base_period = base_period,
        n_rows = nrow(data),
        n_units = length(unique(data[[id]])),
        treated_cohorts = treated_cohorts,
        never_treated_units = length(unique(data[[id]][data[[first_treated]] == 0])),
        omitted_rows = omitted_rows,
        alpha = alpha
      )
    ),
    class = "hds_group_time_did"
  )
}

#' Tidy cohort-by-time treatment effects
#'
#' @param model Object returned by [fit_group_time_did].
#'
#' @return Data frame with treatment cohort, period, event time, ATT, standard
#'   error, pointwise normal confidence interval, and post-treatment indicator.
#' @export
tidy_group_time_did <- function(model) {
  if (!inherits(model, "hds_group_time_did")) {
    stop("`model` must be created by `fit_group_time_did()`", call. = FALSE)
  }

  fit <- model$model
  alpha <- model$metadata$alpha
  critical <- stats::qnorm(1 - alpha / 2)
  se <- as.numeric(fit$se)
  att <- as.numeric(fit$att)
  group <- as.numeric(fit$group)
  period <- as.numeric(fit$t)

  data.frame(
    group = group,
    period = period,
    event_time = period - group,
    estimate = att,
    std_error = se,
    conf_low = att - critical * se,
    conf_high = att + critical * se,
    post_treatment = period >= group,
    row.names = NULL,
    check.names = FALSE
  )
}

#' Aggregate group-time difference-in-differences effects
#'
#' Aggregates cohort-time treatment effects using [did::aggte]. `type =
#' "dynamic"` produces an event-study aggregation indexed by exposure time.
#'
#' @param model Object returned by [fit_group_time_did].
#' @param type Aggregation type: `"simple"`, `"dynamic"`, `"group"`, or
#'   `"calendar"`.
#' @param min_event,max_event Optional minimum and maximum event times for
#'   dynamic aggregation.
#' @param balance_event Optional balanced event-time horizon passed to `aggte`.
#' @param na_rm Whether unavailable group-time effects are removed in aggregation.
#'
#' @return An object of class `hds_did_aggregation` containing the `AGGTEobj`
#'   and a tidy aggregation table.
#' @export
aggregate_group_time_did <- function(
    model,
    type = c("simple", "dynamic", "group", "calendar"),
    min_event = NULL,
    max_event = NULL,
    balance_event = NULL,
    na_rm = FALSE) {
  type <- match.arg(type)
  if (!inherits(model, "hds_group_time_did")) {
    stop("`model` must be created by `fit_group_time_did()`", call. = FALSE)
  }
  if (!is.logical(na_rm) || length(na_rm) != 1L || is.na(na_rm)) {
    stop("`na_rm` must be TRUE or FALSE", call. = FALSE)
  }

  args <- list(
    MP = model$model,
    type = type,
    na.rm = na_rm
  )
  if (!is.null(min_event)) args$min_e <- min_event
  if (!is.null(max_event)) args$max_e <- max_event
  if (!is.null(balance_event)) args$balance_e <- balance_event

  aggregated <- do.call(did::aggte, args)

  if (type == "simple") {
    table <- data.frame(
      type = "simple",
      index = NA_real_,
      estimate = aggregated$overall.att,
      std_error = aggregated$overall.se,
      stringsAsFactors = FALSE
    )
  } else {
    index <- as.numeric(aggregated$egt)
    estimate <- as.numeric(aggregated$att.egt)
    std_error <- as.numeric(aggregated$se.egt)
    table <- data.frame(
      type = type,
      index = index,
      estimate = estimate,
      std_error = std_error,
      stringsAsFactors = FALSE
    )
  }

  alpha <- model$metadata$alpha
  critical <- stats::qnorm(1 - alpha / 2)
  table$conf_low <- table$estimate - critical * table$std_error
  table$conf_high <- table$estimate + critical * table$std_error

  structure(
    list(
      model = aggregated,
      table = table,
      metadata = list(type = type, alpha = alpha)
    ),
    class = "hds_did_aggregation"
  )
}

#' Report the group-time DiD pre-treatment Wald diagnostic
#'
#' Extracts the joint Wald test of pre-treatment group-time effects supplied by
#' the `did` estimator. Failure to reject does not prove parallel trends; it only
#' indicates that the available pre-treatment effects were not jointly detected
#' as different from zero at the chosen sample size and specification.
#'
#' @param model Object returned by [fit_group_time_did].
#'
#' @return One-row data frame with the Wald statistic and p-value.
#' @export
did_pretrend_diagnostic <- function(model) {
  if (!inherits(model, "hds_group_time_did")) {
    stop("`model` must be created by `fit_group_time_did()`", call. = FALSE)
  }

  statistic <- model$model$W
  p_value <- model$model$Wpval
  if (is.null(statistic) || is.null(p_value)) {
    statistic <- NA_real_
    p_value <- NA_real_
  }

  data.frame(
    statistic = as.numeric(statistic),
    p_value = as.numeric(p_value),
    interpretation = paste(
      "This is a joint pre-treatment Wald diagnostic, not proof of parallel trends."
    ),
    row.names = NULL,
    check.names = FALSE
  )
}
