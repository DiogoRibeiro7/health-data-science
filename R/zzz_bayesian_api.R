# ------------------------------------------------------------------------------
# File: zzz_bayesian_api.R
# Purpose: Explicit Bayesian fitting controls, diagnostics, and summaries.
# ------------------------------------------------------------------------------

.hds_bayes_positive_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < 1 || x != floor(x)) {
    stop(paste0("`", name, "` must be a positive integer"), call. = FALSE)
  }
  as.integer(x)
}

.hds_bayes_probability <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x <= 0 || x >= 1) {
    stop(paste0("`", name, "` must be strictly between 0 and 1"), call. = FALSE)
  }
  x
}

.hds_bayes_seed <- function(seed) {
  if (is.null(seed)) return(NULL)
  .hds_bayes_positive_integer(seed, "seed")
}

.hds_stanfit <- function(model) {
  if (inherits(model, "stanfit")) {
    return(model)
  }
  if (inherits(model, "stanreg")) {
    if (!identical(model$algorithm, "sampling")) {
      stop(
        "Sampling diagnostics require a stanreg model fit with algorithm = \"sampling\"",
        call. = FALSE
      )
    }
    if (is.null(model$stanfit) || !inherits(model$stanfit, "stanfit")) {
      stop("The stanreg object does not contain a usable stanfit object", call. = FALSE)
    }
    return(model$stanfit)
  }
  stop("`model` must inherit from `stanfit` or `stanreg`", call. = FALSE)
}

.hds_prepare_bayes_data <- function(formula, data, na_action) {
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a model formula", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
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

#' Run generic MCMC sampling with explicit Stan controls
#'
#' Compiles Stan model code and samples with NUTS using explicit chain, warmup,
#' thinning, seed, adaptation, and tree-depth controls. The native `stanfit`
#' return type is preserved and fitting metadata are attached.
#'
#' @param model Non-empty Stan model code.
#' @param data Named list or environment containing Stan data.
#' @param iter Total iterations per chain, including warmup.
#' @param warmup Warmup/adaptation iterations per chain. Defaults to half of
#'   `iter`.
#' @param chains Number of chains.
#' @param cores Number of parallel cores.
#' @param thin Saved-draw thinning interval.
#' @param seed Optional random seed.
#' @param adapt_delta NUTS target acceptance probability.
#' @param max_treedepth Maximum NUTS tree depth.
#' @return A fitted `stanfit` object.
#' @export
run_mcmc <- function(
    model,
    data,
    iter = 1000,
    warmup = NULL,
    chains = 4,
    cores = 1,
    thin = 1,
    seed = NULL,
    adapt_delta = 0.8,
    max_treedepth = 10) {
  if (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(model)) {
    stop("`model` must be one non-empty Stan program", call. = FALSE)
  }
  if (!(is.list(data) || is.environment(data))) {
    stop("`data` must be a named list or environment", call. = FALSE)
  }
  if (is.list(data) && (is.null(names(data)) || any(!nzchar(names(data))))) {
    stop("Stan data supplied as a list must be named", call. = FALSE)
  }

  iter <- .hds_bayes_positive_integer(iter, "iter")
  if (is.null(warmup)) warmup <- floor(iter / 2)
  warmup <- .hds_bayes_positive_integer(warmup, "warmup")
  if (warmup >= iter) {
    stop("`warmup` must be smaller than `iter`", call. = FALSE)
  }
  chains <- .hds_bayes_positive_integer(chains, "chains")
  cores <- .hds_bayes_positive_integer(cores, "cores")
  thin <- .hds_bayes_positive_integer(thin, "thin")
  seed <- .hds_bayes_seed(seed)
  adapt_delta <- .hds_bayes_probability(adapt_delta, "adapt_delta")
  max_treedepth <- .hds_bayes_positive_integer(max_treedepth, "max_treedepth")

  ensure_packages("rstan")
  stan_model <- rstan::stan_model(model_code = model)
  args <- list(
    object = stan_model,
    data = data,
    chains = chains,
    iter = iter,
    warmup = warmup,
    thin = thin,
    cores = cores,
    refresh = 0,
    control = list(
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth
    )
  )
  if (!is.null(seed)) args$seed <- seed
  fit <- do.call(rstan::sampling, args)

  attr(fit, "healthdatascience") <- list(
    engine = "rstan",
    algorithm = "NUTS",
    iter = iter,
    warmup = warmup,
    chains = chains,
    cores = cores,
    thin = thin,
    seed = seed,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth
  )
  fit
}

#' Bayesian model averaging for generalized linear models
#'
#' Wraps [BMA::bic.glm] with an explicit GLM family, missing-data policy, and
#' model-search controls.
#'
#' @param formula GLM formula.
#' @param data Data frame.
#' @param family GLM family passed to `BMA::bic.glm(glm.family = ...)`.
#' @param strict Whether to remove models having more probable submodels.
#' @param odds_ratio Posterior-odds window used by BMA.
#' @param max_col Maximum number of predictors retained before BMA's preliminary
#'   reduction.
#' @param nbest Maximum number of models retained by the search.
#' @param na_action Missing-value policy: `"fail"` or `"omit"`.
#' @return A `bic.glm` object with healthdatascience metadata.
#' @export
bayesian_model_averaging <- function(
    formula,
    data,
    family = stats::gaussian(),
    strict = FALSE,
    odds_ratio = 20,
    max_col = 30,
    nbest = 150,
    na_action = c("fail", "omit")) {
  na_action <- match.arg(na_action)
  prepared <- .hds_prepare_bayes_data(formula, data, na_action)

  if (!(inherits(family, "family") || is.function(family) ||
        (is.character(family) && length(family) == 1L && nzchar(family)))) {
    stop("`family` must be a GLM family object, family function, or family name", call. = FALSE)
  }
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    stop("`strict` must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.numeric(odds_ratio) || length(odds_ratio) != 1L || !is.finite(odds_ratio) || odds_ratio <= 1) {
    stop("`odds_ratio` must be a finite number greater than 1", call. = FALSE)
  }
  max_col <- .hds_bayes_positive_integer(max_col, "max_col")
  nbest <- .hds_bayes_positive_integer(nbest, "nbest")

  ensure_packages("BMA")
  fit <- BMA::bic.glm(
    formula,
    data = prepared$data,
    glm.family = family,
    strict = strict,
    OR = odds_ratio,
    maxCol = max_col,
    nbest = nbest,
    na.action = stats::na.fail
  )
  attr(fit, "healthdatascience") <- list(
    engine = "BMA::bic.glm",
    family = if (inherits(family, "family")) family$family else as.character(substitute(family)),
    strict = strict,
    OR = odds_ratio,
    maxCol = max_col,
    nbest = nbest,
    n = nrow(prepared$data),
    omitted_rows = prepared$omitted_rows
  )
  fit
}

#' Tidy Bayesian model-averaged coefficients
#'
#' @param model A fitted `bic.glm` object.
#' @return Data frame containing posterior inclusion probabilities and
#'   model-averaged coefficient summaries.
#' @export
tidy_bayesian_model_average <- function(model) {
  if (!inherits(model, "bic.glm")) {
    stop("`model` must inherit from `bic.glm`", call. = FALSE)
  }
  inclusion <- as.numeric(model$probne0)
  postmean <- as.numeric(model$postmean)
  postsd <- as.numeric(model$postsd)
  condmean <- as.numeric(model$condpostmean)
  condsd <- as.numeric(model$condpostsd)

  lengths <- c(length(inclusion), length(postmean), length(postsd), length(condmean), length(condsd))
  if (length(unique(lengths)) != 1L) {
    stop("The BMA object contains inconsistent coefficient summary lengths", call. = FALSE)
  }
  terms <- names(model$probne0)
  if (is.null(terms) || length(terms) != length(inclusion) || any(!nzchar(terms))) {
    terms <- names(model$postmean)
  }
  if (is.null(terms) || length(terms) != length(inclusion) || any(!nzchar(terms))) {
    terms <- paste0("term_", seq_along(inclusion))
  }

  data.frame(
    term = terms,
    inclusion_probability = inclusion / 100,
    posterior_mean = postmean,
    posterior_sd = postsd,
    conditional_posterior_mean = condmean,
    conditional_posterior_sd = condsd,
    row.names = NULL,
    check.names = FALSE
  )
}

#' Fit a hierarchical Bayesian generalized linear model
#'
#' Fits a `stan_glmer` model with explicit MCMC controls. The formula must
#' contain at least one group-specific term using `|`.
#'
#' @param formula Hierarchical model formula.
#' @param data Data frame.
#' @param family GLM family.
#' @param iter Total iterations per chain.
#' @param warmup Warmup iterations per chain. Defaults to half of `iter`.
#' @param chains Number of chains.
#' @param cores Number of parallel cores.
#' @param seed Optional random seed.
#' @param adapt_delta NUTS target acceptance probability.
#' @param max_treedepth Maximum NUTS tree depth.
#' @param qr Whether to use rstanarm's scaled QR decomposition.
#' @param na_action Missing-value policy.
#' @param ... Additional prior or model arguments forwarded to
#'   [rstanarm::stan_glmer].
#' @return A `stanreg` object with fitting metadata.
#' @export
fit_hierarchical_bayes <- function(
    formula,
    data,
    family = stats::gaussian(),
    iter = 2000,
    warmup = NULL,
    chains = 4,
    cores = 1,
    seed = NULL,
    adapt_delta = 0.95,
    max_treedepth = 15,
    qr = FALSE,
    na_action = c("fail", "omit"),
    ...) {
  na_action <- match.arg(na_action)
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a model formula", call. = FALSE)
  }
  formula_text <- paste(deparse(formula), collapse = " ")
  if (!grepl("\\|", formula_text)) {
    stop("`formula` must contain at least one group-specific `|` term", call. = FALSE)
  }
  prepared <- .hds_prepare_bayes_data(formula, data, na_action)

  iter <- .hds_bayes_positive_integer(iter, "iter")
  if (is.null(warmup)) warmup <- floor(iter / 2)
  warmup <- .hds_bayes_positive_integer(warmup, "warmup")
  if (warmup >= iter) {
    stop("`warmup` must be smaller than `iter`", call. = FALSE)
  }
  chains <- .hds_bayes_positive_integer(chains, "chains")
  cores <- .hds_bayes_positive_integer(cores, "cores")
  seed <- .hds_bayes_seed(seed)
  adapt_delta <- .hds_bayes_probability(adapt_delta, "adapt_delta")
  max_treedepth <- .hds_bayes_positive_integer(max_treedepth, "max_treedepth")
  if (!is.logical(qr) || length(qr) != 1L || is.na(qr)) {
    stop("`qr` must be TRUE or FALSE", call. = FALSE)
  }

  ensure_packages("rstanarm")
  args <- c(
    list(
      formula = formula,
      data = prepared$data,
      family = family,
      algorithm = "sampling",
      iter = iter,
      warmup = warmup,
      chains = chains,
      cores = cores,
      adapt_delta = adapt_delta,
      QR = qr,
      refresh = 0,
      na.action = stats::na.fail,
      control = list(max_treedepth = max_treedepth)
    ),
    list(...)
  )
  if (!is.null(seed)) args$seed <- seed
  fit <- do.call(rstanarm::stan_glmer, args)

  attr(fit, "healthdatascience") <- list(
    engine = "rstanarm::stan_glmer",
    algorithm = "sampling",
    iter = iter,
    warmup = warmup,
    chains = chains,
    cores = cores,
    seed = seed,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    qr = qr,
    n = nrow(prepared$data),
    omitted_rows = prepared$omitted_rows,
    formula = formula_text
  )
  fit
}

#' Summarise posterior draws from Stan models
#'
#' @param model A `stanfit` or sampling-based `stanreg` object.
#' @param pars Optional parameter names.
#' @param prob Central credible-interval probability.
#' @param include_lp Include Stan's `lp__` row.
#' @return Data frame with posterior means, medians, standard deviations,
#'   credible intervals, effective sample sizes, and R-hat.
#' @export
tidy_bayesian_posterior <- function(
    model,
    pars = NULL,
    prob = 0.95,
    include_lp = FALSE) {
  prob <- .hds_bayes_probability(prob, "prob")
  if (!is.null(pars) &&
      (!is.character(pars) || length(pars) == 0L || anyNA(pars) || any(!nzchar(pars)))) {
    stop("`pars` must be NULL or a non-empty character vector", call. = FALSE)
  }
  if (!is.logical(include_lp) || length(include_lp) != 1L || is.na(include_lp)) {
    stop("`include_lp` must be TRUE or FALSE", call. = FALSE)
  }

  ensure_packages("rstan")
  stanfit <- .hds_stanfit(model)
  alpha <- 1 - prob
  probs <- c(alpha / 2, 0.5, 1 - alpha / 2)
  args <- list(object = stanfit, probs = probs)
  if (!is.null(pars)) args$pars <- pars
  summary_matrix <- do.call(base::summary, args)$summary

  if (!include_lp && "lp__" %in% rownames(summary_matrix)) {
    summary_matrix <- summary_matrix[rownames(summary_matrix) != "lp__", , drop = FALSE]
  }
  quantile_columns <- grep("%$", colnames(summary_matrix))
  if (length(quantile_columns) != 3L) {
    stop("Unexpected Stan summary quantile structure", call. = FALSE)
  }

  data.frame(
    parameter = rownames(summary_matrix),
    mean = summary_matrix[, "mean"],
    median = summary_matrix[, quantile_columns[2L]],
    sd = summary_matrix[, "sd"],
    credible_low = summary_matrix[, quantile_columns[1L]],
    credible_high = summary_matrix[, quantile_columns[3L]],
    n_eff = summary_matrix[, "n_eff"],
    rhat = summary_matrix[, "Rhat"],
    row.names = NULL,
    check.names = FALSE
  )
}

#' Diagnose Stan MCMC sampling
#'
#' Reports R-hat, effective sample size, divergent transitions, and maximum
#' tree-depth saturation. Thresholds are descriptive diagnostics rather than a
#' guarantee that a posterior approximation is adequate.
#'
#' @param model A `stanfit` or sampling-based `stanreg` object.
#' @param rhat_threshold R-hat threshold used for flagging parameters.
#' @param min_ess Minimum effective sample size used for flagging parameters.
#' @return Object of class `hds_bayesian_diagnostics` containing aggregate and
#'   parameter-level diagnostics.
#' @export
bayesian_sampling_diagnostics <- function(
    model,
    rhat_threshold = 1.01,
    min_ess = 400) {
  if (!is.numeric(rhat_threshold) || length(rhat_threshold) != 1L ||
      !is.finite(rhat_threshold) || rhat_threshold <= 1) {
    stop("`rhat_threshold` must be greater than 1", call. = FALSE)
  }
  min_ess <- .hds_bayes_positive_integer(min_ess, "min_ess")

  ensure_packages("rstan")
  stanfit <- .hds_stanfit(model)
  summary_matrix <- base::summary(stanfit)$summary
  keep <- rownames(summary_matrix) != "lp__"
  parameter_table <- data.frame(
    parameter = rownames(summary_matrix)[keep],
    n_eff = summary_matrix[keep, "n_eff"],
    rhat = summary_matrix[keep, "Rhat"],
    row.names = NULL,
    check.names = FALSE
  )
  parameter_table$rhat_flag <- is.finite(parameter_table$rhat) &
    parameter_table$rhat > rhat_threshold
  parameter_table$ess_flag <- is.finite(parameter_table$n_eff) &
    parameter_table$n_eff < min_ess

  sampler_params <- rstan::get_sampler_params(stanfit, inc_warmup = FALSE)
  metadata <- attr(model, "healthdatascience")
  max_treedepth <- if (!is.null(metadata$max_treedepth)) {
    metadata$max_treedepth
  } else if (inherits(model, "stanreg")) {
    15L
  } else {
    10L
  }
  divergences <- sum(vapply(
    sampler_params,
    function(x) sum(x[, "divergent__"]),
    numeric(1)
  ))
  treedepth_hits <- sum(vapply(
    sampler_params,
    function(x) sum(x[, "treedepth__"] >= max_treedepth),
    numeric(1)
  ))
  post_warmup_draws <- sum(vapply(sampler_params, nrow, integer(1)))

  aggregate <- data.frame(
    max_rhat = if (any(is.finite(parameter_table$rhat))) {
      max(parameter_table$rhat, na.rm = TRUE)
    } else {
      NA_real_
    },
    min_n_eff = if (any(is.finite(parameter_table$n_eff))) {
      min(parameter_table$n_eff, na.rm = TRUE)
    } else {
      NA_real_
    },
    rhat_flagged_parameters = sum(parameter_table$rhat_flag, na.rm = TRUE),
    ess_flagged_parameters = sum(parameter_table$ess_flag, na.rm = TRUE),
    divergent_transitions = divergences,
    max_treedepth_hits = treedepth_hits,
    post_warmup_draws = post_warmup_draws,
    rhat_threshold = rhat_threshold,
    min_ess = min_ess,
    diagnostics_flag = (
      sum(parameter_table$rhat_flag, na.rm = TRUE) > 0L ||
      sum(parameter_table$ess_flag, na.rm = TRUE) > 0L ||
      divergences > 0L ||
      treedepth_hits > 0L
    ),
    row.names = NULL,
    check.names = FALSE
  )

  structure(
    list(
      summary = aggregate,
      parameters = parameter_table,
      interpretation = paste(
        "Flags identify potential sampling problems; absence of flags does not",
        "establish model adequacy, prior adequacy, or predictive validity."
      )
    ),
    class = "hds_bayesian_diagnostics"
  )
}

#' Draw from a stanreg posterior predictive distribution
#'
#' @param model A fitted sampling-based `stanreg` model.
#' @param newdata Optional predictor data.
#' @param draws Optional number of posterior predictive draws.
#' @param seed Optional random seed.
#' @param ... Additional arguments passed to [rstanarm::posterior_predict].
#' @return A draws-by-observations matrix.
#' @export
posterior_predictive_draws <- function(
    model,
    newdata = NULL,
    draws = NULL,
    seed = NULL,
    ...) {
  if (!inherits(model, "stanreg")) {
    stop("`model` must inherit from `stanreg`", call. = FALSE)
  }
  if (!is.null(draws)) draws <- .hds_bayes_positive_integer(draws, "draws")
  seed <- .hds_bayes_seed(seed)

  ensure_packages("rstanarm")
  args <- c(list(object = model, newdata = newdata), list(...))
  if (!is.null(draws)) args$draws <- draws
  if (!is.null(seed)) args$seed <- seed
  do.call(rstanarm::posterior_predict, args)
}

#' Posterior predictive check for stanreg models
#'
#' Uses rstanarm's `pp_check.stanreg` method and bayesplot. Posterior predictive
#' checks compare replicated outcomes with the observed development data; they
#' are model-checking tools, not predictive validation.
#'
#' @param model Fitted `stanreg` model.
#' @param data Deprecated compatibility argument. New-data posterior draws should
#'   use [posterior_predictive_draws].
#' @param plotfun bayesplot PPC function name, without or with the `ppc_` prefix.
#' @param nreps Optional number of replicated datasets shown.
#' @param seed Optional random seed.
#' @param ... Additional arguments passed to the PPC plotting function.
#' @return A ggplot object produced through `pp_check.stanreg`.
#' @export
posterior_predictive_check <- function(
    model,
    data = NULL,
    plotfun = "dens_overlay",
    nreps = NULL,
    seed = NULL,
    ...) {
  if (!inherits(model, "stanreg")) {
    stop("`model` must inherit from `stanreg`", call. = FALSE)
  }
  if (!is.null(data)) {
    warning(
      paste(
        "`data` is deprecated and ignored by posterior predictive checks.",
        "Use `posterior_predictive_draws(newdata = ...)` for new predictor data."
      ),
      call. = FALSE
    )
  }
  if (!is.character(plotfun) || length(plotfun) != 1L ||
      is.na(plotfun) || !nzchar(plotfun)) {
    stop("`plotfun` must be one non-empty plot name", call. = FALSE)
  }
  if (!is.null(nreps)) nreps <- .hds_bayes_positive_integer(nreps, "nreps")
  seed <- .hds_bayes_seed(seed)

  ensure_packages(c("rstanarm", "bayesplot"))
  args <- c(list(object = model, plotfun = plotfun), list(...))
  if (!is.null(nreps)) args$nreps <- nreps
  if (!is.null(seed)) args$seed <- seed
  do.call(bayesplot::pp_check, args)
}
