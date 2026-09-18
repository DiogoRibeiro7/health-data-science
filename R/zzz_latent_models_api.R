# ------------------------------------------------------------------------------
# File: zzz_latent_models_api.R
# Purpose: Validated latent-variable and finite-mixture model wrappers.
# ------------------------------------------------------------------------------

.hds_latent_positive_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < 1 || x != floor(x)) {
    stop(paste0("`", name, "` must be a positive integer"), call. = FALSE)
  }
  as.integer(x)
}

.hds_latent_seed <- function(seed) {
  if (is.null(seed)) {
    return(NULL)
  }
  .hds_latent_positive_integer(seed, "seed")
}

.hds_quote_name <- function(x) {
  paste0("`", gsub("`", "", x, fixed = TRUE), "`")
}

.hds_prepare_binary_patterns <- function(data, allow_missing = FALSE) {
  if (is.data.frame(data)) {
    supported <- vapply(
      data,
      function(x) is.numeric(x) || is.integer(x) || is.logical(x),
      logical(1)
    )
    if (!all(supported)) {
      stop("Latent-class indicators must be numeric/integer/logical", call. = FALSE)
    }
    data <- as.matrix(data)
  }
  if (!is.matrix(data) ||
      !(is.numeric(data) || is.integer(data) || is.logical(data))) {
    stop("`data` must be a binary matrix or data frame", call. = FALSE)
  }
  if (nrow(data) < 2L || ncol(data) < 2L) {
    stop("At least two observations and two indicators are required", call. = FALSE)
  }
  if (!allow_missing && anyNA(data)) {
    stop("Missing indicator values are not supported by this wrapper", call. = FALSE)
  }
  observed <- data[!is.na(data)]
  if (any(!is.finite(observed)) || any(!observed %in% c(0, 1))) {
    stop("Latent-class indicators must be coded 0/1", call. = FALSE)
  }
  storage.mode(data) <- "integer"
  data
}

#' Fit a categorical latent-transition model
#'
#' Uses the current [LMest::lmest] API. Long-format data can be supplied with
#' explicit id/time columns. For backward compatibility, when `id` and `time`
#' are both omitted, `data` is interpreted as a wide matrix/data frame with one
#' subject per row and one categorical response occasion per column, then
#' converted internally to long format.
#'
#' @param data Long-format data frame, or legacy wide response matrix/data frame.
#' @param k Number of latent states.
#' @param id Unit-identifier column for long-format data.
#' @param time Time-occasion column for long-format data.
#' @param responses Optional response-column names for long-format data. When
#'   omitted, all columns other than id/time are treated as responses.
#' @param latent_formula Optional LMest latent-process formula.
#' @param start Initialization mode supported here: 0 deterministic or 1 random.
#' @param model_selection `"BIC"` or `"AIC"`.
#' @param mod_basic Transition-homogeneity setting passed to LMest.
#' @param tolerance Convergence tolerance.
#' @param max_iter Maximum EM iterations.
#' @param output_standard_errors Request information-matrix standard errors.
#' @param seed Optional random seed.
#' @param random_starts Number of additional random initializations.
#' @return Native LMest fit with `healthdatascience` metadata.
#' @export
fit_latent_transition <- function(
    data,
    k,
    id = NULL,
    time = NULL,
    responses = NULL,
    latent_formula = NULL,
    start = 0,
    model_selection = c("BIC", "AIC"),
    mod_basic = 0,
    tolerance = 1e-8,
    max_iter = 1000,
    output_standard_errors = FALSE,
    seed = NULL,
    random_starts = 0) {
  k <- .hds_latent_positive_integer(k, "k")
  model_selection <- match.arg(model_selection)
  max_iter <- .hds_latent_positive_integer(max_iter, "max_iter")
  seed <- .hds_latent_seed(seed)
  random_starts <- if (identical(random_starts, 0)) {
    0L
  } else {
    .hds_latent_positive_integer(random_starts, "random_starts")
  }

  if (!is.numeric(start) || length(start) != 1L ||
      !is.finite(start) || !start %in% c(0, 1)) {
    stop("`start` must be 0 (deterministic) or 1 (random)", call. = FALSE)
  }
  if (!is.numeric(mod_basic) || length(mod_basic) != 1L ||
      !is.finite(mod_basic) || mod_basic < 0 || mod_basic != floor(mod_basic)) {
    stop("`mod_basic` must be a non-negative integer", call. = FALSE)
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      !is.finite(tolerance) || tolerance <= 0) {
    stop("`tolerance` must be a positive finite number", call. = FALSE)
  }
  if (!is.logical(output_standard_errors) ||
      length(output_standard_errors) != 1L ||
      is.na(output_standard_errors)) {
    stop("`output_standard_errors` must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(latent_formula) && !inherits(latent_formula, "formula")) {
    stop("`latent_formula` must be NULL or a formula", call. = FALSE)
  }

  legacy_wide <- is.null(id) && is.null(time)
  if (xor(is.null(id), is.null(time))) {
    stop("Supply both `id` and `time`, or neither for legacy wide data", call. = FALSE)
  }

  if (legacy_wide) {
    if (!(is.matrix(data) || is.data.frame(data))) {
      stop("Legacy wide `data` must be a matrix or data frame", call. = FALSE)
    }
    wide <- as.data.frame(data)
    if (nrow(wide) < 2L || ncol(wide) < 2L) {
      stop(
        "Legacy wide data require at least two subjects and two occasions",
        call. = FALSE
      )
    }
    supported <- vapply(
      wide,
      function(x) {
        is.numeric(x) || is.integer(x) || is.logical(x) ||
          is.factor(x) || is.character(x)
      },
      logical(1)
    )
    if (!all(supported)) {
      stop("Legacy wide responses must be atomic categorical columns", call. = FALSE)
    }
    response_matrix <- as.matrix(wide)
    if (is.numeric(response_matrix) &&
        any(!is.finite(response_matrix), na.rm = TRUE)) {
      stop("Numeric responses must be finite or NA", call. = FALSE)
    }

    long_data <- data.frame(
      .hds_id = rep(seq_len(nrow(wide)), each = ncol(wide)),
      .hds_time = rep(seq_len(ncol(wide)), times = nrow(wide)),
      .hds_response = as.vector(t(response_matrix)),
      stringsAsFactors = FALSE
    )
    id <- ".hds_id"
    time <- ".hds_time"
    responses_formula <- stats::as.formula(".hds_response ~ NULL")
    response_names <- ".hds_response"
  } else {
    if (!is.data.frame(data)) {
      stop("Long-format `data` must be a data frame", call. = FALSE)
    }
    valid_name <- function(x) {
      is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
    }
    if (!valid_name(id) || !valid_name(time) || id == time) {
      stop("`id` and `time` must be distinct column names", call. = FALSE)
    }
    if (!all(c(id, time) %in% names(data))) {
      stop("`id` and `time` columns must exist in `data`", call. = FALSE)
    }
    if (anyNA(data[[id]]) || anyNA(data[[time]])) {
      stop("Identifier and time columns cannot contain missing values", call. = FALSE)
    }
    if (anyDuplicated(data[c(id, time)])) {
      stop("Each id/time occasion must appear at most once", call. = FALSE)
    }
    if (length(unique(data[[time]])) < 2L) {
      stop("Latent-transition models require at least two time occasions", call. = FALSE)
    }

    if (is.null(responses)) {
      response_names <- setdiff(names(data), c(id, time))
    } else {
      if (!is.character(responses) || length(responses) == 0L ||
          anyNA(responses) || any(!nzchar(responses)) ||
          anyDuplicated(responses)) {
        stop("`responses` must be unique non-empty column names", call. = FALSE)
      }
      response_names <- responses
    }
    if (length(response_names) == 0L ||
        any(!response_names %in% names(data))) {
      stop("At least one valid response column is required", call. = FALSE)
    }
    if (any(response_names %in% c(id, time))) {
      stop("Response columns must be distinct from id/time columns", call. = FALSE)
    }

    lhs <- paste(vapply(response_names, .hds_quote_name, character(1)),
                 collapse = " + ")
    responses_formula <- stats::as.formula(paste(lhs, "~ NULL"))
    long_data <- data
  }

  ensure_packages("LMest")
  args <- list(
    responsesFormula = responses_formula,
    latentFormula = latent_formula,
    data = long_data,
    index = c(id, time),
    k = k,
    start = as.integer(start),
    modSel = model_selection,
    modBasic = as.integer(mod_basic),
    tol = tolerance,
    maxit = max_iter,
    out_se = output_standard_errors,
    seed = seed,
    ntry = random_starts
  )
  fit <- do.call(LMest::lmest, args)

  attr(fit, "healthdatascience") <- list(
    engine = "LMest::lmest",
    k = k,
    input_format = if (legacy_wide) "legacy_wide" else "long",
    id = id,
    time = time,
    responses = response_names,
    model_selection = model_selection,
    mod_basic = as.integer(mod_basic),
    start = as.integer(start),
    tolerance = tolerance,
    max_iter = max_iter,
    random_starts = random_starts,
    seed = seed
  )
  fit
}

#' Fit a finite mixture model with covariates
#'
#' @param formula Model formula.
#' @param data Data frame.
#' @param k Number of mixture components.
#' @param model Optional FlexMix model driver.
#' @param concomitant Optional FlexMix concomitant-variable model.
#' @param control Optional FlexMix control object/list.
#' @param seed Optional random seed.
#' @param na_action Missing-value policy for variables in `formula`.
#' @param ... Additional arguments forwarded to [flexmix::flexmix].
#' @return Native `flexmix` fit with `healthdatascience` metadata.
#' @export
fit_mixture_covariates <- function(
    formula,
    data,
    k,
    model = NULL,
    concomitant = NULL,
    control = NULL,
    seed = NULL,
    na_action = c("fail", "omit"),
    ...) {
  na_action <- match.arg(na_action)
  k <- .hds_latent_positive_integer(k, "k")
  seed <- .hds_latent_seed(seed)

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
  if (nrow(data) < k) {
    stop("The fitted sample must contain at least as many rows as components", call. = FALSE)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ensure_packages("flexmix")
  args <- c(
    list(
      formula = formula,
      data = data,
      k = k
    ),
    list(...)
  )
  if (!is.null(model)) args$model <- model
  if (!is.null(concomitant)) args$concomitant <- concomitant
  if (!is.null(control)) args$control <- control
  fit <- do.call(flexmix::flexmix, args)

  attr(fit, "healthdatascience") <- list(
    engine = "flexmix::flexmix",
    k = k,
    n = nrow(data),
    omitted_rows = omitted_rows,
    seed = seed,
    formula = paste(deparse(formula), collapse = " ")
  )
  fit
}

#' Fit Bayesian latent class analysis for binary indicators
#'
#' @param data Binary indicator matrix/data frame.
#' @param nclass Number of latent classes.
#' @param method BayesLCA inference method: `"em"`, `"gibbs"`, `"boot"`,
#'   or `"vb"`.
#' @param ... Method-specific arguments forwarded to [BayesLCA::blca].
#' @return Native `blca` fit with `healthdatascience` metadata.
#' @export
fit_bayesian_lca <- function(
    data,
    nclass,
    method = c("em", "gibbs", "boot", "vb"),
    ...) {
  nclass <- .hds_latent_positive_integer(nclass, "nclass")
  method <- match.arg(method)
  patterns <- .hds_prepare_binary_patterns(data, allow_missing = FALSE)
  if (nclass > nrow(patterns)) {
    stop("`nclass` cannot exceed the number of observations", call. = FALSE)
  }

  ensure_packages("BayesLCA")
  fit <- BayesLCA::blca(
    X = patterns,
    G = nclass,
    method = method,
    ...
  )
  attr(fit, "healthdatascience") <- list(
    engine = "BayesLCA::blca",
    nclass = nclass,
    method = method,
    n = nrow(patterns),
    indicators = ncol(patterns)
  )
  fit
}

#' Fit a random-effects latent class model
#'
#' Uses [randomLCA::randomLCA] with named arguments so the class count cannot be
#' confused with the package's second positional `freq` argument.
#'
#' @param data Binary indicator matrix/data frame.
#' @param nclass Number of latent classes.
#' @param frequency Optional frequency for each supplied pattern.
#' @param random_effect Include a random effect. Defaults to TRUE because this
#'   wrapper is intended for random-effects/multilevel LCA.
#' @param level2 Fit the package's two-level random-effects formulation.
#' @param calculate_se Calculate parameter standard errors.
#' @param random_starts Number of random starting values.
#' @param cores Number of cores used for starting-value evaluation.
#' @param seed Optional random seed.
#' @param ... Additional arguments forwarded to [randomLCA::randomLCA].
#' @return Native `randomLCA` fit with `healthdatascience` metadata.
#' @export
fit_multilevel_lca <- function(
    data,
    nclass,
    frequency = NULL,
    random_effect = TRUE,
    level2 = FALSE,
    calculate_se = TRUE,
    random_starts = 20,
    cores = 1,
    seed = NULL,
    ...) {
  nclass <- .hds_latent_positive_integer(nclass, "nclass")
  patterns <- .hds_prepare_binary_patterns(data, allow_missing = TRUE)
  random_starts <- .hds_latent_positive_integer(random_starts, "random_starts")
  cores <- .hds_latent_positive_integer(cores, "cores")
  seed <- .hds_latent_seed(seed)

  logical_scalar <- function(x, name) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop(paste0("`", name, "` must be TRUE or FALSE"), call. = FALSE)
    }
    x
  }
  random_effect <- logical_scalar(random_effect, "random_effect")
  level2 <- logical_scalar(level2, "level2")
  calculate_se <- logical_scalar(calculate_se, "calculate_se")
  if (level2 && !random_effect) {
    stop("`level2 = TRUE` requires `random_effect = TRUE`", call. = FALSE)
  }

  if (!is.null(frequency)) {
    if (!is.numeric(frequency) || length(frequency) != nrow(patterns) ||
        anyNA(frequency) || any(!is.finite(frequency)) ||
        any(frequency < 0) || any(frequency != floor(frequency))) {
      stop(
        "`frequency` must be non-negative integer counts, one per pattern",
        call. = FALSE
      )
    }
    frequency <- as.integer(frequency)
    if (sum(frequency) == 0L) {
      stop("At least one pattern frequency must be positive", call. = FALSE)
    }
  }

  ensure_packages("randomLCA")
  args <- c(
    list(
      patterns = patterns,
      freq = frequency,
      nclass = nclass,
      calcSE = calculate_se,
      notrials = random_starts,
      random = random_effect,
      level2 = level2,
      cores = cores
    ),
    list(...)
  )
  if (!is.null(seed)) args$seed <- seed
  fit <- do.call(randomLCA::randomLCA, args)

  attr(fit, "healthdatascience") <- list(
    engine = "randomLCA::randomLCA",
    nclass = nclass,
    random_effect = random_effect,
    level2 = level2,
    calculate_se = calculate_se,
    random_starts = random_starts,
    cores = cores,
    seed = seed,
    patterns = nrow(patterns),
    indicators = ncol(patterns),
    frequency_supplied = !is.null(frequency)
  )
  fit
}

#' Latent-model wrapper metadata
#'
#' @param model Model returned by one of the advanced latent-model wrappers.
#' @return Named metadata list.
#' @export
latent_model_metadata <- function(model) {
  metadata <- attr(model, "healthdatascience")
  if (is.null(metadata) || is.null(metadata$engine)) {
    stop("Model does not contain healthdatascience latent-model metadata",
         call. = FALSE)
  }
  metadata
}
