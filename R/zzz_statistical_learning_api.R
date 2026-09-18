# ------------------------------------------------------------------------------
# File: zzz_statistical_learning_api.R
# Purpose: Explicit contracts for retained statistical-learning helpers.
# ------------------------------------------------------------------------------

.hds_positive_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < 1 || x != floor(x)) {
    stop(paste0("`", name, "` must be a positive integer"), call. = FALSE)
  }
  as.integer(x)
}

.hds_probability <- function(x, name, include_one = TRUE) {
  upper_ok <- if (include_one) x <= 1 else x < 1
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x <= 0 || !upper_ok) {
    stop(
      paste0("`", name, "` must be in (0, ", if (include_one) "1]" else "1)"),
      call. = FALSE
    )
  }
  x
}

.hds_numeric_matrix <- function(x) {
  if (is.data.frame(x)) {
    if (ncol(x) == 0L || any(!vapply(x, is.numeric, logical(1)))) {
      stop("All predictor columns must be numeric", call. = FALSE)
    }
    x <- as.matrix(x)
  }
  if (!is.matrix(x) || !is.numeric(x) || nrow(x) == 0L || ncol(x) == 0L) {
    stop("`x` must be a non-empty numeric matrix or numeric data frame", call. = FALSE)
  }
  if (anyNA(x) || any(!is.finite(x))) {
    stop("`x` must contain only finite, non-missing values", call. = FALSE)
  }
  x
}

.hds_encode_binary <- function(y) {
  if (is.logical(y)) {
    encoded <- as.integer(y)
    levels <- c("FALSE", "TRUE")
  } else if (is.factor(y)) {
    y <- droplevels(y)
    if (nlevels(y) != 2L) {
      stop("Binary classification requires exactly two outcome levels", call. = FALSE)
    }
    levels <- levels(y)
    encoded <- as.integer(y == levels[2L])
  } else if (is.character(y)) {
    factor_y <- factor(y)
    if (nlevels(factor_y) != 2L) {
      stop("Binary classification requires exactly two outcome levels", call. = FALSE)
    }
    levels <- levels(factor_y)
    encoded <- as.integer(factor_y == levels[2L])
  } else if (is.numeric(y)) {
    if (anyNA(y) || any(!is.finite(y))) {
      stop("Binary outcomes must contain only finite, non-missing values", call. = FALSE)
    }
    observed <- sort(unique(y))
    if (length(observed) != 2L || !all(observed %in% c(0, 1))) {
      stop("Numeric binary outcomes must be coded 0/1", call. = FALSE)
    }
    levels <- c("0", "1")
    encoded <- as.integer(y)
  } else {
    stop(
      "Binary outcomes must be logical, factor, character, or numeric 0/1",
      call. = FALSE
    )
  }

  if (length(unique(encoded)) != 2L) {
    stop("Both outcome classes must be present", call. = FALSE)
  }
  list(y = encoded, levels = levels)
}

.hds_auc_binary <- function(y, probability) {
  positives <- sum(y == 1L)
  negatives <- sum(y == 0L)
  ranks <- rank(probability, ties.method = "average")
  (sum(ranks[y == 1L]) - positives * (positives + 1) / 2) /
    (positives * negatives)
}

.hds_apparent_performance <- function(task, y, prediction) {
  if (task == "regression") {
    return(data.frame(
      assessment = "apparent_training",
      rmse = sqrt(mean((y - prediction)^2)),
      mae = mean(abs(y - prediction)),
      r_squared = if (stats::var(y) == 0) NA_real_ else
        1 - sum((y - prediction)^2) / sum((y - mean(y))^2),
      row.names = NULL,
      check.names = FALSE
    ))
  }

  eps <- sqrt(.Machine$double.eps)
  probability <- pmin(pmax(prediction, eps), 1 - eps)
  predicted <- as.integer(probability >= 0.5)
  data.frame(
    assessment = "apparent_training",
    auc = .hds_auc_binary(y, probability),
    log_loss = -mean(y * log(probability) + (1 - y) * log(1 - probability)),
    accuracy = mean(predicted == y),
    row.names = NULL,
    check.names = FALSE
  )
}

#' Random forest with explicit task and missing-value handling
#'
#' Retains the historical `ranger` model return type while making the learning
#' task and missing-predictor strategy explicit. Classification is inferred for
#' factor, logical, or character responses; numeric responses default to
#' regression unless `task = "classification"` is supplied.
#'
#' @param data Data frame containing response and predictors.
#' @param response Response column name.
#' @param task `"auto"`, `"regression"`, or `"classification"`.
#' @param num.trees Number of trees.
#' @param mtry Optional number of candidate variables at each split.
#' @param min.node.size Optional minimum terminal-node size.
#' @param missing_action Predictor-missingness handling: `"learn"`, `"omit"`,
#'   or `"fail"`.
#' @param probability For classification, grow a probability forest.
#' @param seed Optional ranger random seed.
#' @return A fitted `ranger` object with a `healthdatascience` metadata
#'   attribute.
#' @export
train_rf_missing <- function(
    data,
    response,
    task = c("auto", "regression", "classification"),
    num.trees = 500,
    mtry = NULL,
    min.node.size = NULL,
    missing_action = c("learn", "omit", "fail"),
    probability = FALSE,
    seed = NULL) {
  task <- match.arg(task)
  missing_action <- match.arg(missing_action)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  if (!is.character(response) || length(response) != 1L ||
      is.na(response) || !nzchar(response) || !response %in% names(data)) {
    stop("`response` must name one column in `data`", call. = FALSE)
  }
  num.trees <- .hds_positive_integer(num.trees, "num.trees")
  if (!is.null(mtry)) {
    mtry <- .hds_positive_integer(mtry, "mtry")
    if (mtry > ncol(data) - 1L) {
      stop("`mtry` cannot exceed the number of predictors", call. = FALSE)
    }
  }
  if (!is.null(min.node.size)) {
    min.node.size <- .hds_positive_integer(min.node.size, "min.node.size")
  }
  if (!is.logical(probability) || length(probability) != 1L || is.na(probability)) {
    stop("`probability` must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(seed)) {
    seed <- .hds_positive_integer(seed, "seed")
  }

  predictors <- setdiff(names(data), response)
  if (length(predictors) == 0L) {
    stop("At least one predictor is required", call. = FALSE)
  }
  supported <- vapply(
    data[predictors],
    function(z) is.numeric(z) || is.logical(z) || is.factor(z),
    logical(1)
  )
  if (!all(supported)) {
    stop(
      "Random-forest predictors must be numeric, logical, or factor variables",
      call. = FALSE
    )
  }
  if (anyNA(data[[response]])) {
    stop("The response cannot contain missing values", call. = FALSE)
  }

  y <- data[[response]]
  inferred <- if (is.factor(y) || is.logical(y) || is.character(y)) {
    "classification"
  } else if (is.numeric(y)) {
    "regression"
  } else {
    stop("Unsupported random-forest response type", call. = FALSE)
  }
  resolved_task <- if (task == "auto") inferred else task

  model_data <- data
  class_levels <- NULL
  if (resolved_task == "classification") {
    if (is.numeric(y)) {
      if (length(unique(y)) < 2L) {
        stop("Classification requires at least two response classes", call. = FALSE)
      }
      model_data[[response]] <- factor(y)
    } else {
      model_data[[response]] <- droplevels(factor(y))
    }
    class_levels <- levels(model_data[[response]])
    if (length(class_levels) < 2L) {
      stop("Classification requires at least two response classes", call. = FALSE)
    }
  } else {
    if (!is.numeric(y) || any(!is.finite(y))) {
      stop("Regression requires a finite numeric response", call. = FALSE)
    }
    if (probability) {
      stop("`probability = TRUE` is only available for classification", call. = FALSE)
    }
  }

  ensure_packages("ranger")
  args <- list(
    formula = stats::reformulate(predictors, response = response),
    data = model_data,
    num.trees = num.trees,
    probability = probability,
    na.action = paste0("na.", missing_action),
    seed = seed,
    write.forest = TRUE
  )
  if (!is.null(mtry)) args$mtry <- mtry
  if (!is.null(min.node.size)) args$min.node.size <- min.node.size
  fit <- do.call(ranger::ranger, args)

  oob_metric <- if (resolved_task == "regression") {
    "oob_mse"
  } else if (probability) {
    "oob_brier_score"
  } else {
    "oob_misclassification_error"
  }
  attr(fit, "healthdatascience") <- list(
    engine = "ranger",
    task = resolved_task,
    response = response,
    predictors = predictors,
    class_levels = class_levels,
    num_trees = num.trees,
    mtry = fit$mtry,
    min_node_size = min.node.size,
    missing_action = missing_action,
    probability = probability,
    seed = seed,
    performance = data.frame(
      assessment = "out_of_bag",
      metric = oob_metric,
      value = fit$prediction.error,
      row.names = NULL,
      check.names = FALSE
    )
  )
  fit
}

#' Gradient boosting with explicit objective
#'
#' @param x Numeric predictor matrix or data frame.
#' @param y Numeric regression outcome or binary classification outcome.
#' @param nrounds Number of boosting rounds.
#' @param task `"regression"` or `"binary"`.
#' @param eta Learning rate.
#' @param max_depth Maximum tree depth.
#' @param subsample Row subsampling fraction.
#' @param colsample_bytree Column subsampling fraction per tree.
#' @param seed Optional R random seed.
#' @return An `xgb.Booster` with explicit training metadata.
#' @export
train_gbm <- function(
    x,
    y,
    nrounds = 10,
    task = c("regression", "binary"),
    eta = 0.3,
    max_depth = 6,
    subsample = 1,
    colsample_bytree = 1,
    seed = NULL) {
  task <- match.arg(task)
  x <- .hds_numeric_matrix(x)
  nrounds <- .hds_positive_integer(nrounds, "nrounds")
  max_depth <- .hds_positive_integer(max_depth, "max_depth")
  eta <- .hds_probability(eta, "eta")
  subsample <- .hds_probability(subsample, "subsample")
  colsample_bytree <- .hds_probability(colsample_bytree, "colsample_bytree")
  if (!is.null(seed)) {
    seed <- .hds_positive_integer(seed, "seed")
    set.seed(seed)
  }
  if (length(y) != nrow(x)) {
    stop("`y` must contain one outcome per predictor row", call. = FALSE)
  }

  if (task == "regression") {
    if (!is.numeric(y) || anyNA(y) || any(!is.finite(y))) {
      stop("Regression requires a finite numeric outcome", call. = FALSE)
    }
    encoded_y <- as.numeric(y)
    objective <- "reg:squarederror"
    eval_metric <- "rmse"
    class_levels <- NULL
  } else {
    encoded <- .hds_encode_binary(y)
    encoded_y <- encoded$y
    objective <- "binary:logistic"
    eval_metric <- "logloss"
    class_levels <- encoded$levels
  }

  ensure_packages("xgboost")
  fit <- xgboost::xgboost(
    data = x,
    label = encoded_y,
    nrounds = nrounds,
    objective = objective,
    eval_metric = eval_metric,
    eta = eta,
    max_depth = max_depth,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    verbose = 0
  )
  prediction <- as.numeric(stats::predict(fit, x))
  performance <- .hds_apparent_performance(task, encoded_y, prediction)

  attr(fit, "healthdatascience") <- list(
    engine = "xgboost",
    task = task,
    objective = objective,
    eval_metric = eval_metric,
    nrounds = nrounds,
    eta = eta,
    max_depth = max_depth,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    feature_names = colnames(x),
    class_levels = class_levels,
    seed = seed,
    performance = performance
  )
  fit
}

#' Neural network with explicit regression or binary-classification output
#'
#' @param x Numeric predictor matrix or data frame.
#' @param y Numeric regression outcome or binary classification outcome.
#' @param size Number of hidden units.
#' @param task `"auto"`, `"regression"`, or `"binary"`.
#' @param decay Weight-decay penalty.
#' @param maxit Maximum optimizer iterations.
#' @param scale_inputs Standardize predictors before fitting.
#' @param seed Optional R random seed.
#' @return A fitted `nnet` object with task/preprocessing metadata.
#' @export
train_neural_net <- function(
    x,
    y,
    size = 5,
    task = c("auto", "regression", "binary"),
    decay = 0,
    maxit = 100,
    scale_inputs = FALSE,
    seed = NULL) {
  task <- match.arg(task)
  x <- .hds_numeric_matrix(x)
  size <- .hds_positive_integer(size, "size")
  maxit <- .hds_positive_integer(maxit, "maxit")
  if (!is.numeric(decay) || length(decay) != 1L || !is.finite(decay) || decay < 0) {
    stop("`decay` must be a non-negative finite number", call. = FALSE)
  }
  if (!is.logical(scale_inputs) || length(scale_inputs) != 1L || is.na(scale_inputs)) {
    stop("`scale_inputs` must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(seed)) {
    seed <- .hds_positive_integer(seed, "seed")
    set.seed(seed)
  }
  if (length(y) != nrow(x)) {
    stop("`y` must contain one outcome per predictor row", call. = FALSE)
  }

  inferred <- if (is.factor(y) || is.logical(y) || is.character(y)) {
    "binary"
  } else if (is.numeric(y) && length(unique(y)) == 2L &&
             all(sort(unique(y)) %in% c(0, 1))) {
    "binary"
  } else {
    "regression"
  }
  resolved_task <- if (task == "auto") inferred else task

  center <- NULL
  scale <- NULL
  model_x <- x
  if (scale_inputs) {
    center <- colMeans(x)
    scale <- apply(x, 2L, stats::sd)
    scale[!is.finite(scale) | scale == 0] <- 1
    model_x <- sweep(x, 2L, center, "-")
    model_x <- sweep(model_x, 2L, scale, "/")
  }

  class_levels <- NULL
  if (resolved_task == "regression") {
    if (!is.numeric(y) || anyNA(y) || any(!is.finite(y))) {
      stop("Regression requires a finite numeric outcome", call. = FALSE)
    }
    encoded_y <- as.numeric(y)
    linout <- TRUE
    entropy <- FALSE
  } else {
    encoded <- .hds_encode_binary(y)
    encoded_y <- encoded$y
    class_levels <- encoded$levels
    linout <- FALSE
    entropy <- TRUE
  }

  ensure_packages("nnet")
  fit <- nnet::nnet(
    x = model_x,
    y = encoded_y,
    size = size,
    linout = linout,
    entropy = entropy,
    decay = decay,
    maxit = maxit,
    trace = FALSE
  )
  prediction <- as.numeric(stats::predict(fit, model_x, type = "raw"))
  performance <- .hds_apparent_performance(resolved_task, encoded_y, prediction)

  attr(fit, "healthdatascience") <- list(
    engine = "nnet",
    task = resolved_task,
    size = size,
    decay = decay,
    maxit = maxit,
    scale_inputs = scale_inputs,
    center = center,
    scale = scale,
    feature_names = colnames(x),
    class_levels = class_levels,
    seed = seed,
    performance = performance
  )
  fit
}

#' Statistical-learning model metadata
#'
#' @param model Model returned by `train_rf_missing()`, `train_gbm()`, or
#'   `train_neural_net()`.
#' @return Named metadata list.
#' @export
statistical_learning_metadata <- function(model) {
  metadata <- attr(model, "healthdatascience")
  if (is.null(metadata) || is.null(metadata$engine)) {
    stop("Model does not contain healthdatascience learning metadata", call. = FALSE)
  }
  metadata
}

#' Statistical-learning model performance
#'
#' Returns out-of-bag performance for ranger and apparent training performance
#' for xgboost/nnet. Apparent metrics are descriptive and are not validation.
#'
#' @param model Model returned by `train_rf_missing()`, `train_gbm()`, or
#'   `train_neural_net()`.
#' @return One-row performance data frame.
#' @export
statistical_learning_performance <- function(model) {
  metadata <- statistical_learning_metadata(model)
  metadata$performance
}
