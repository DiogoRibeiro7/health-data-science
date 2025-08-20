# ------------------------------------------------------------------------------
# File: mlops.R
# Purpose: Model management utilities for versioning, validation and monitoring.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Register a model version
#'
#' Saves a model object to a registry directory with a timestamped filename for
#' reproducibility and audit purposes.
#'
#' @param model Fitted model object.
#' @param name Character identifier for the model.
#' @param registry Directory where models are stored.
#'
#' @return Path to the saved model file.
#' @export
#'
#' @examples
#' path <- register_model(lm(mpg ~ wt, mtcars), "lm_mpg")
register_model <- function(model, name, registry = "models") {
  if (!dir.exists(registry)) dir.create(registry, recursive = TRUE)
  file <- file.path(registry, sprintf("%s_%s.rds", name, format(Sys.time(), "%Y%m%d%H%M%S")))
  saveRDS(model, file)
  file
}

#' Validate a model using a user-supplied function
#'
#' @param model Fitted model object.
#' @param data Validation data frame.
#' @param metric_fn Function that computes a metric given predictions and
#'   outcomes.
#'
#' @return Numeric validation score.
#' @export
#'
#' @examples
#' score <- validate_model(lm(mpg ~ wt, mtcars), mtcars,
#'                         metric_fn = function(pred, obs) mean((pred - obs)^2))
validate_model <- function(model, data, metric_fn) {
  pred <- predict(model, data)
  metric_fn(pred, data[[all.vars(stats::formula(model))[1]]])
}

#' A/B test two models
#'
#' Computes a metric for two models on the same dataset to facilitate selection.
#'
#' @param model_a First model.
#' @param model_b Second model.
#' @param data Data frame for evaluation.
#' @param metric_fn Metric function taking predictions and outcomes.
#'
#' @return Named vector of metric values for each model.
#' @export
#'
#' @examples
#' ab <- ab_test_models(m1, m2, mtcars, metric_fn = function(p, o) mean((p - o)^2))
ab_test_models <- function(model_a, model_b, data, metric_fn) {
  pred_a <- predict(model_a, data)
  pred_b <- predict(model_b, data)
  c(model_a = metric_fn(pred_a, data[[all.vars(stats::formula(model_a))[1]]]),
    model_b = metric_fn(pred_b, data[[all.vars(stats::formula(model_b))[1]]]))
}

#' Detect model performance drift
#'
#' Evaluates a rolling window of metric values and flags when the change exceeds
#' a threshold.
#'
#' @param metrics Numeric vector of historical metric values ordered by time.
#' @param threshold Acceptable change in metric.
#'
#' @return Logical indicating whether drift is detected.
#' @export
#'
#' @examples
#' drift <- detect_model_drift(c(0.1, 0.15, 0.3), threshold = 0.1)
detect_model_drift <- function(metrics, threshold = 0.1) {
  if (length(metrics) < 2) return(FALSE)
  abs(metrics[length(metrics)] - metrics[length(metrics) - 1]) > threshold
}
