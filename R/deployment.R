# ------------------------------------------------------------------------------
# File: deployment.R
# Purpose: Utilities for model scoring and deployment in production settings.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Score new data with a model via REST API
#'
#' Simple wrapper that can be used inside a plumber endpoint to score incoming
#' data.
#'
#' @param model Fitted model object.
#' @param newdata Data frame of new observations.
#'
#' @return Predictions as a vector or matrix.
#' @export
#'
#' @examples
#' preds <- score_model_api(lm(mpg ~ wt, mtcars), mtcars)
score_model_api <- function(model, newdata) {
  predict(model, newdata)
}

#' Batch prediction helper
#'
#' Splits a dataset into chunks and scores each batch to avoid memory pressure on
#' large datasets.
#'
#' @param model Fitted model object.
#' @param data Data frame of observations.
#' @param chunk_size Number of rows per batch.
#'
#' @return Vector of predictions.
#' @export
#'
#' @examples
#' preds <- batch_predict(lm(mpg ~ wt, mtcars), mtcars, chunk_size = 10)
batch_predict <- function(model, data, chunk_size = 1000) {
  n <- nrow(data)
  idx <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  unlist(lapply(idx, function(i) predict(model, data[i, , drop = FALSE])))
}

#' Stub for model containerisation
#'
#' Placeholder function that would build a container image for the supplied
#' model. Currently logs the target platform for future integration.
#'
#' @param model_path Path to the serialised model file.
#' @param platform Target platform (e.g., "docker").
#'
#' @return Invisible TRUE on success.
#' @export
#'
#' @examples
#' containerize_model("models/model.rds")
containerize_model <- function(model_path, platform = "docker") {
  logger::log_info(sprintf("Containerising %s for %s", model_path, platform))
  invisible(TRUE)
}

#' Deploy model to external platform
#'
#' Placeholder for future integration with cloud deployment APIs.
#'
#' @param model_path Path to the serialised model file.
#' @param platform Deployment target.
#'
#' @return Invisible TRUE on success.
#' @export
#'
#' @examples
#' deploy_model("models/model.rds", platform = "sagemaker")
deploy_model <- function(model_path, platform) {
  logger::log_info(sprintf("Deploying %s to %s", model_path, platform))
  invisible(TRUE)
}
