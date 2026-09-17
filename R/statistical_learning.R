# ------------------------------------------------------------------------------
# File: statistical_learning.R
# Purpose: Statistical-learning helpers retained from the former advanced file.
# ------------------------------------------------------------------------------

#' Random forest with missing data handling
#'
#' Uses [ranger::ranger] with inbuilt imputation.
#'
#' @param data Data frame.
#' @param response Response variable name.
#' @return Fitted ranger model.
#' @export
train_rf_missing <- function(data, response) {
  ensure_packages("ranger")
  formula <- stats::as.formula(paste(response, "~ ."))
  ranger::ranger(formula, data = data, na.action = "na.impute")
}

#' Gradient boosting model
#'
#' Trains an [xgboost::xgboost] model for regression.
#'
#' @param x Matrix of features.
#' @param y Numeric outcome vector.
#' @param nrounds Number of boosting rounds.
#' @return Fitted xgboost model.
#' @export
train_gbm <- function(x, y, nrounds = 10) {
  ensure_packages("xgboost")
  xgboost::xgboost(data = x, label = y, nrounds = nrounds, verbose = 0)
}

#' Simple neural network
#'
#' Fits an [nnet::nnet] model for classification or regression.
#'
#' @param x Matrix or data frame of predictors.
#' @param y Response vector.
#' @param size Number of hidden units.
#' @return Fitted nnet model.
#' @export
train_neural_net <- function(x, y, size = 5) {
  ensure_packages("nnet")
  nnet::nnet(x, y, size = size, trace = FALSE)
}
