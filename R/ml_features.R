# ------------------------------------------------------------------------------
# File: ml_features.R
# Purpose: Automated feature selection and engineering utilities.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Automatic feature engineering using caret
#'
#' Applies centering, scaling and near-zero variance filtering using
#' [caret::preProcess] to produce a cleaned dataset suitable for modelling.
#'
#' @param df Data frame of predictor variables.
#' @param methods Pre-processing steps to apply.
#'
#' @return List containing the processed data frame and the `preProcess` object
#'   for reuse on new data.
#' @export
#'
#' @examples
#' fe <- auto_feature_engineering(mtcars)
#' head(fe$data)
auto_feature_engineering <- function(df,
                                     methods = c("center", "scale", "nzv")) {
  ensure_packages("caret")
  pp <- caret::preProcess(df, method = methods)
  list(data = predict(pp, df), prep = pp)
}

#' Select important features using random forests
#'
#' Fits a random forest model and returns the top features ranked by importance.
#'
#' @param df Data frame of predictors.
#' @param y Response vector.
#' @param top Number of top features to return.
#'
#' @return Character vector of selected feature names.
#' @export
#'
#' @examples
#' feats <- select_important_features(mtcars[, -1], mtcars$mpg)
select_important_features <- function(df, y, top = 5) {
  ensure_packages("randomForest")
  rf <- randomForest::randomForest(df, y, importance = TRUE)
  imp <- randomForest::importance(rf)
  rownames(imp)[order(imp[, 1], decreasing = TRUE)][seq_len(top)]
}
