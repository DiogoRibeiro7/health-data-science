# ------------------------------------------------------------------------------
# File: interpretability.R
# Purpose: Model interpretability utilities including SHAP values and partial
#   dependence plots.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Calculate SHAP values for a model
#'
#' Utilises [iml::Shapley] to estimate Shapley values for each feature.
#'
#' @param model Fitted model object with a `predict` method.
#' @param data Data frame of predictors.
#' @param sample_size Number of observations to sample for estimation.
#'
#' @return Data frame of SHAP values.
#' @export
#'
#' @examples
#' shap <- compute_shap(ens$models$rf, iris[, -5])
compute_shap <- function(model, data, sample_size = 100) {
  ensure_packages("iml")
  sample_data <- data[sample(nrow(data), min(sample_size, nrow(data))), ]
  predictor <- iml::Predictor$new(model, data = sample_data)
  shap <- iml::Shapley$new(predictor, x.interest = sample_data)
  shap$results
}

#' Feature importance analysis
#'
#' Uses [iml::FeatureImp] to compute permutation importance for each feature.
#'
#' @param model Fitted model.
#' @param data Data frame of predictors.
#' @param target Response vector.
#'
#' @return Data frame of feature importances.
#' @export
#'
#' @examples
#' imp <- feature_importance(ens$models$rf, iris[, -5], iris$Species)
feature_importance <- function(model, data, target) {
  ensure_packages("iml")
  predictor <- iml::Predictor$new(model, data = data, y = target)
  iml::FeatureImp$new(predictor)$results
}

#' Partial dependence plot data
#'
#' Generates data for plotting partial dependence of a feature using [pdp::partial].
#'
#' @param model Fitted model.
#' @param data Data frame of predictors.
#' @param feature Feature name to examine.
#'
#' @return Data frame suitable for plotting.
#' @export
#'
#' @examples
#' pd <- partial_dependence_data(ens$models$rf, iris[, -5], "Sepal.Length")
partial_dependence_data <- function(model, data, feature) {
  ensure_packages("pdp")
  pdp::partial(model, pred.var = feature, train = data)
}

#' Local explanation using LIME
#'
#' Provides an interpretable explanation for a single prediction using
#' [lime::lime].
#'
#' @param model Fitted model.
#' @param data Training data for background distribution.
#' @param instance Single-row data frame representing the instance to explain.
#'
#' @return LIME explanation object.
#' @export
#'
#' @examples
#' expl <- local_explanation(ens$models$rf, iris[, -5], iris[1, -5])
local_explanation <- function(model, data, instance) {
  ensure_packages("lime")
  explainer <- lime::lime(data, model)
  lime::explain(instance, explainer)
}
