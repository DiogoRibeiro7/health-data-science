# ------------------------------------------------------------------------------
# File: bayesian_methods.R
# Purpose: Bayesian modelling helpers.
# ------------------------------------------------------------------------------

#' Run generic MCMC sampling with rstan
#'
#' @param model Stan model code.
#' @param data List of data for Stan.
#' @param iter Number of iterations.
#' @return rstan fit object.
#' @export
run_mcmc <- function(model, data, iter = 1000) {
  ensure_packages("rstan")
  sm <- rstan::stan_model(model_code = model)
  rstan::sampling(sm, data = data, iter = iter, refresh = 0)
}

#' Bayesian model averaging
#'
#' Uses [BMA::bic.glm] to average over GLM models.
#'
#' @param formula Model formula.
#' @param data Data frame.
#' @return BMA model object.
#' @export
bayesian_model_averaging <- function(formula, data) {
  ensure_packages("BMA")
  BMA::bic.glm(formula, data = data)
}

#' Hierarchical Bayesian model
#'
#' Fits a mixed-effects model via [rstanarm::stan_glmer].
#'
#' @param formula Model formula.
#' @param data Data frame.
#' @param family Outcome family.
#' @return stanreg model.
#' @export
fit_hierarchical_bayes <- function(formula, data, family = stats::gaussian()) {
  ensure_packages("rstanarm")
  rstanarm::stan_glmer(formula, data = data, family = family, refresh = 0)
}

#' Posterior predictive check
#'
#' Generates posterior predictive plots using [bayesplot::pp_check].
#'
#' @param model Fitted Bayesian model.
#' @param data Optional new data for prediction.
#' @export
posterior_predictive_check <- function(model, data = NULL) {
  ensure_packages("bayesplot")
  bayesplot::pp_check(model, newdata = data)
}
