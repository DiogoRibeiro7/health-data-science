# ------------------------------------------------------------------------------
# File: advanced_latent_models.R
# Purpose: Advanced latent-variable and mixture-model helpers.
# ------------------------------------------------------------------------------

#' Fit a latent transition analysis (LTA) model
#'
#' Uses [lmest::LMest] to estimate transitions between latent states over time.
#'
#' @param data Matrix or data.frame of ordinal responses over time.
#' @param k Number of latent states.
#' @return Fitted LTA model object.
#' @export
fit_latent_transition <- function(data, k) {
  ensure_packages("lmest")
  lmest::LMest(data, k = k)
}

#' Fit a mixture model with covariates
#'
#' Wrapper around [flexmix::flexmix] to include covariates in component models.
#'
#' @param formula Model formula.
#' @param data Data frame with variables.
#' @param k Number of mixture components.
#' @return Fitted flexmix model.
#' @export
fit_mixture_covariates <- function(formula, data, k) {
  ensure_packages("flexmix")
  flexmix::flexmix(formula, data = data, k = k)
}

#' Bayesian latent class analysis
#'
#' Implements Bayesian LCA via [BayesLCA::blca].
#'
#' @param data Matrix of categorical indicators.
#' @param nclass Number of latent classes.
#' @return Posterior samples and summaries.
#' @export
fit_bayesian_lca <- function(data, nclass) {
  ensure_packages("BayesLCA")
  BayesLCA::blca(data, classes = nclass)
}

#' Multi-level latent class model
#'
#' Uses [randomLCA::randomLCA] for hierarchical latent class modelling.
#'
#' @param data Matrix of categorical indicators.
#' @param nclass Number of classes.
#' @return Fitted randomLCA model.
#' @export
fit_multilevel_lca <- function(data, nclass) {
  ensure_packages("randomLCA")
  randomLCA::randomLCA(data, nclass)
}
