# ------------------------------------------------------------------------------
# File: advanced_statistics.R
# Purpose: Cutting-edge statistical methods for healthcare research including
#   latent variable models, causal inference, survival analysis, machine learning,
#   specialised healthcare analytics, and Bayesian utilities.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-04-??
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

#' Propensity score stratification
#'
#' Creates subclasses based on propensity scores using [MatchIt::matchit].
#'
#' @param formula Treatment ~ covariates formula.
#' @param data Data frame with variables.
#' @param subclass Number of strata.
#' @return Matched object from [MatchIt].
#' @export
propensity_stratification <- function(formula, data, subclass = 5) {
  ensure_packages("MatchIt")
  MatchIt::matchit(formula, data = data, method = "subclass", subclass = subclass)
}

#' Instrumental variable analysis
#'
#' Performs IV regression using [AER::ivreg].
#'
#' @param formula 2SLS formula of form `y ~ x | z`.
#' @param data Data frame.
#' @return Fitted ivreg model.
#' @export
instrumental_variable <- function(formula, data) {
  ensure_packages("AER")
  AER::ivreg(formula, data = data)
}

#' Difference-in-differences estimation
#'
#' Uses [fixest::feols] for two-way fixed-effects DiD.
#'
#' @param formula Outcome ~ treatment + time + treatment:time.
#' @param data Data frame.
#' @return Fitted model.
#' @export
difference_in_differences <- function(formula, data) {
  ensure_packages("fixest")
  fixest::feols(formula, data = data)
}

#' Regression discontinuity design
#'
#' Estimates treatment effect at cutoff using [rdrobust::rdrobust].
#'
#' @param y Outcome vector.
#' @param x Running variable.
#' @param c Cutoff value.
#' @return Result list from [rdrobust].
#' @export
regression_discontinuity <- function(y, x, c = 0) {
  ensure_packages("rdrobust")
  rdrobust::rdrobust(y, x, c = c)
}

#' Competing risks regression
#'
#' Fits Fine-Gray competing risks model via [cmprsk::crr].
#'
#' @param time Follow-up time.
#' @param status Event indicator (1 = event of interest, 2 = competing event).
#' @param covariates Matrix of covariates.
#' @return Fitted crr model.
#' @export
fit_competing_risks <- function(time, status, covariates) {
  ensure_packages("cmprsk")
  cmprsk::crr(time, status, covariates)
}

#' Time-varying coefficient Cox model
#'
#' Wraps [survival::coxph] with `tt` for time-varying effects.
#'
#' @param formula Cox model formula using `tt()` for terms.
#' @param data Data frame.
#' @return Fitted coxph object.
#' @export
fit_time_varying_cox <- function(formula, data) {
  ensure_packages("survival")
  survival::coxph(formula, data = data)
}

#' Frailty Cox model
#'
#' Includes random effects via [survival::coxph] and `frailty` term.
#'
#' @param formula Cox model formula with `frailty()`.
#' @param data Data frame.
#' @return Fitted coxph object.
#' @export
fit_frailty_cox <- function(formula, data) {
  ensure_packages("survival")
  survival::coxph(formula, data = data)
}

#' Landmark analysis
#'
#' Fits a Cox model using subjects at risk at a landmark time.
#'
#' @param formula Cox model formula.
#' @param data Data frame with `time` variable.
#' @param landmark Landmark time point.
#' @return Fitted coxph object on truncated data.
#' @export
landmark_cox <- function(formula, data, landmark) {
  ensure_packages("survival")
  d <- data[data$time >= landmark, ]
  survival::coxph(formula, data = d)
}

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

#' Pharmacovigilance signal detection
#'
#' Computes proportional reporting ratio (PRR) for a drug-event pair.
#'
#' @param a Count of drug and event.
#' @param b Count of drug without event.
#' @param c Count of event without drug.
#' @param d Count of neither drug nor event.
#' @return PRR value.
#' @export
detect_pharmacovigilance <- function(a, b, c, d) {
  ((a / (a + b)) / (c / (c + d)))
}

#' Cost-effectiveness analysis
#'
#' Calculates the incremental cost-effectiveness ratio (ICER).
#'
#' @param cost1 Cost for treatment 1.
#' @param effect1 Effect for treatment 1.
#' @param cost2 Cost for treatment 2.
#' @param effect2 Effect for treatment 2.
#' @return ICER value.
#' @export
cost_effectiveness <- function(cost1, effect1, cost2, effect2) {
  (cost1 - cost2) / (effect1 - effect2)
}

#' Develop a clinical prediction rule
#'
#' Fits a logistic regression and returns coefficients as a rule.
#'
#' @param formula Formula for logistic regression.
#' @param data Data frame.
#' @return Named coefficient vector.
#' @export
develop_clinical_prediction_rule <- function(formula, data) {
  fit <- stats::glm(formula, data = data, family = stats::binomial())
  stats::coef(fit)
}

#' Biomarker discovery using limma
#'
#' Identifies differentially expressed biomarkers via [limma::eBayes].
#'
#' @param expression Matrix of expression values (genes x samples).
#' @param group Factor indicating sample groups.
#' @return Data frame of top biomarkers.
#' @export
biomarker_discovery <- function(expression, group) {
  ensure_packages("limma")
  design <- stats::model.matrix(~ group)
  fit <- limma::lmFit(expression, design)
  fit <- limma::eBayes(fit)
  limma::topTable(fit, coef = 2)
}

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
