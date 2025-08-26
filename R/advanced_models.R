# ------------------------------------------------------------------------------
# File: advanced_models.R
# Purpose: Advanced modelling utilities including ensembles and specialised
#   analyses for health data.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Train an ensemble of models and average predictions
#'
#' Currently supports random forests and generalized linear models. Additional
#' models can be added via the `models` argument.
#'
#' @param df Data frame containing predictors and response.
#' @param response Name of the response variable.
#' @param models Character vector of models to train.
#'
#' @return List with fitted models and a prediction function.
#' @export
#'
#' @examples
#' ens <- train_ensemble(mtcars, "mpg", models = c("rf", "lm"))
train_ensemble <- function(df, response, models = c("rf", "lm")) {
  y <- df[[response]]
  x <- df[, setdiff(names(df), response), drop = FALSE]
  fits <- list()
  if ("rf" %in% models) {
    ensure_packages("randomForest")
    fits$rf <- randomForest::randomForest(x, y)
  }
  if ("lm" %in% models) {
    fits$lm <- stats::lm(stats::as.formula(paste(response, "~ .")), data = df)
  }
  predict_fn <- function(newdata) {
    preds <- lapply(fits, predict, newdata = newdata)
    Reduce(`+`, preds) / length(preds)
  }
  list(models = fits, predict = predict_fn)
}

#' Fit a time-series model
#'
#' Uses [forecast::auto.arima] to model univariate time-series data.
#'
#' @param ts_data Numeric vector or ts object.
#'
#' @return Fitted ARIMA model.
#' @export
#'
#' @examples
#' fit <- fit_time_series(AirPassengers)
fit_time_series <- function(ts_data) {
  ensure_packages("forecast")
  forecast::auto.arima(ts_data)
}

#' Fit a Cox proportional hazards model
#'
#' Wrapper around [survival::coxph] to model time-to-event data.
#'
#' @param formula Survival formula of the form `Surv(time, status) ~ vars`.
#' @param data Data frame containing survival variables.
#'
#' @return Fitted coxph model.
#' @export
#'
#' @examples
#' library(survival)
#' fit <- fit_survival_model(Surv(time, status) ~ age, lung)
fit_survival_model <- function(formula, data) {
  ensure_packages("survival")
  survival::coxph(formula, data = data)
}

#' Estimate causal effect using propensity score matching
#'
#' Implements a basic propensity score matching workflow via [MatchIt::matchit].
#'
#' @param formula Treatment-outcome formula.
#' @param data Data frame containing variables.
#'
#' @return Matched dataset as produced by [MatchIt::match.data].
#' @export
#'
#' @examples
#' matched <- estimate_causal_effect(treat ~ x1 + x2, df)
estimate_causal_effect <- function(formula, data) {
  ensure_packages("MatchIt")
  m <- MatchIt::matchit(formula, data = data)
  MatchIt::match.data(m)
}
