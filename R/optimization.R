# ------------------------------------------------------------------------------
# File: optimization.R
# Purpose: Parameter optimization helpers for latent class analysis and
#   regression workflows.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Determine optimal number of LCA classes using BIC
#'
#' Fits models across a range of class counts and selects the one with the
#' lowest Bayesian Information Criterion.
#'
#' @param df_full Prepared dataset with all variables.
#' @param df_subset Subset of variables used in the model.
#' @param k_range Integer vector of class counts to evaluate.
#'
#' @return List with `best_k` and a data frame of `scores` for each k.
#' @export
#'
#' @examples
#' opt <- optimal_lca_classes(df, subset, 2:6)
optimal_lca_classes <- function(df_full, df_subset, k_range = 2:10) {
  ensure_packages("poLCA")
  scores <- data.frame(k = integer(), BIC = numeric(), AIC = numeric())
  for (k in k_range) {
    model <- poLCA::poLCA(
      with(df_subset, cbind(race3, hispanic3, urbandwell, age4, SES, insurancetype) ~ 1),
      df_full, nclass = k, verbose = FALSE
    )
    scores <- rbind(scores, data.frame(k = k, BIC = model$bic, AIC = model$aic))
  }
  best_k <- scores$k[which.min(scores$BIC)]
  list(best_k = best_k, scores = scores)
}

#' Parameter sweep utility for sensitivity analysis
#'
#' Executes a function over a vector of parameter values and collates the
#' results for downstream inspection.
#'
#' @param fn Function to execute.
#' @param params Numeric vector of parameter values.
#'
#' @return List of results from each function call.
#' @export
#'
#' @examples
#' sweep <- parameter_sweep(function(x) x^2, 1:3)
parameter_sweep <- function(fn, params) {
  lapply(params, fn)
}

#' K-fold cross-validation helper
#'
#' Splits data into k folds, trains the supplied modelling function on each
#' training set, and evaluates on the holdout fold using the provided metric.
#'
#' @param data Data frame for modelling.
#' @param k Number of folds.
#' @param model_fn Function that accepts training data and returns a model.
#' @param metric_fn Function that accepts a model and validation data and
#'   returns a numeric performance metric.
#'
#' @return Vector of metric values for each fold.
#' @export
#'
#' @examples
#' scores <- cross_validate(mtcars, 5, function(d) lm(mpg ~ wt, d),
#'                          function(m, v) mean((v$mpg - predict(m, v))^2))
cross_validate <- function(data, k = 5, model_fn, metric_fn) {
  n <- nrow(data)
  idx <- sample(rep(1:k, length.out = n))
  scores <- numeric(k)
  for (i in seq_len(k)) {
    train <- data[idx != i, ]
    test <- data[idx == i, ]
    model <- model_fn(train)
    scores[i] <- metric_fn(model, test)
  }
  scores
}

#' Grid search for hyperparameter tuning
#'
#' Evaluates a model function over a grid of parameter combinations and
#' returns the best-performing set according to a scoring function.
#'
#' @param param_grid Named list of vectors representing parameter values.
#' @param model_fn Function that fits a model given a list of parameters.
#' @param score_fn Function that scores the fitted model.
#'
#' @return List with `best_params` and `scores` data frame.
#' @export
#'
#' @examples
#' grid <- list(alpha = c(0.1, 0.5), lambda = c(1, 10))
#' res <- grid_search(grid, lm, function(m) AIC(m))
grid_search <- function(param_grid, model_fn, score_fn) {
  combos <- expand.grid(param_grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  scores <- data.frame()
  best_score <- Inf
  best_params <- NULL
  for (i in seq_len(nrow(combos))) {
    params <- as.list(combos[i, ])
    model <- do.call(model_fn, params)
    sc <- score_fn(model)
    scores <- rbind(scores, cbind(combos[i, ], score = sc))
    if (sc < best_score) {
      best_score <- sc
      best_params <- params
    }
  }
  list(best_params = best_params, scores = scores)
}
