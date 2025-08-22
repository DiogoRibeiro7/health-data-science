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
optimal_lca_classes <- function(df_full, df_subset, k_range = 2:10,
                                parallel = FALSE, early_stop = FALSE,
                                patience = 1) {
  ensure_packages("poLCA")
  worker <- function(k) {
    model <- poLCA::poLCA(
      with(df_subset, cbind(race3, hispanic3, urbandwell, age4, SES, insurancetype) ~ 1),
      df_full, nclass = k, verbose = FALSE
    )
    data.frame(k = k, BIC = model$bic, AIC = model$aic)
  }
  scores <- data.frame()
  if (parallel) {
    res <- parallel::mclapply(k_range, worker, mc.cores = max(1, parallel::detectCores() - 1))
    scores <- do.call(rbind, res)
  } else {
    best_bic <- Inf
    worse <- 0
    for (k in k_range) {
      sc <- worker(k)
      scores <- rbind(scores, sc)
      if (early_stop) {
        if (sc$BIC < best_bic) {
          best_bic <- sc$BIC
          worse <- 0
        } else {
          worse <- worse + 1
          if (worse >= patience) break
        }
      }
    }
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
parameter_sweep <- function(fn, params, parallel = FALSE) {
  if (parallel) {
    parallel::mclapply(params, fn, mc.cores = max(1, parallel::detectCores() - 1))
  } else {
    lapply(params, fn)
  }
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
cross_validate <- function(data, k = 5, model_fn, metric_fn, parallel = FALSE) {
  n <- nrow(data)
  idx <- sample(rep(1:k, length.out = n))
  worker <- function(i) {
    train <- data[idx != i, ]
    test <- data[idx == i, ]
    model <- model_fn(train)
    metric_fn(model, test)
  }
  if (parallel) {
    unlist(parallel::mclapply(seq_len(k), worker, mc.cores = max(1, parallel::detectCores() - 1)))
  } else {
    vapply(seq_len(k), worker, numeric(1))
  }
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
grid_search <- function(param_grid, model_fn, score_fn, parallel = FALSE) {
  combos <- expand.grid(param_grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  worker <- function(i) {
    params <- as.list(combos[i, ])
    model <- do.call(model_fn, params)
    c(params, score = score_fn(model))
  }
  if (parallel) {
    res <- parallel::mclapply(seq_len(nrow(combos)), worker, mc.cores = max(1, parallel::detectCores() - 1))
    scores <- as.data.frame(do.call(rbind, res))
  } else {
    scores <- as.data.frame(do.call(rbind, lapply(seq_len(nrow(combos)), worker)))
  }
  scores$score <- as.numeric(scores$score)
  best_idx <- which.min(scores$score)
  list(best_params = as.list(scores[best_idx, names(param_grid)]), scores = scores)
}

#' Automated LCA model selection with cross-validation
#'
#' Performs a grid search over candidate class counts for a latent class analysis
#' model. Each candidate is evaluated using k-fold cross-validation and the
#' Bayesian Information Criterion (BIC). The model with the lowest average BIC
#' across folds is selected.
#'
#' @param df_full Data frame containing all variables required by the model.
#' @param df_subset Data frame produced by [`select_lca_variables()`] used for the
#'   formula specification.
#' @param k_range Integer vector of class counts to evaluate.
#' @param folds Number of cross-validation folds.
#' @param parallel Logical; whether to parallelise the inner cross-validation
#'   loop.
#'
#' @return List with `best_k` and a data frame of `scores` for each candidate.
#' @export
#'
#' @examples
#' sel <- auto_select_lca(df, subset, k_range = 2:6, folds = 3)
auto_select_lca <- function(df_full, df_subset, k_range = 2:10,
                            folds = 5, parallel = FALSE) {
  ensure_packages("poLCA")
  eff <- with(df_subset, cbind(race3, hispanic3, urbandwell, age4, SES, insurancetype) ~ 1)
  score_k <- function(k) {
    model_fn <- function(train) {
      poLCA::poLCA(eff, train, nclass = k, verbose = FALSE)
    }
    metrics <- cross_validate(df_full, k = folds, model_fn = model_fn,
                              metric_fn = function(m, v) m$bic,
                              parallel = parallel)
    data.frame(k = k, BIC = mean(metrics))
  }
  scores <- do.call(rbind, lapply(k_range, score_k))
  best_k <- scores$k[which.min(scores$BIC)]
  list(best_k = best_k, scores = scores)
}
