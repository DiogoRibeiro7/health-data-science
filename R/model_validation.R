#' Model validation utilities
#'
#' Collection of functions to assess statistical models and latent class analyses.
#'
#' @section Regression diagnostics:
#' * `analyze_residuals()` - return residual diagnostics for fitted models
#' * `goodness_of_fit()` - provide AIC/BIC and optional likelihood ratio test
#' * `bootstrap_ci()` - compute bootstrap confidence intervals for a model coefficient
#' * `kfold_cv()` - perform k-fold cross validation for a modelling function
#'
#' @section Latent class diagnostics:
#' * `posterior_probabilities()` - extract posterior probabilities for each observation
#' * `class_assignment_metrics()` - compute entropy and average posterior probabilities
#' * `plot_profiles()` - visualize latent class profiles using ggplot2
#' * `lca_stability()` - fit an LCA model across multiple seeds and summarise stability
#'
#' @name model_validation
NULL

#' Residual diagnostics for regression models
#'
#' Returns residuals, standardized residuals, and fitted values for a model.
#'
#' @param model Fitted regression model.
#' @return A data frame with fitted values and residuals.
#' @examples
#' mod <- stats::lm(mpg ~ cyl, data = mtcars)
#' analyze_residuals(mod)
analyze_residuals <- function(model) {
  res <- stats::residuals(model)
  std_res <- stats::rstandard(model)
  fit <- stats::fitted(model)
  data.frame(fitted = fit, residual = res, std_residual = std_res)
}

#' Model goodness-of-fit statistics
#'
#' Computes AIC, BIC and optionally performs a likelihood ratio test
#' comparing two nested models.
#'
#' @param model Fitted model object.
#' @param model_null Optional null model for likelihood ratio test.
#' @return A list with AIC, BIC and, when `model_null` is supplied, the LRT p-value.
#' @examples
#' full <- stats::glm(vs ~ mpg + cyl, data = mtcars, family = binomial())
#' null <- stats::glm(vs ~ 1, data = mtcars, family = binomial())
#' goodness_of_fit(full, null)
goodness_of_fit <- function(model, model_null = NULL) {
  out <- list(AIC = stats::AIC(model), BIC = stats::BIC(model))
  if (!is.null(model_null)) {
    lr <- stats::anova(model_null, model, test = "LRT")
    out$LRT_p <- lr$"Pr(>Chi)"[2]
  }
  out
}

#' Bootstrap confidence intervals for model coefficients
#'
#' @param model Fitted model.
#' @param param Name of coefficient to bootstrap.
#' @param R Number of bootstrap resamples.
#' @return A numeric vector with lower and upper 95% confidence interval.
#' @examples
#' mod <- stats::lm(mpg ~ cyl, data = mtcars)
#' bootstrap_ci(mod, "cyl", R = 100)
bootstrap_ci <- function(model, param, R = 1000) {
  stopifnot(requireNamespace("boot", quietly = TRUE))
  coef_index <- which(names(stats::coef(model)) == param)
  boot_fun <- function(data, idx) {
    mod <- stats::update(model, data = data[idx, ])
    stats::coef(mod)[coef_index]
  }
  b <- boot::boot(model$model, boot_fun, R = R)
  boot::boot.ci(b, type = "perc")$percent[4:5]
}

#' k-fold cross validation for arbitrary modelling functions
#'
#' @param data Data frame.
#' @param k Number of folds.
#' @param fit_fn Function that takes `train_data` and returns a model.
#' @param pred_fn Function that takes a fitted model and `test_data` and returns predictions.
#' @return Vector of holdout errors.
#' @examples
#' kfold_cv(mtcars, k = 5,
#'   fit_fn = function(d) stats::lm(mpg ~ cyl, data = d),
#'   pred_fn = function(mod, d) stats::predict(mod, newdata = d))
kfold_cv <- function(data, k = 5, fit_fn, pred_fn) {
  n <- nrow(data)
  folds <- sample(rep(1:k, length.out = n))
  errs <- numeric(k)
  for (i in seq_len(k)) {
    train <- data[folds != i, ]
    test  <- data[folds == i, ]
    mod <- fit_fn(train)
    preds <- pred_fn(mod, test)
    errs[i] <- mean((test[[1]] - preds)^2)
  }
  errs
}

#' Extract posterior probabilities from a poLCA model
#'
#' @param lc_model Fitted `poLCA` model.
#' @return Data frame of posterior probabilities per class.
#' @examples
#' # Not run: posterior_probabilities(lca_mod)
posterior_probabilities <- function(lc_model) {
  as.data.frame(lc_model$posterior)
}

#' Class assignment quality metrics
#'
#' Computes entropy and the average maximum posterior probability.
#'
#' @param lc_model Fitted `poLCA` model.
#' @return Named vector with entropy and avg_posterior.
#' @examples
#' # Not run: class_assignment_metrics(lca_mod)
class_assignment_metrics <- function(lc_model) {
  post <- lc_model$posterior
  max_post <- apply(post, 1, max)
  entropy <- -mean(rowSums(post * log(post)))
  c(entropy = entropy, avg_posterior = mean(max_post))
}

#' Plot latent class profiles
#'
#' @param lc_model Fitted `poLCA` model.
#' @return ggplot object of class profiles.
#' @examples
#' # Not run: plot_profiles(lca_mod)
plot_profiles <- function(lc_model) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  df <- as.data.frame(lc_model$probs)
  df$variable <- rownames(df)
  long <- tidyr::pivot_longer(df, -variable, names_to = "class", values_to = "prob")
  ggplot2::ggplot(long, ggplot2::aes(variable, prob, colour = class, group = class)) +
    ggplot2::geom_line() +
    ggplot2::labs(y = "Probability", x = NULL)
}

#' Assess LCA model stability across random seeds
#'
#' Fits the same LCA specification across multiple seeds and returns a
#' data frame with the resulting log-likelihoods.
#'
#' @param formula LCA formula.
#' @param data Data frame for fitting the model.
#' @param nclass Number of classes.
#' @param seeds Numeric vector of seeds.
#' @return Data frame with seed and log-likelihood.
#' @examples
#' # Not run: lca_stability(f, df, 3, 1:5)
lca_stability <- function(formula, data, nclass, seeds) {
  stopifnot(requireNamespace("poLCA", quietly = TRUE))
  res <- lapply(seeds, function(s) {
    set.seed(s)
    mod <- poLCA::poLCA(formula, data, nclass = nclass, verbose = FALSE, maxiter = 1000)
    data.frame(seed = s, logLik = mod$llik)
  })
  do.call(rbind, res)
}
