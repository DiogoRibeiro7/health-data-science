test_that("run_mcmc validates explicit sampling controls before compilation", {
  code <- "parameters { real y; } model { y ~ normal(0, 1); }"

  expect_error(
    run_mcmc(code, list(), iter = 100, warmup = 100),
    "smaller than"
  )
  expect_error(
    run_mcmc(code, list(x = 1), chains = 0),
    "positive integer"
  )
  expect_error(
    run_mcmc(code, list(x = 1), adapt_delta = 1),
    "strictly between"
  )
  expect_error(
    run_mcmc(code, list(1)),
    "must be named"
  )
})

test_that("bayesian_model_averaging validates family and missingness", {
  data <- mtcars[, c("mpg", "wt", "hp")]
  data$wt[1] <- NA_real_

  expect_error(
    bayesian_model_averaging(
      mpg ~ wt + hp,
      data,
      family = stats::gaussian(),
      na_action = "fail"
    ),
    "Missing values"
  )

  expect_error(
    bayesian_model_averaging(
      mpg ~ wt + hp,
      data,
      family = 42,
      na_action = "omit"
    ),
    "family"
  )
})

test_that("tidy_bayesian_model_average produces inclusion and posterior summaries", {
  fake <- structure(
    list(
      probne0 = c(alpha = 80, beta = 20),
      postmean = c(alpha = 1.2, beta = -0.1),
      postsd = c(alpha = 0.3, beta = 0.2),
      condpostmean = c(alpha = 1.5, beta = -0.5),
      condpostsd = c(alpha = 0.25, beta = 0.4)
    ),
    class = "bic.glm"
  )

  tidy <- tidy_bayesian_model_average(fake)

  expect_equal(tidy$term, c("alpha", "beta"))
  expect_equal(tidy$inclusion_probability, c(0.8, 0.2))
  expect_true(all(c(
    "posterior_mean",
    "posterior_sd",
    "conditional_posterior_mean",
    "conditional_posterior_sd"
  ) %in% names(tidy)))
})

test_that("fit_hierarchical_bayes requires group-specific structure before fitting", {
  expect_error(
    fit_hierarchical_bayes(
      mpg ~ wt,
      mtcars,
      iter = 100,
      warmup = 50,
      chains = 2
    ),
    "group-specific"
  )

  expect_error(
    fit_hierarchical_bayes(
      mpg ~ wt + (1 | cyl),
      transform(mtcars, wt = replace(wt, 1, NA_real_)),
      iter = 100,
      warmup = 50,
      chains = 2,
      na_action = "fail"
    ),
    "Missing values"
  )
})

test_that("posterior summary and diagnostics reject unsupported objects", {
  fit <- stats::lm(mpg ~ wt, data = mtcars)

  expect_error(
    tidy_bayesian_posterior(fit),
    "stanfit"
  )
  expect_error(
    bayesian_sampling_diagnostics(fit),
    "stanfit"
  )
})

test_that("posterior predictive APIs distinguish PPCs from new-data draws", {
  fit <- stats::lm(mpg ~ wt, data = mtcars)

  expect_error(
    posterior_predictive_draws(fit),
    "stanreg"
  )
  expect_error(
    posterior_predictive_check(fit),
    "stanreg"
  )
})
