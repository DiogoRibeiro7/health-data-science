test_that("legacy matching helpers warn without changing return behavior", {
  skip_if_not_installed("MatchIt")

  df <- data.frame(
    treat = c(1, 1, 0, 0, 0, 1),
    x = c(2, 1.5, -1, -0.5, 0.2, 1),
    propensity = c(0.8, 0.7, 0.2, 0.3, 0.45, 0.65)
  )

  expect_warning(
    matched <- estimate_causal_effect(treat ~ x, df),
    "deprecated.*fit_propensity_design"
  )
  expect_s3_class(matched, "data.frame")

  expect_warning(
    subclass <- propensity_stratification(treat ~ x, df, subclass = 2),
    "deprecated.*fit_propensity_design"
  )
  expect_true(inherits(subclass, "matchit"))

  expect_warning(
    cohort <- match_cohort(df, caliper = 0.2),
    "deprecated.*fit_propensity_design"
  )
  expect_true(is.data.frame(cohort) || is.matrix(cohort))
})

test_that("legacy IV wrapper warns and still returns ivreg", {
  skip_if_not_installed("AER")

  set.seed(11)
  n <- 200
  z <- stats::rnorm(n)
  x <- 0.9 * z + stats::rnorm(n)
  y <- 1.5 * x + stats::rnorm(n)
  df <- data.frame(y = y, x = x, z = z)

  expect_warning(
    fit <- instrumental_variable(y ~ x | z, df),
    "deprecated.*fit_instrumental_variable"
  )
  expect_true(inherits(fit, "ivreg"))
})

test_that("legacy DiD wrapper warns and still returns fixest", {
  skip_if_not_installed("fixest")

  df <- expand.grid(id = 1:20, time = 0:1)
  df$treated <- as.integer(df$id <= 10)
  df$post <- as.integer(df$time == 1)
  df$y <- 1 + df$treated + df$post + 2 * df$treated * df$post

  expect_warning(
    fit <- difference_in_differences(y ~ treated * post, df),
    "deprecated.*fit_group_time_did"
  )
  expect_true(inherits(fit, "fixest"))
})

test_that("legacy RD wrapper warns and still returns rdrobust", {
  skip_if_not_installed("rdrobust")

  set.seed(12)
  x <- stats::runif(300, -1, 1)
  y <- x + 2 * (x >= 0) + stats::rnorm(300, sd = 0.2)

  expect_warning(
    fit <- regression_discontinuity(y, x, c = 0),
    "deprecated.*fit_regression_discontinuity"
  )
  expect_true(inherits(fit, "rdrobust"))
})
