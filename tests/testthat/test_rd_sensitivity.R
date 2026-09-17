test_that("RD bandwidth sensitivity validates its grid", {
  expect_error(
    rd_bandwidth_sensitivity(list(), c(0.5, 1)),
    "fit_regression_discontinuity"
  )
})

test_that("RD sensitivity recovers stable estimates around a known jump", {
  skip_if_not_installed("rdrobust")

  set.seed(2026)
  n <- 1200
  x <- stats::runif(n, -2, 2)
  y <- 1 + 0.6 * x + 2 * (x >= 0) + stats::rnorm(n, sd = 0.7)
  z <- 0.4 * x + stats::rnorm(n)
  df <- data.frame(y = y, x = x, z = z)

  fit <- fit_regression_discontinuity(
    df,
    outcome = "y",
    running = "x",
    cutoff = 0,
    covariates = "z"
  )

  sensitivity <- rd_bandwidth_sensitivity(fit, c(0.5, 0.75, 1.0))
  expect_equal(sensitivity$bandwidth, c(0.5, 0.75, 1.0))
  expect_true(all(sensitivity$n_left_within_bandwidth > 0))
  expect_true(all(sensitivity$n_right_within_bandwidth > 0))
  expect_true(all(abs(sensitivity$estimate - 2) < 0.8))
  expect_true(all(sensitivity$conf_low < sensitivity$conf_high))
})

test_that("placebo cutoffs exclude the true cutoff and support boundaries", {
  skip_if_not_installed("rdrobust")

  set.seed(7)
  x <- stats::runif(500, -2, 2)
  y <- x + 1.5 * (x >= 0) + stats::rnorm(500)
  df <- data.frame(y = y, x = x)
  fit <- fit_regression_discontinuity(df, "y", "x", cutoff = 0)

  expect_error(rd_placebo_cutoffs(fit, c(-1, 0, 1)), "true RD cutoff")
  expect_error(rd_placebo_cutoffs(fit, 3), "strictly inside")
})

test_that("placebo cutoffs remain small away from the true threshold", {
  skip_if_not_installed("rdrobust")

  set.seed(99)
  n <- 2000
  x <- stats::runif(n, -3, 3)
  y <- 2 + 0.5 * x + 2.5 * (x >= 0) + stats::rnorm(n, sd = 0.6)
  df <- data.frame(y = y, x = x)

  fit <- fit_regression_discontinuity(df, "y", "x", cutoff = 0)
  placebo <- rd_placebo_cutoffs(fit, c(-1.5, -1, 1, 1.5), bandwidth = 0.5)

  expect_equal(placebo$cutoff, c(-1.5, -1, 1, 1.5))
  expect_true(all(abs(placebo$estimate) < 1.2))
  expect_true(all(placebo$conf_low < placebo$conf_high))
})
