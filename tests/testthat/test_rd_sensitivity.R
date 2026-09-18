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


test_that("RD extraction selects the named Robust inference row", {
  fit <- list(
    coef = matrix(
      c(10, 30, 20),
      ncol = 1,
      dimnames = list(
        c("Conventional", "Robust", "Bias-Corrected"),
        "Estimate"
      )
    ),
    se = matrix(
      c(1, 3, 2),
      ncol = 1,
      dimnames = list(
        c("Conventional", "Robust", "Bias-Corrected"),
        "Std. Err."
      )
    ),
    pv = matrix(
      c(0.1, 0.3, 0.2),
      ncol = 1,
      dimnames = list(
        c("Conventional", "Robust", "Bias-Corrected"),
        "P>|z|"
      )
    ),
    ci = matrix(
      c(
        8, 12,
        24, 36,
        16, 24
      ),
      ncol = 2,
      byrow = TRUE,
      dimnames = list(
        c("Conventional", "Robust", "Bias-Corrected"),
        c("CI Lower", "CI Upper")
      )
    )
  )

  row <- .extract_rd_robust_row(fit, cutoff = 0)

  expect_equal(row$estimate, 30)
  expect_equal(row$std_error, 3)
  expect_equal(row$p_value, 0.3)
  expect_equal(row$conf_low, 24)
  expect_equal(row$conf_high, 36)
  expect_equal(row$inference, "robust_bias_corrected")
})

test_that("fixed placebo bandwidth cannot cross the true cutoff", {
  skip_if_not_installed("rdrobust")

  set.seed(101)
  x <- stats::runif(600, -2, 2)
  y <- x + 2 * (x >= 0) + stats::rnorm(600)
  df <- data.frame(y = y, x = x)
  fit <- fit_regression_discontinuity(df, "y", "x", cutoff = 0)

  expect_error(
    rd_placebo_cutoffs(
      fit,
      cutoffs = c(-0.4, 1),
      bandwidth = 0.5
    ),
    "cannot touch or cross"
  )
})
