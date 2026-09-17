test_that("IV input roles and missingness are validated", {
  df <- data.frame(y = 1:6, d = c(0, 1, 0, 1, 0, 1), z = c(0, 0, 1, 1, 0, 1), x = 1:6)

  expect_error(
    fit_instrumental_variable(df, "y", "d", character()),
    "non-empty"
  )
  expect_error(
    fit_instrumental_variable(df, "y", "d", "d"),
    "distinct roles"
  )

  df$x[1] <- NA
  expect_error(
    fit_instrumental_variable(df, "y", "d", "z", "x"),
    "Missing values"
  )
})

test_that("strong instrument recovers a linear treatment effect", {
  skip_if_not_installed("AER")
  set.seed(42)
  n <- 1500
  x <- rnorm(n)
  z <- rnorm(n)
  u <- rnorm(n)
  d <- 1.4 * z + 0.5 * x + 0.8 * u + rnorm(n, sd = 0.5)
  y <- 2.0 * d + 0.7 * x + u + rnorm(n)
  df <- data.frame(y = y, d = d, z = z, x = x)

  fit <- fit_instrumental_variable(df, "y", "d", "z", "x")
  diag <- instrumental_variable_diagnostics(fit)
  tab <- tidy_instrumental_variable(fit)

  expect_s3_class(fit, "hds_iv_model")
  expect_gt(diag$first_stage_f, 10)
  expect_gt(diag$partial_r_squared, 0)
  expect_false(diag$weak_instrument_flag)
  expect_equal(tab$estimate, 2, tolerance = 0.2)
})

test_that("weak instruments are surfaced rather than hidden", {
  skip_if_not_installed("AER")
  set.seed(7)
  n <- 700
  z <- rnorm(n)
  u <- rnorm(n)
  d <- 0.015 * z + u + rnorm(n)
  y <- 1.5 * d + u + rnorm(n)
  df <- data.frame(y = y, d = d, z = z)

  expect_warning(
    fit <- fit_instrumental_variable(
      df, "y", "d", "z", weak_f_threshold = 10
    ),
    "partial F statistic"
  )
  expect_true(instrumental_variable_diagnostics(fit)$weak_instrument_flag)
})

test_that("overidentified models report a Sargan diagnostic", {
  skip_if_not_installed("AER")
  set.seed(99)
  n <- 1200
  z1 <- rnorm(n)
  z2 <- rnorm(n)
  u <- rnorm(n)
  d <- z1 + 0.8 * z2 + u + rnorm(n)
  y <- 1.2 * d + u + rnorm(n)
  df <- data.frame(y = y, d = d, z1 = z1, z2 = z2)

  fit <- fit_instrumental_variable(df, "y", "d", c("z1", "z2"))
  diag <- instrumental_variable_diagnostics(fit)

  expect_true(diag$overidentified)
  expect_equal(diag$sargan_df, 1)
  expect_true(is.finite(diag$sargan_statistic))
  expect_true(is.finite(diag$sargan_p_value))
})
