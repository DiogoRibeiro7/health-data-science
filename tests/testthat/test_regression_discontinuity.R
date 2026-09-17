test_that("RD inputs are validated", {
  df <- data.frame(y = 1:6, x = c(-3, -2, -1, 1, 2, 3))

  expect_error(
    fit_regression_discontinuity(df, "y", "y"),
    "different"
  )
  expect_error(
    fit_regression_discontinuity(df[df$x > 0, ], "y", "x"),
    "both sides"
  )
  expect_error(
    fit_regression_discontinuity(df, "y", "x", p = 2, q = 2),
    "greater than"
  )
})

test_that("sharp RD recovers a cutoff jump", {
  skip_if_not_installed("rdrobust")
  set.seed(401)

  n <- 1500
  x <- stats::runif(n, -1, 1)
  y <- 1 + 2 * x + 3 * (x >= 0) + stats::rnorm(n, sd = 0.5)
  z <- 0.5 * x + stats::rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x = x, z = z)

  fit <- fit_regression_discontinuity(
    df,
    outcome = "y",
    running = "x",
    cutoff = 0,
    covariates = "z"
  )

  expect_s3_class(fit, "hds_rd_model")
  expect_equal(fit$metadata$estimand, "sharp RD treatment effect at the cutoff")
  expect_equal(fit$metadata$n_left + fit$metadata$n_right, n)

  tab <- tidy_regression_discontinuity(fit)
  expect_true(all(c("estimate", "std_error", "p_value", "conf_low", "conf_high") %in% names(tab)))
  expect_true(any(abs(tab$estimate - 3) < 0.6))
})

test_that("RD covariate continuity uses the fitted cutoff", {
  skip_if_not_installed("rdrobust")
  set.seed(402)

  n <- 1200
  x <- stats::runif(n, -1, 1)
  y <- 2 * (x >= 0) + x + stats::rnorm(n)
  balanced <- x + stats::rnorm(n)
  shifted <- x + 1.2 * (x >= 0) + stats::rnorm(n, sd = 0.4)
  df <- data.frame(y = y, x = x, balanced = balanced, shifted = shifted)

  fit <- fit_regression_discontinuity(df, "y", "x")
  diagnostic <- rd_covariate_balance(fit, c("balanced", "shifted"))

  expect_equal(nrow(diagnostic), 2)
  expect_true(all(c("variable", "estimate", "std_error", "p_value") %in% names(diagnostic)))
  expect_gt(abs(diagnostic$estimate[diagnostic$variable == "shifted"]), 0.5)
})

test_that("RD density diagnostic returns the raw rddensity result", {
  skip_if_not_installed("rdrobust")
  skip_if_not_installed("rddensity")
  set.seed(403)

  x <- stats::runif(1200, -1, 1)
  y <- x + 2 * (x >= 0) + stats::rnorm(1200)
  fit <- fit_regression_discontinuity(data.frame(y = y, x = x), "y", "x")
  density <- rd_density_diagnostic(fit)

  expect_s3_class(density, "hds_rd_density_diagnostic")
  expect_false(is.null(density$raw))
  if (is.finite(density$p_value)) {
    expect_gte(density$p_value, 0)
    expect_lte(density$p_value, 1)
  }
})
