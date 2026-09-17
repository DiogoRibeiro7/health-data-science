test_that("doubly robust estimator validates the target estimand", {
  set.seed(1)
  n <- 80
  x <- rnorm(n)
  p <- plogis(0.4 * x)
  a <- rbinom(n, 1, p)
  y <- 2 * a + x + rnorm(n)
  df <- data.frame(a = a, x = x, y = y)

  design_ato <- fit_propensity_design(df, "a", "x", estimand = "ATO")
  expect_error(
    estimate_doubly_robust(design_ato, "y"),
    "only ATE and ATT"
  )
})

test_that("AIPW ATE recovers a known treatment effect", {
  set.seed(42)
  n <- 3000
  x <- rnorm(n)
  z <- rnorm(n)
  p <- plogis(-0.2 + 0.8 * x - 0.5 * z)
  a <- rbinom(n, 1, p)
  y <- 3 + 2 * a + 1.2 * x - 0.7 * z + rnorm(n)
  df <- data.frame(a = a, x = x, z = z, y = y)

  design <- fit_propensity_design(
    df,
    treatment = "a",
    covariates = c("x", "z"),
    estimand = "ATE"
  )
  result <- estimate_doubly_robust(design, "y")

  expect_equal(result$estimand, "ATE")
  expect_lt(abs(result$estimate - 2), 0.15)
  expect_gt(result$std_error, 0)
  expect_lt(result$conf_low, 2)
  expect_gt(result$conf_high, 2)
})

test_that("ATT estimator targets the treated population", {
  set.seed(7)
  n <- 3000
  x <- rnorm(n)
  p <- plogis(-0.3 + x)
  a <- rbinom(n, 1, p)
  y0 <- 1 + x + rnorm(n)
  tau <- 1 + 0.5 * x
  y <- y0 + a * tau
  df <- data.frame(a = a, x = x, y = y)

  truth_att <- mean(tau[a == 1])
  design <- fit_propensity_design(df, "a", "x", estimand = "ATT")
  result <- estimate_doubly_robust(design, "y")

  expect_equal(result$estimand, "ATT")
  expect_lt(abs(result$estimate - truth_att), 0.15)
  expect_equal(result$target_fraction, mean(a), tolerance = 1e-12)
})

test_that("outcome-model covariates can differ from propensity covariates", {
  set.seed(11)
  n <- 1500
  x <- rnorm(n)
  z <- rnorm(n)
  p <- plogis(0.9 * x)
  a <- rbinom(n, 1, p)
  y <- 1.5 * a + x + 2 * z + rnorm(n)
  df <- data.frame(a = a, x = x, z = z, y = y)

  design <- fit_propensity_design(df, "a", "x", estimand = "ATE")
  result <- estimate_doubly_robust(
    design,
    "y",
    outcome_covariates = c("x", "z")
  )

  expect_lt(abs(result$estimate - 1.5), 0.2)
})

test_that("missing outcome handling is explicit", {
  set.seed(3)
  n <- 100
  x <- rnorm(n)
  a <- rbinom(n, 1, plogis(x))
  y <- a + x + rnorm(n)
  y[1] <- NA_real_
  df <- data.frame(a = a, x = x, y = y)

  design <- fit_propensity_design(df, "a", "x", estimand = "ATE")
  expect_error(
    estimate_doubly_robust(design, "y"),
    "Missing values"
  )

  result <- estimate_doubly_robust(design, "y", na_action = "omit")
  expect_equal(result$omitted_rows, 1)
  expect_equal(result$n, n - 1)
})
