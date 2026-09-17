test_that("propensity design validates treatment and covariates", {
  df <- data.frame(
    treat = c(1, 0, 1, 0),
    x = c(0.2, -0.1, 0.5, -0.4)
  )

  expect_error(
    fit_propensity_design(df, "missing", "x"),
    "treatment"
  )
  expect_error(
    fit_propensity_design(df, "treat", character()),
    "non-empty"
  )
})

test_that("ATE weights follow inverse-probability formulas", {
  set.seed(1)
  n <- 120
  x <- stats::rnorm(n)
  p <- stats::plogis(0.3 + 0.7 * x)
  treat <- stats::rbinom(n, 1, p)
  df <- data.frame(treat = treat, x = x)

  design <- fit_propensity_design(df, "treat", "x", estimand = "ATE")
  ps <- design$propensity_score
  expected <- ifelse(design$treatment == 1L, 1 / ps, 1 / (1 - ps))

  expect_equal(design$weights, expected, tolerance = 1e-10)
  expect_equal(design$metadata$estimand, "ATE")
})

test_that("ATT weights target the treated population", {
  set.seed(2)
  n <- 120
  x <- stats::rnorm(n)
  p <- stats::plogis(-0.2 + 0.8 * x)
  treat <- stats::rbinom(n, 1, p)
  df <- data.frame(treat = treat, x = x)

  design <- fit_propensity_design(df, "treat", "x", estimand = "ATT")
  ps <- design$propensity_score
  expected <- ifelse(design$treatment == 1L, 1, ps / (1 - ps))

  expect_equal(design$weights, expected, tolerance = 1e-10)
  expect_true(all(design$weights[design$treatment == 1L] == 1))
})

test_that("ATO overlap weights remain bounded by one", {
  set.seed(3)
  n <- 150
  x <- stats::rnorm(n)
  p <- stats::plogis(0.1 + x)
  treat <- stats::rbinom(n, 1, p)
  df <- data.frame(treat = treat, x = x)

  design <- fit_propensity_design(df, "treat", "x", estimand = "ATO")
  ps <- design$propensity_score
  expected <- ifelse(design$treatment == 1L, 1 - ps, ps)

  expect_equal(design$weights, expected, tolerance = 1e-10)
  expect_true(all(design$weights > 0 & design$weights < 1))
})

test_that("balance table uses model-matrix covariates", {
  set.seed(4)
  n <- 300
  x <- stats::rnorm(n)
  z <- factor(stats::rbinom(n, 1, 0.5), labels = c("A", "B"))
  p <- stats::plogis(-0.3 + 1.2 * x + 0.8 * (z == "B"))
  treat <- stats::rbinom(n, 1, p)
  df <- data.frame(treat = treat, x = x, z = z)

  design <- fit_propensity_design(df, "treat", c("x", "z"), estimand = "ATO")
  balance <- propensity_balance(design)

  expect_true(all(c(
    "term", "unweighted_smd", "weighted_smd", "balanced"
  ) %in% names(balance)))
  expect_true(any(grepl("z", balance$term)))
  expect_true(mean(balance$abs_weighted_smd, na.rm = TRUE) <=
              mean(balance$abs_unweighted_smd, na.rm = TRUE))
})

test_that("overlap diagnostics expose support and effective sample size", {
  set.seed(5)
  n <- 180
  x <- stats::rnorm(n)
  p <- stats::plogis(0.2 + 0.6 * x)
  treat <- stats::rbinom(n, 1, p)
  df <- data.frame(treat = treat, x = x)

  design <- fit_propensity_design(df, "treat", "x", estimand = "ATE")
  overlap <- propensity_overlap(design)

  expect_equal(nrow(overlap), 1)
  expect_true(overlap$treated_ess > 0)
  expect_true(overlap$control_ess > 0)
  expect_true(overlap$common_support_lower <= overlap$common_support_upper)
})

test_that("weighted marginal effect returns explicit fixed-weight inference", {
  set.seed(6)
  n <- 250
  x <- stats::rnorm(n)
  p <- stats::plogis(-0.1 + 0.7 * x)
  treat <- stats::rbinom(n, 1, p)
  y <- 2 * treat + 0.8 * x + stats::rnorm(n)
  df <- data.frame(treat = treat, x = x, y = y)

  design <- fit_propensity_design(df, "treat", "x", estimand = "ATE")
  effect <- estimate_propensity_effect(design, "y")

  expect_equal(nrow(effect), 1)
  expect_true(all(c(
    "estimate", "std_error", "conf_low", "conf_high",
    "variance_assumption"
  ) %in% names(effect)))
  expect_gt(effect$estimate, 1)
})
