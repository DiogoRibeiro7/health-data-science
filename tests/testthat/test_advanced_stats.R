context("advanced statistics")

skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(paste("Package", pkg, "not installed"))
  }
}

test_that("cost_effectiveness computes ICER", {
  expect_equal(cost_effectiveness(200, 0.8, 150, 0.6), 250)
})

test_that("detect_pharmacovigilance computes PRR", {
  prr <- detect_pharmacovigilance(10, 20, 5, 50)
  expect_gt(prr, 0)
})

test_that("train_rf_missing runs when ranger available", {
  skip_if_not_installed("ranger")
  df <- data.frame(y = c(1, 2, NA, 3), x1 = c(4, 5, 6, 7))
  fit <- train_rf_missing(df, "y")
  expect_true(inherits(fit, "ranger"))
})

test_that("fit_competing_risks validates inputs", {
  expect_error(
    fit_competing_risks(c(1, -1), c(1, 0), c(1, 2)),
    "negative"
  )
  expect_error(
    fit_competing_risks(c(1, 2), c(1, 0), data.frame(group = factor(c("a", "b")))),
    "numeric"
  )
  expect_error(
    fit_competing_risks(c(1, 2), c(1, 0), c(1, 2), event_code = 0, censor_code = 0),
    "different"
  )
  expect_error(
    fit_competing_risks(c(1, 2), c(0, 2), c(1, 2), event_code = 1),
    "not observed"
  )
})

test_that("fit_competing_risks handles missing values explicitly", {
  expect_error(
    fit_competing_risks(c(1, NA, 3), c(1, 0, 2), c(2, 3, 4)),
    "Missing values"
  )
})

test_that("fit_competing_risks fits arbitrary event coding", {
  skip_if_not_installed("cmprsk")

  set.seed(42)
  n <- 120
  time <- stats::rexp(n, rate = 0.15)
  status <- sample(c(7, 9, 12), n, replace = TRUE, prob = c(0.35, 0.4, 0.25))
  covariates <- data.frame(
    age = stats::rnorm(n, 65, 8),
    treatment = stats::rbinom(n, 1, 0.5)
  )
  time[5] <- NA

  fit <- fit_competing_risks(
    time,
    status,
    covariates,
    event_code = 9,
    censor_code = 7,
    na_action = "omit"
  )

  expect_true(inherits(fit, "crr"))
  metadata <- attr(fit, "healthdatascience")
  expect_equal(metadata$event_code, 9)
  expect_equal(metadata$censor_code, 7)
  expect_equal(metadata$competing_codes, 12)
  expect_equal(metadata$n, n - 1)
  expect_equal(metadata$omitted_rows, 1)
  expect_equal(metadata$covariates, c("age", "treatment"))
})

test_that("tidy_competing_risks returns interpretable Fine-Gray estimates", {
  skip_if_not_installed("cmprsk")

  set.seed(123)
  n <- 150
  time <- stats::rexp(n, rate = 0.1)
  status <- sample(c(0, 1, 2), n, replace = TRUE, prob = c(0.3, 0.45, 0.25))
  covariates <- cbind(
    age = stats::rnorm(n, 60, 10),
    exposed = stats::rbinom(n, 1, 0.4)
  )

  fit <- fit_competing_risks(time, status, covariates)
  result <- tidy_competing_risks(fit)

  expect_equal(result$term, c("age", "exposed"))
  expect_equal(nrow(result), 2)
  expect_true(all(result$subdistribution_hazard_ratio > 0))
  expect_true(all(result$conf_low <= result$subdistribution_hazard_ratio))
  expect_true(all(result$conf_high >= result$subdistribution_hazard_ratio))
  expect_true(all(result$p_value >= 0 & result$p_value <= 1))
})

test_that("tidy_competing_risks rejects invalid inputs", {
  expect_error(tidy_competing_risks(stats::lm(mpg ~ wt, mtcars)), "inherit")
  expect_error(
    tidy_competing_risks(structure(list(coef = 1, var = matrix(1)), class = "crr"), conf_level = 1),
    "strictly between"
  )
})
