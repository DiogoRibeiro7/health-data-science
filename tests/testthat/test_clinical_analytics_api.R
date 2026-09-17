test_that("pharmacovigilance_signal returns interpretable metrics", {
  signal <- pharmacovigilance_signal(a = 12, b = 88, c = 20, d = 880)

  expect_equal(nrow(signal), 1)
  expect_true(signal$prr > 1)
  expect_true(signal$ror > 1)
  expect_true(signal$prr_conf_low < signal$prr)
  expect_true(signal$prr_conf_high > signal$prr)
  expect_true(signal$ror_conf_low < signal$ror)
  expect_true(signal$ror_conf_high > signal$ror)
  expect_false(signal$corrected_zero_cells)
  expect_type(signal$evans_screening_flag, "logical")
})

test_that("pharmacovigilance_signal handles zero cells explicitly", {
  signal <- pharmacovigilance_signal(a = 3, b = 0, c = 4, d = 20)

  expect_true(signal$corrected_zero_cells)
  expect_equal(signal$zero_cell_correction, 0.5)
  expect_true(is.finite(signal$prr))
  expect_true(is.finite(signal$ror))
})

test_that("pharmacovigilance count validation rejects invalid tables", {
  expect_error(
    pharmacovigilance_signal(-1, 2, 3, 4),
    "non-negative finite integer counts"
  )
  expect_error(
    pharmacovigilance_signal(0, 0, 3, 4),
    "Each exposure row"
  )
  expect_error(
    pharmacovigilance_signal(0, 2, 0, 4),
    "Each event column"
  )
})

test_that("detect_pharmacovigilance preserves scalar PRR behavior", {
  expected <- (12 / 100) / (20 / 900)
  expect_equal(detect_pharmacovigilance(12, 88, 20, 880), expected)
})

test_that("clinical prediction rule retains model and apparent performance", {
  set.seed(42)
  n <- 400
  x1 <- stats::rnorm(n)
  x2 <- stats::rnorm(n)
  probability <- stats::plogis(-0.4 + 1.1 * x1 - 0.7 * x2)
  y <- stats::rbinom(n, 1, probability)
  data <- data.frame(y = y, x1 = x1, x2 = x2)

  rule <- fit_clinical_prediction_rule(y ~ x1 + x2, data)
  expect_s3_class(rule, "hds_clinical_prediction_rule")
  expect_s3_class(rule$model, "glm")
  expect_equal(rule$metadata$n, n)

  performance <- clinical_prediction_performance(rule)
  expect_equal(nrow(performance), 1)
  expect_true(performance$auc > 0.65)
  expect_true(performance$brier_score >= 0)
  expect_true(performance$brier_score <= 1)
  expect_true(performance$sensitivity >= 0 && performance$sensitivity <= 1)
  expect_true(performance$specificity >= 0 && performance$specificity <= 1)

  tidy <- tidy_clinical_prediction_rule(rule)
  expect_true(all(c(
    "term", "estimate", "std_error", "p_value",
    "odds_ratio", "conf_low", "conf_high"
  ) %in% names(tidy)))
  expect_equal(nrow(tidy), 3)
})

test_that("develop_clinical_prediction_rule preserves coefficient vector", {
  data <- data.frame(
    y = rep(c(0, 1), 20),
    x = rep(c(-1, 1), each = 20)
  )

  coefficients <- develop_clinical_prediction_rule(y ~ x, data)
  expect_type(coefficients, "double")
  expect_named(coefficients)
  expect_true(all(c("(Intercept)", "x") %in% names(coefficients)))
})

test_that("clinical prediction rule validates outcome coding and missingness", {
  bad <- data.frame(y = c(0, 1, 2, 0), x = 1:4)
  expect_error(
    fit_clinical_prediction_rule(y ~ x, bad),
    "must be coded 0/1"
  )

  missing <- data.frame(y = c(0, 1, 0, 1), x = c(1, 2, NA, 4))
  expect_error(
    fit_clinical_prediction_rule(y ~ x, missing),
    "Missing values detected"
  )
  expect_s3_class(
    fit_clinical_prediction_rule(y ~ x, missing, na_action = "omit"),
    "hds_clinical_prediction_rule"
  )
})
