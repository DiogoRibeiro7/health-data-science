test_that("delta sensitivity validates inputs", {
  df <- data.frame(y = c(1, NA, 3, 4), x = 1:4)
  analysis <- function(d) {
    fit <- stats::lm(y ~ x, data = d)
    c(estimate = stats::coef(fit)[["x"]],
      std_error = summary(fit)$coefficients["x", "Std. Error"])
  }

  expect_error(
    run_delta_sensitivity(df, "missing", c(0, 1), analysis),
    "outcome"
  )
  expect_error(
    run_delta_sensitivity(transform(df, y = 1:4), "y", c(0, 1), analysis),
    "no missing values"
  )
  expect_error(
    run_delta_sensitivity(df, "y", c(0, 0), analysis),
    "duplicates"
  )
})

test_that("delta shifts only originally missing outcome values", {
  skip_if_not_installed("mice")

  df <- data.frame(
    y = c(10, NA, 12, NA, 14, 15, 16, 17),
    x = 0:7
  )
  observed_y <- df$y[!is.na(df$y)]

  analysis <- function(d) {
    expect_equal(d$y[!is.na(df$y)], observed_y)
    fit <- stats::lm(y ~ x, data = d)
    c(
      estimate = stats::coef(fit)[["x"]],
      std_error = summary(fit)$coefficients["x", "Std. Error"]
    )
  }

  out <- run_delta_sensitivity(
    df,
    outcome = "y",
    deltas = c(-2, 0, 2),
    analysis = analysis,
    m = 3,
    maxit = 2,
    seed = 42
  )

  expect_s3_class(out, "hds_delta_sensitivity")
  expect_equal(out$results$delta, c(-2, 0, 2))
  expect_equal(out$metadata$n_missing_outcome, 2)
  expect_equal(out$metadata$n_delta_target, 2)
  expect_true(all(is.finite(out$results$estimate)))
  expect_true(all(out$results$std_error > 0))
})

test_that("group-specific delta adjustment targets only matching missing rows", {
  skip_if_not_installed("mice")

  df <- data.frame(
    y = c(10, NA, 12, NA, 14, NA, 16, 17),
    x = 0:7,
    arm = factor(c("A", "A", "B", "B", "A", "B", "A", "B"))
  )

  analysis <- function(d) {
    fit <- stats::lm(y ~ x + arm, data = d)
    c(
      estimate = stats::coef(fit)[["armB"]],
      std_error = summary(fit)$coefficients["armB", "Std. Error"]
    )
  }

  out <- run_delta_sensitivity(
    df,
    outcome = "y",
    deltas = c(-1, 0, 1),
    analysis = analysis,
    m = 3,
    maxit = 2,
    seed = 7,
    group = "arm",
    group_value = "B"
  )

  expect_equal(out$metadata$n_missing_outcome, 3)
  expect_equal(out$metadata$n_delta_target, 2)
  expect_equal(out$metadata$group$column, "arm")
  expect_equal(out$metadata$group$values, "B")
})

test_that("analysis contract is explicit", {
  skip_if_not_installed("mice")

  df <- data.frame(y = c(1, NA, 3, 4, 5, 6), x = 1:6)

  expect_error(
    run_delta_sensitivity(
      df,
      outcome = "y",
      deltas = 0,
      analysis = function(d) mean(d$y),
      m = 2,
      maxit = 1
    ),
    "estimate.*std_error"
  )
})

test_that("tipping point is selected from evaluated grid", {
  sensitivity <- structure(
    list(
      results = data.frame(
        delta = c(-2, -1, 0, 1, 2),
        estimate = c(-0.5, 0.1, 0.5, 0.8, 1.0),
        std_error = rep(0.2, 5),
        statistic = NA_real_,
        p_value = NA_real_,
        conf_low = c(-0.9, -0.3, 0.1, 0.4, 0.6),
        conf_high = c(-0.1, 0.5, 0.9, 1.2, 1.4),
        within_variance = NA_real_,
        between_variance = NA_real_,
        total_variance = NA_real_,
        degrees_freedom = NA_real_,
        fraction_missing_information = NA_real_
      ),
      metadata = list()
    ),
    class = "hds_delta_sensitivity"
  )

  confidence_tip <- find_delta_tipping_point(
    sensitivity,
    null = 0,
    direction = "lower",
    criterion = "confidence"
  )
  expect_equal(confidence_tip$baseline_state, "above")
  expect_equal(confidence_tip$tipping_delta, -1)
  expect_equal(confidence_tip$tipping_state, "includes")

  estimate_tip <- find_delta_tipping_point(
    sensitivity,
    null = 0,
    direction = "lower",
    criterion = "estimate"
  )
  expect_equal(estimate_tip$tipping_delta, -2)
  expect_equal(estimate_tip$tipping_state, "below")
})

test_that("tipping analysis reports no evaluated change", {
  sensitivity <- structure(
    list(
      results = data.frame(
        delta = c(-1, 0, 1),
        estimate = c(1, 1.1, 1.2),
        conf_low = c(0.5, 0.6, 0.7),
        conf_high = c(1.5, 1.6, 1.7)
      ),
      metadata = list()
    ),
    class = "hds_delta_sensitivity"
  )

  tip <- find_delta_tipping_point(sensitivity)
  expect_true(is.na(tip$tipping_delta))
  expect_true(is.na(tip$tipping_state))
})
