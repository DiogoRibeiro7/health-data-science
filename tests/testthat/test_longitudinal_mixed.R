test_that("longitudinal mixed model validates repeated measures", {
  df <- data.frame(
    id = 1:4,
    time = c(0, 0, 0, 0),
    y = c(1, 2, 3, 4)
  )

  expect_error(
    fit_longitudinal_mixed(df, "y", "time", "id"),
    "repeated measurements"
  )
})

test_that("random slopes require repeated distinct time points", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    time = c(0, 0, 0, 0),
    y = c(1, 2, 3, 4)
  )

  expect_error(
    fit_longitudinal_mixed(
      df, "y", "time", "id",
      random = "intercept-slope"
    ),
    "multiple distinct time points"
  )
})

test_that("missing-value handling is explicit", {
  df <- data.frame(
    id = rep(1:3, each = 2),
    time = rep(c(0, 1), 3),
    y = c(1, NA, 2, 3, 4, 5)
  )

  expect_error(
    fit_longitudinal_mixed(df, "y", "time", "id"),
    "Missing values"
  )
})

test_that("random-intercept longitudinal model fits and summarises", {
  skip_if_not_installed("lme4")

  set.seed(42)
  n_subjects <- 20
  df <- data.frame(
    id = rep(seq_len(n_subjects), each = 4),
    time = rep(0:3, n_subjects)
  )
  subject_effect <- rep(stats::rnorm(n_subjects, sd = 0.8), each = 4)
  df$y <- 2 + 0.5 * df$time + subject_effect + stats::rnorm(nrow(df), sd = 0.3)

  fit <- fit_longitudinal_mixed(df, "y", "time", "id")

  expect_s3_class(fit, "hds_longitudinal_mixed")
  expect_s4_class(fit$model, "lmerMod")
  expect_equal(fit$metadata$n_subjects, n_subjects)
  expect_equal(fit$metadata$random, "intercept")

  fixed <- tidy_longitudinal_mixed(fit)
  expect_true(all(c("term", "estimate", "std_error", "conf_low", "conf_high") %in% names(fixed)))
  expect_false("p_value" %in% names(fixed))

  variance <- longitudinal_variance_components(fit)
  expect_true(all(c("group", "variance_or_covariance", "standard_deviation_or_correlation") %in% names(variance)))
})

test_that("random-slope model records structure", {
  skip_if_not_installed("lme4")

  set.seed(99)
  n_subjects <- 24
  id <- rep(seq_len(n_subjects), each = 5)
  time <- rep(0:4, n_subjects)
  intercept <- rep(stats::rnorm(n_subjects, sd = 0.7), each = 5)
  slope <- rep(stats::rnorm(n_subjects, sd = 0.15), each = 5)
  df <- data.frame(
    id = id,
    time = time,
    x = rep(c(0, 1), length.out = length(id))
  )
  df$y <- 5 + 0.4 * time + 0.3 * df$x + intercept + slope * time +
    stats::rnorm(nrow(df), sd = 0.25)

  fit <- suppressWarnings(fit_longitudinal_mixed(
    df,
    outcome = "y",
    time = "time",
    id = "id",
    covariates = "x",
    random = "intercept-slope",
    correlated = FALSE,
    estimation = "ML"
  ))

  expect_equal(fit$metadata$random, "intercept-slope")
  expect_false(fit$metadata$correlated)
  expect_equal(fit$metadata$estimation, "ML")
  expect_true(is.logical(fit$metadata$singular))
})
