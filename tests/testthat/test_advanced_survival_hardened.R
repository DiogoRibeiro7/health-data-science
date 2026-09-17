test_that("time-varying Cox requires an explicit tt term", {
  skip_if_not_installed("survival")
  df <- data.frame(
    time = c(1, 2, 3, 4, 5, 6),
    status = c(1, 0, 1, 0, 1, 0),
    x = c(0.2, 0.5, 0.7, 1.0, 1.1, 1.4)
  )

  expect_error(
    fit_time_varying_cox(survival::Surv(time, status) ~ x, df),
    "tt"
  )
})

test_that("time-varying Cox records fitted-sample metadata", {
  skip_if_not_installed("survival")
  set.seed(101)
  n <- 80
  df <- data.frame(
    time = rexp(n, 0.1) + 0.1,
    status = rbinom(n, 1, 0.7),
    x = rnorm(n)
  )
  df$x[1] <- NA_real_

  fit <- suppressWarnings(fit_time_varying_cox(
    survival::Surv(time, status) ~ tt(x),
    df,
    tt = function(x, t, ...) x * log(t + 1),
    na_action = "omit"
  ))

  meta <- attr(fit, "healthdatascience")
  expect_s3_class(fit, "coxph")
  expect_equal(meta$model_type, "time-varying-cox")
  expect_equal(meta$omitted_rows, 1)
  expect_true(meta$tt_supplied)
  expect_equal(meta$n, n - 1)
})

test_that("frailty Cox requires a frailty term and stores metadata", {
  skip_if_not_installed("survival")
  set.seed(102)
  id <- rep(seq_len(30), each = 2)
  df <- data.frame(
    id = id,
    time = rexp(length(id), 0.15) + 0.1,
    status = rbinom(length(id), 1, 0.65),
    x = rnorm(length(id))
  )

  expect_error(
    fit_frailty_cox(survival::Surv(time, status) ~ x, df),
    "frailty"
  )

  fit <- suppressWarnings(fit_frailty_cox(
    survival::Surv(time, status) ~ x + frailty(id),
    df
  ))
  meta <- attr(fit, "healthdatascience")

  expect_s3_class(fit, "coxph")
  expect_equal(meta$model_type, "frailty-cox")
  expect_equal(meta$n, nrow(df))
  expect_true(meta$event_count >= 0)
})

test_that("landmark Cox filters the risk set and can reset time", {
  skip_if_not_installed("survival")
  df <- data.frame(
    followup = c(1, 2, 3, 4, 5, 6, 7, 8),
    status = c(1, 0, 1, 0, 1, 0, 1, 0),
    x = c(-1.0, -0.5, 0.0, 0.4, 0.8, 1.0, 1.3, 1.5)
  )

  fit <- suppressWarnings(landmark_cox(
    survival::Surv(followup, status) ~ x,
    df,
    landmark = 4,
    time = "followup"
  ))
  meta <- attr(fit, "healthdatascience")

  expect_s3_class(fit, "coxph")
  expect_equal(meta$model_type, "landmark-cox")
  expect_equal(meta$n_before_landmark, 8)
  expect_equal(meta$n_at_landmark, 5)
  expect_equal(meta$excluded_before_landmark, 3)
  expect_true(meta$time_reset)
  expect_equal(min(fit$model$`survival::Surv(followup, status)`[, "time"]), 0)
})

test_that("landmark Cox rejects mismatched and counting-process time specifications", {
  skip_if_not_installed("survival")
  df <- data.frame(
    start = c(0, 1, 0, 2),
    stop = c(1, 3, 2, 4),
    time = c(1, 3, 2, 4),
    status = c(0, 1, 0, 1),
    x = c(0.1, 0.2, 0.3, 0.4)
  )

  expect_error(
    landmark_cox(
      survival::Surv(time, status) ~ x,
      df,
      landmark = 1,
      time = "stop"
    ),
    "must match"
  )
  expect_error(
    landmark_cox(
      survival::Surv(start, stop, status) ~ x,
      df,
      landmark = 1,
      time = "stop"
    ),
    "two-argument"
  )
})

test_that("tidy_cox_model returns a common hazard-ratio schema", {
  skip_if_not_installed("survival")
  df <- data.frame(
    time = 1:12,
    status = rep(c(1, 0, 1), 4),
    x = seq(-1, 1, length.out = 12)
  )
  fit <- suppressWarnings(landmark_cox(
    survival::Surv(time, status) ~ x,
    df,
    landmark = 3
  ))
  out <- tidy_cox_model(fit)

  expect_true(all(c(
    "model_type", "term", "estimate", "std_error", "statistic",
    "p_value", "hazard_ratio", "conf_low", "conf_high"
  ) %in% names(out)))
  expect_equal(out$model_type, "landmark-cox")
  expect_equal(out$hazard_ratio, exp(out$estimate))
})
