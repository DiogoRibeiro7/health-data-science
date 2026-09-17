test_that("recurrent-event inputs are validated", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    start = c(0, 2, 0, 3),
    stop = c(2, 4, 3, 5),
    event = c(1, 0, 1, 0),
    x = c(0.2, 0.2, -0.1, -0.1)
  )

  expect_error(
    fit_recurrent_events(df, "start", "stop", "event", "id", "x",
                         model = "pwp-total-time"),
    "event_order"
  )

  bad <- df
  bad$stop[2] <- 1
  expect_error(
    fit_recurrent_events(bad, "start", "stop", "event", "id", "x"),
    "stop > start"
  )

  overlap <- df
  overlap$start[2] <- 1
  expect_error(
    fit_recurrent_events(overlap, "start", "stop", "event", "id", "x"),
    "overlap"
  )
})

test_that("missing-value policy is explicit", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    start = c(0, 2, 0, 3),
    stop = c(2, 4, 3, 5),
    event = c(1, 0, 1, 0),
    x = c(0.2, NA, -0.1, -0.1)
  )

  expect_error(
    fit_recurrent_events(df, "start", "stop", "event", "id", "x"),
    "Missing values"
  )
})

test_that("Andersen-Gill model stores recurrent-event metadata", {
  skip_if_not_installed("survival")

  df <- data.frame(
    id = rep(1:6, each = 2),
    start = rep(c(0, 2), 6),
    stop = rep(c(2, 5), 6),
    event = c(1,0, 0,1, 1,1, 0,0, 1,0, 0,1),
    x = rep(c(-1, -0.5, 0, 0.5, 1, 1.5), each = 2)
  )

  fit <- fit_recurrent_events(
    df, "start", "stop", "event", "id", "x",
    model = "andersen-gill"
  )

  expect_s3_class(fit, "coxph")
  meta <- attr(fit, "healthdatascience")
  expect_equal(meta$model, "andersen-gill")
  expect_equal(meta$n_subjects, 6)
  expect_equal(meta$event_count, sum(df$event))

  tab <- tidy_recurrent_events(fit)
  expect_true(all(c("term", "hazard_ratio", "conf_low", "conf_high") %in% names(tab)))
})

test_that("PWP total-time model accepts event-order strata", {
  skip_if_not_installed("survival")

  df <- data.frame(
    id = rep(1:6, each = 2),
    start = rep(c(0, 2), 6),
    stop = rep(c(2, 5), 6),
    event = c(1,0, 0,1, 1,1, 0,0, 1,0, 0,1),
    event_order = rep(c(1, 2), 6),
    x = rep(c(-1, -0.5, 0, 0.5, 1, 1.5), each = 2)
  )

  fit <- fit_recurrent_events(
    df, "start", "stop", "event", "id", "x",
    model = "pwp-total-time",
    event_order = "event_order"
  )

  expect_s3_class(fit, "coxph")
  meta <- attr(fit, "healthdatascience")
  expect_equal(meta$model, "pwp-total-time")
  expect_equal(meta$event_order, "event_order")
})
