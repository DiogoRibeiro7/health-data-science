test_that("group-time DiD validates panel and cohort structure", {
  df <- data.frame(
    id = rep(1:4, each = 3),
    period = rep(1:3, times = 4),
    first_treated = rep(c(2, 3, 0, 0), each = 3),
    y = seq_len(12),
    x = rep(c(0, 1, 0, 1), each = 3)
  )

  duplicate <- rbind(df, df[1, ])
  expect_error(
    fit_group_time_did(
      duplicate, "y", "period", "id", "first_treated", "x"
    ),
    "one row per unit-period"
  )

  varying <- df
  varying$first_treated[2] <- 3
  expect_error(
    fit_group_time_did(varying, "y", "period", "id", "first_treated", "x"),
    "constant within each unit"
  )

  no_never <- df[df$id %in% c(1, 2), ]
  expect_error(
    fit_group_time_did(
      no_never, "y", "period", "id", "first_treated", "x",
      control_group = "nevertreated"
    ),
    "requires units"
  )
})

test_that("group-time DiD recovers a staggered treatment effect", {
  skip_if_not_installed("did")

  set.seed(42)
  n_per_group <- 80
  ids <- seq_len(3 * n_per_group)
  cohort <- rep(c(3, 4, 0), each = n_per_group)
  unit_effect <- rnorm(length(ids), sd = 0.5)
  periods <- 1:6

  df <- do.call(rbind, lapply(seq_along(ids), function(i) {
    g <- cohort[i]
    treatment_effect <- ifelse(g > 0 & periods >= g, 2, 0)
    data.frame(
      id = ids[i],
      period = periods,
      first_treated = g,
      x = rep((i %% 2), length(periods)),
      y = unit_effect[i] + 0.4 * periods + treatment_effect +
        rnorm(length(periods), sd = 0.25)
    )
  }))

  fit <- fit_group_time_did(
    df,
    outcome = "y",
    period = "period",
    id = "id",
    first_treated = "first_treated",
    covariates = "x",
    control_group = "nevertreated",
    bstrap = FALSE,
    cband = FALSE
  )

  expect_s3_class(fit, "hds_group_time_did")
  expect_equal(fit$metadata$treated_cohorts, c(3, 4))
  expect_equal(fit$metadata$never_treated_units, n_per_group)

  gt <- tidy_group_time_did(fit)
  expect_true(all(c(
    "group", "period", "event_time", "estimate", "std_error",
    "post_treatment"
  ) %in% names(gt)))
  expect_true(any(gt$post_treatment))

  simple <- aggregate_group_time_did(fit, type = "simple")
  expect_s3_class(simple, "hds_did_aggregation")
  expect_equal(nrow(simple$table), 1)
  expect_equal(simple$table$estimate, 2, tolerance = 0.5)

  dynamic <- aggregate_group_time_did(fit, type = "dynamic")
  expect_true(all(c("index", "estimate", "std_error") %in% names(dynamic$table)))
  expect_true(any(dynamic$table$index >= 0))

  pretrend <- did_pretrend_diagnostic(fit)
  expect_true(all(c("statistic", "p_value", "interpretation") %in% names(pretrend)))
})

test_that("not-yet-treated controls are supported without never-treated units", {
  skip_if_not_installed("did")

  set.seed(7)
  n <- 40
  ids <- seq_len(2 * n)
  cohort <- rep(c(3, 5), each = n)
  periods <- 1:6

  df <- do.call(rbind, lapply(seq_along(ids), function(i) {
    g <- cohort[i]
    data.frame(
      id = ids[i],
      period = periods,
      first_treated = g,
      y = 0.2 * periods + ifelse(periods >= g, 1.5, 0) +
        rnorm(length(periods), sd = 0.2)
    )
  }))

  fit <- fit_group_time_did(
    df,
    outcome = "y",
    period = "period",
    id = "id",
    first_treated = "first_treated",
    control_group = "notyettreated",
    bstrap = FALSE,
    cband = FALSE
  )

  expect_s3_class(fit, "hds_group_time_did")
  expect_equal(fit$metadata$control_group, "notyettreated")
  expect_equal(fit$metadata$never_treated_units, 0)
})
