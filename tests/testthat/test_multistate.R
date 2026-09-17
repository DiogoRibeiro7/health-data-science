test_that("transition structures are validated", {
  expect_error(
    validate_transition_structure(data.frame(from = "A", to = "A")),
    "Self-transitions"
  )

  expect_error(
    validate_transition_structure(data.frame(from = c("A", "A"), to = c("B", "B"))),
    "Duplicate"
  )

  tr <- validate_transition_structure(data.frame(
    from = c("A", "A", "B"),
    to = c("B", "C", "C")
  ))
  expect_s3_class(tr, "hds_transition_structure")
  expect_equal(tr$transition_id, 1:3)
})

test_that("multi-state paths and transitions are validated", {
  transitions <- data.frame(from = c("A", "A", "B"), to = c("B", "C", "C"))
  df <- data.frame(
    id = c(1, 1, 2),
    start = c(0, 2, 0),
    stop = c(2, 5, 5),
    from = c("A", "B", "A"),
    to = c("B", "C", "C"),
    event = c(1, 1, 1),
    x = c(0.1, 0.1, -0.2)
  )

  bad <- df
  bad$from[2] <- "A"
  expect_error(
    fit_multistate_cox(
      bad, "start", "stop", "from", "to", "event", "id",
      transitions, covariates = "x"
    ),
    "preserve state history"
  )

  bad_transition <- df
  bad_transition$to[1] <- "C"
  bad_transition$from[2] <- "C"
  bad_transition$to[2] <- "B"
  expect_error(
    fit_multistate_cox(
      bad_transition, "start", "stop", "from", "to", "event", "id",
      transitions, covariates = "x"
    ),
    "not allowed"
  )
})

test_that("transition-specific Cox models and tidy output work", {
  skip_if_not_installed("survival")

  transitions <- data.frame(
    from = c("A", "A", "B"),
    to = c("B", "C", "C")
  )

  df <- data.frame(
    id = c(1,1,2,3,4,5,6,7,8),
    start = c(0,2,0,0,0,0,0,0,0),
    stop = c(2,5,4,3,5,2,4,3,5),
    from = c("A","B","A","A","A","A","A","A","A"),
    to = c("B","C","C","B","C","B","C","B","C"),
    event = rep(1, 9),
    x = c(-1,-1,-0.7,-0.5,-0.2,0.1,0.4,0.7,1)
  )

  fit <- fit_multistate_cox(
    df, "start", "stop", "from", "to", "event", "id",
    transitions, covariates = "x"
  )

  expect_s3_class(fit, "hds_multistate_cox")
  expect_equal(fit$metadata$n_subjects, 8)
  expect_true(all(c(1, 2, 3) %in% fit$metadata$estimable_transitions))

  tab <- tidy_multistate_cox(fit)
  expect_true(all(c(
    "transition_id", "transition", "term",
    "cause_specific_hazard_ratio", "conf_low", "conf_high"
  ) %in% names(tab)))
})

test_that("Aalen-Johansen state occupation returns valid probabilities", {
  transitions <- data.frame(
    from = c("A", "A", "B"),
    to = c("B", "C", "C")
  )

  df <- data.frame(
    id = c(1,1,2,3),
    start = c(0,2,0,0),
    stop = c(2,5,4,5),
    from = c("A","B","A","A"),
    to = c("B","C","C", NA),
    event = c(1,1,1,0)
  )

  occ <- estimate_state_occupation(
    df, "start", "stop", "from", "to", "event", "id",
    transitions, times = c(0, 2, 4, 5)
  )

  expect_equal(sort(unique(occ$state)), c("A", "B", "C"))
  sums <- tapply(occ$probability, occ$time, sum)
  expect_equal(as.numeric(sums), rep(1, length(sums)), tolerance = 1e-10)
  expect_true(all(occ$probability >= -1e-12 & occ$probability <= 1 + 1e-12))

  meta <- attr(occ, "healthdatascience")
  expect_equal(meta$estimator, "aalen-johansen")
  expect_equal(meta$n_subjects, 3)
})
