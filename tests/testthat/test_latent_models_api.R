test_that("latent-transition wrapper validates legacy wide data", {
  expect_error(
    fit_latent_transition(matrix(1, nrow = 1, ncol = 3), k = 2),
    "at least two subjects"
  )

  expect_error(
    fit_latent_transition(
      data.frame(id = c(1, 1), time = c(1, 1), y = c(1, 2)),
      k = 2,
      id = "id",
      time = "time",
      responses = "y"
    ),
    "at most once"
  )

  expect_error(
    fit_latent_transition(
      data.frame(id = 1:3, time = 1:3, y = 1:3),
      k = 2,
      id = "id"
    ),
    "both"
  )
})

test_that("latent-transition wrapper calls current LMest API", {
  skip_if_not_installed("LMest")

  data <- data.frame(
    id = rep(1:4, each = 2),
    time = rep(1:2, times = 4),
    response = c(1, 1, 1, 2, 2, 2, 2, 1)
  )

  fit <- fit_latent_transition(
    data,
    k = 1,
    id = "id",
    time = "time",
    responses = "response",
    max_iter = 20
  )

  meta <- latent_model_metadata(fit)
  expect_equal(meta$engine, "LMest::lmest")
  expect_equal(meta$k, 1)
  expect_equal(meta$input_format, "long")
})

test_that("mixture wrapper validates sample and missingness contracts", {
  data <- data.frame(y = c(1, 2, NA), x = c(1, 2, 3))

  expect_error(
    fit_mixture_covariates(y ~ x, data, k = 2, na_action = "fail"),
    "Missing values"
  )

  expect_error(
    fit_mixture_covariates(
      y ~ x,
      data.frame(y = 1, x = 1),
      k = 2
    ),
    "at least as many rows"
  )
})

test_that("BayesLCA wrapper validates binary indicators and class count", {
  x <- matrix(
    c(
      0, 0, 1,
      0, 1, 1,
      1, 0, 0,
      1, 1, 0
    ),
    nrow = 4,
    byrow = TRUE
  )

  expect_error(
    fit_bayesian_lca(replace(x, 1, 2), nclass = 2),
    "coded 0/1"
  )

  expect_error(
    fit_bayesian_lca(x, nclass = 5),
    "cannot exceed"
  )
})

test_that("BayesLCA wrapper passes class count through G", {
  skip_if_not_installed("BayesLCA")

  x <- matrix(
    c(
      0, 0, 0,
      0, 0, 1,
      0, 1, 0,
      0, 1, 1,
      1, 0, 0,
      1, 0, 1,
      1, 1, 0,
      1, 1, 1
    ),
    nrow = 8,
    byrow = TRUE
  )

  fit <- fit_bayesian_lca(x, nclass = 2, method = "em", iter = 20)
  meta <- latent_model_metadata(fit)

  expect_equal(meta$engine, "BayesLCA::blca")
  expect_equal(meta$nclass, 2)
  expect_equal(meta$method, "em")
})

test_that("randomLCA wrapper keeps frequency and class count distinct", {
  x <- matrix(
    c(
      0, 0, 0,
      0, 0, 1,
      0, 1, 0,
      0, 1, 1,
      1, 0, 0,
      1, 0, 1,
      1, 1, 0,
      1, 1, 1
    ),
    nrow = 8,
    byrow = TRUE
  )

  expect_error(
    fit_multilevel_lca(x, nclass = 2, frequency = c(1, 2)),
    "one per pattern"
  )

  expect_error(
    fit_multilevel_lca(
      x,
      nclass = 2,
      random_effect = FALSE,
      level2 = TRUE
    ),
    "requires"
  )
})

test_that("randomLCA wrapper passes nclass by name", {
  skip_if_not_installed("randomLCA")

  x <- matrix(
    rep(
      c(
        0, 0, 0,
        0, 1, 1,
        1, 0, 1,
        1, 1, 0
      ),
      5
    ),
    ncol = 3,
    byrow = TRUE
  )

  fit <- fit_multilevel_lca(
    x,
    nclass = 1,
    random_effect = FALSE,
    random_starts = 1,
    calculate_se = FALSE,
    seed = 123
  )
  meta <- latent_model_metadata(fit)

  expect_equal(meta$engine, "randomLCA::randomLCA")
  expect_equal(meta$nclass, 1)
  expect_false(meta$random_effect)
})
