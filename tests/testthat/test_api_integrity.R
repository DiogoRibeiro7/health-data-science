test_that("public namespace has no duplicate export directives", {
  namespace_path <- system.file("NAMESPACE", package = "healthdatascience")
  expect_true(nzchar(namespace_path))

  exports <- grep(
    "^export\\(",
    readLines(namespace_path, warn = FALSE),
    value = TRUE
  )
  expect_false(anyDuplicated(exports) > 0L)
})

test_that("cost_effectiveness has one stable positional result", {
  expect_equal(cost_effectiveness(200, 0.8, 150, 0.6), 250)
  expect_equal(
    cost_effectiveness(
      cost1 = 200,
      effect1 = 0.8,
      cost2 = 150,
      effect2 = 0.6
    ),
    250
  )
})

test_that("cost_effectiveness accepts historical population-health aliases", {
  expect_equal(
    cost_effectiveness(
      cost_a = 200,
      effect_a = 0.8,
      cost_b = 150,
      effect_b = 0.6
    ),
    250
  )
})

test_that("cost_effectiveness rejects ambiguous and incomplete calls", {
  expect_error(
    cost_effectiveness(
      cost1 = 200,
      effect1 = 0.8,
      cost2 = 150,
      effect2 = 0.6,
      cost_a = 200
    ),
    "either `cost1` or `cost_a`"
  )
  expect_error(
    cost_effectiveness(cost1 = 200, effect1 = 0.8),
    "Missing required argument"
  )
  expect_error(
    cost_effectiveness(200, 0.8, 150, 0.6, nonsense = 1),
    "Unknown argument"
  )
})
