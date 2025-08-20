context("data processing")

library(testthat)

# Sample data for tests
sample <- data.frame(a = c(1, 2, NA, 4), b = c(1, 100, 3, 4))

test_that("data quality summary has expected columns", {
  res <- assess_data_quality(sample)
  expect_true(all(c("variable", "class", "n_missing", "pct_missing", "n_unique") %in% names(res)))
})

test_that("polynomial features are added", {
  df <- add_polynomial_features(sample, "a", degree = 2)
  expect_true("a_pow_2" %in% names(df))
})

test_that("outliers are detected and handled", {
  flags <- detect_outliers(sample, "b")
  expect_equal(ncol(flags), 1)
  capped <- handle_outliers(sample, "b")
  expect_lt(max(capped$b) - min(capped$b), max(sample$b) - min(sample$b))
})

test_that("reading and exporting CSV works", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  export_data(sample, tmp, "csv")
  df <- read_data_source(tmp, "csv")
  expect_equal(nrow(df), nrow(sample))
})

test_that("missing data can be imputed", {
  skip_if_not_installed("mice")
  imp <- impute_missing(sample)
  expect_false(any(is.na(imp)))
})
