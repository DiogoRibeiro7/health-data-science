source(file.path("..", "..", "R", "data_processing.R"))
source(file.path("..", "..", "R", "utils.R"))
source("helper-mock-data.R")


test_that("impute_missing removes all NAs", {
  df <- mock_ncdb_data(10)
  df$CROWFLY[1] <- NA
  imputed <- impute_missing(df)
  expect_false(anyNA(imputed))
})


test_that("detect_outliers flags extremes", {
  df <- data.frame(a = c(1, 100, 2, 3))
  out <- detect_outliers(df, "a")
  expect_true(out[2, 1])
})


test_that("run_transform_pipeline validates data", {
  df <- data.frame(a = c(1, NA, 3))
  steps <- list(function(d) {d})
  validators <- list(function(d) all(!is.na(d$a)))
  expect_error(run_transform_pipeline(df, steps, validators))
})


test_that("record_data_version logs entry", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(mtcars, tmp)
  log <- tempfile(fileext = ".csv")
  record_data_version(tmp, log)
  expect_true(file.exists(log))
})

test_that("read_data_source enforces schema and retries", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1, b = 2), tmp, row.names = FALSE)
  expect_error(read_data_source(tmp, "csv", schema = list(a = "character")))
  expect_silent(read_data_source(tmp, "csv", schema = list(a = "double", b = "double")))
})

test_that("export_data errors on non-data frame", {
  tmp <- tempfile(fileext = ".csv")
  expect_error(export_data(1, tmp))
})
