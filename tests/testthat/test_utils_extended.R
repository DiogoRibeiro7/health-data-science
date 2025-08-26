source(file.path("..", "..", "R", "utils.R"))


test_that("ensure_dir creates directory", {
  tmp <- file.path(tempdir(), "new", "file.txt")
  ensure_dir(tmp)
  expect_true(dir.exists(dirname(tmp)))
})


test_that("require_data_file errors for missing", {
  expect_error(require_data_file("nonexistent.csv"))
})

test_that("validate_schema catches column issues", {
  df <- data.frame(a = 1L, b = "x")
  expect_error(validate_schema(df, list(a = "numeric")))
  expect_silent(validate_schema(df, list(a = "integer", b = "character")))
})

test_that("read_csv_safely validates schema and reports duplicates", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = c(1,1), b = c("x","y")), tmp, row.names = FALSE)
  expect_error(read_csv_safely(tmp, schema = list(a = "character")))
  expect_silent(read_csv_safely(tmp, schema = list(a = "double", b = "character")))
})


test_that("benchmark_expr returns times", {
  times <- benchmark_expr({sum(1:100)}, times = 3)
  expect_length(times, 3)
  expect_true(all(times >= 0))
})
