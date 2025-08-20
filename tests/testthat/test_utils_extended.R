source(file.path("..", "..", "R", "utils.R"))


test_that("ensure_dir creates directory", {
  tmp <- file.path(tempdir(), "new", "file.txt")
  ensure_dir(tmp)
  expect_true(dir.exists(dirname(tmp)))
})


test_that("require_data_file errors for missing", {
  expect_error(require_data_file("nonexistent.csv"))
})


test_that("benchmark_expr returns times", {
  times <- benchmark_expr({sum(1:100)}, times = 3)
  expect_length(times, 3)
  expect_true(all(times >= 0))
})
