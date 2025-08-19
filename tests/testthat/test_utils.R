source(file.path("..", "..", "R", "utils.R"))

test_that("ensure_dir creates directories", {
  tmp <- file.path(tempdir(), "sub", "file.txt")
  dir <- dirname(tmp)
  if (dir.exists(dir)) unlink(dir, recursive = TRUE)
  ensure_dir(tmp)
  expect_true(dir.exists(dir))
})

test_that("require_data_file errors for missing files", {
  missing <- tempfile()
  expect_error(require_data_file(missing), "Data file not found")
})
