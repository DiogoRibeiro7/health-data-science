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

test_that("read_csv_safely loads data and errors informatively", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(x = 1), tmp, row.names = FALSE)
  df <- read_csv_safely(tmp)
  expect_equal(df$x, 1)
  unlink(tmp)
  expect_error(read_csv_safely(tmp), "Data file not found")
})

test_that("cache_result caches expressions", {
  tmp <- tempfile(fileext = ".rds")
  if (file.exists(tmp)) file.remove(tmp)
  res1 <- cache_result(tmp, { 1 + 1 })
  res2 <- cache_result(tmp, { stop("should not run") })
  expect_equal(res1, 2)
  expect_equal(res2, 2)
})

test_that("chunk_apply processes data in chunks", {
  df <- data.frame(x = 1:10)
  chunks <- 0
  chunk_apply(df, 3, function(chunk) { chunks <<- chunks + 1 })
  expect_equal(chunks, ceiling(nrow(df) / 3))
})

test_that("monitor_step returns expression result", {
  res <- monitor_step("test", {1 + 1}, verbose = FALSE)
  expect_equal(res, 2)
})
