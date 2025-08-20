source(file.path("..", "..", "R", "storage.R"))

skip_if_not_installed("arrow")


test_that("parquet roundtrip preserves rows", {
  tmp <- tempfile(fileext = ".parquet")
  write_parquet_data(mtcars, tmp)
  df <- read_parquet_data(tmp)
  expect_equal(nrow(df), nrow(mtcars))
})

