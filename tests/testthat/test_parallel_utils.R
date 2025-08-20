source(file.path("..", "..", "R", "utils.R"))
source(file.path("..", "..", "R", "model_validation.R"))
source(file.path("..", "..", "R", "data_processing.R"))

testthat::skip_if_not_installed("future.apply")
testthat::skip_if_not_installed("Matrix")


test_that("read_csv_chunked processes all rows", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(x = 1:50), tmp, row.names = FALSE)
  count <- 0
  read_csv_chunked(tmp, chunk_size = 10, callback = function(df) {
    count <<- count + nrow(df)
  }, progress = FALSE)
  expect_equal(count, 50)
})


test_that("cache_result invalidates when dependency changes", {
  dep <- tempfile()
  writeLines("a", dep)
  cache_path <- tempfile(fileext = ".rds")
  val1 <- cache_result(cache_path, { readLines(dep) }, depends = dep)
  writeLines("b", dep)
  val2 <- cache_result(cache_path, { readLines(dep) }, depends = dep)
  expect_false(identical(val1, val2))
})


test_that("assess_data_quality parallel matches sequential", {
  res1 <- assess_data_quality(mtcars, parallel = FALSE, progress = FALSE)
  res2 <- assess_data_quality(mtcars, parallel = TRUE, progress = FALSE)
  expect_equal(res1, res2)
})


test_that("to_sparse_matrix produces sparse matrix", {
  sp <- to_sparse_matrix(data.frame(x = c(0,1,0)))
  expect_s4_class(sp, "dgCMatrix")
})


test_that("kfold_cv runs in parallel", {
  errs <- kfold_cv(mtcars, k = 3,
    fit_fn = function(d) lm(mpg ~ cyl, data = d),
    pred_fn = function(mod, d) predict(mod, newdata = d),
    parallel = TRUE, progress = FALSE)
  expect_length(errs, 3)
})


test_that("bootstrap_ci runs in parallel", {
  mod <- lm(mpg ~ cyl, data = mtcars)
  ci <- bootstrap_ci(mod, "cyl", R = 20, parallel = TRUE, progress = FALSE)
  expect_length(ci, 2)
})
