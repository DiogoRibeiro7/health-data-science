source(file.path("..", "..", "R", "model_validation.R"))


test_that("analyze_residuals returns data frame", {
  mod <- lm(mpg ~ cyl, data = mtcars)
  res <- analyze_residuals(mod)
  expect_s3_class(res, "data.frame")
})


test_that("goodness_of_fit computes AIC", {
  mod <- lm(mpg ~ cyl, data = mtcars)
  res <- goodness_of_fit(mod)
  expect_true("AIC" %in% names(res))
})
