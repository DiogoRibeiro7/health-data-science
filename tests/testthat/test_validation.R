context("model validation")

test_that("analyze_residuals returns expected columns", {
  mod <- lm(mpg ~ cyl, data = mtcars)
  res <- analyze_residuals(mod)
  expect_true(all(c("fitted", "residual", "std_residual") %in% names(res)))
})

test_that("posterior_probabilities extracts matrix when poLCA available", {
  skip_if_not_installed("poLCA")
  data("gss82", package = "poLCA")
  f <- cbind(occup, income) ~ 1
  m <- poLCA::poLCA(f, gss82, nclass = 2, verbose = FALSE)
  post <- posterior_probabilities(m)
  expect_equal(nrow(post), nrow(gss82))
})
