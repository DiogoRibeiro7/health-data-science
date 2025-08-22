context("advanced statistics")

skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(paste("Package", pkg, "not installed"))
  }
}

test_that("cost_effectiveness computes ICER", {
  expect_equal(cost_effectiveness(200, 0.8, 150, 0.6), 250)
})

test_that("detect_pharmacovigilance computes PRR", {
  prr <- detect_pharmacovigilance(10, 20, 5, 50)
  expect_gt(prr, 0)
})

test_that("train_rf_missing runs when ranger available", {
  skip_if_not_installed("ranger")
  df <- data.frame(y = c(1,2,NA,3), x1 = c(4,5,6,7))
  fit <- train_rf_missing(df, "y")
  expect_true(inherits(fit, "ranger"))
})
