test_that("theme_publication returns theme", {
  skip_if_not_installed("ggplot2")
  th <- theme_publication()
  expect_s3_class(th, "theme")
})

test_that("forest_plot builds plot", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(term = c("A", "B"), estimate = c(1.2, 0.8), lcl = c(0.9, 0.5), ucl = c(1.5, 1.1))
  p <- forest_plot(df, estimate, lcl, ucl, term)
  expect_s3_class(p, "ggplot")
})
