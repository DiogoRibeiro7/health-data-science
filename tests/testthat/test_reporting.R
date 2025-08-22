skip_if_not_installed("rmarkdown")

source(file.path("..", "..", "R", "reporting.R"))
source(file.path("..", "..", "R", "reporting_visuals.R"))
source(file.path("..", "..", "R", "insights.R"))

test_that("format_table returns knitr_kable", {
  skip_if_not_installed("knitr")
  tab <- format_table(head(mtcars))
  expect_s3_class(tab, "knitr_kable")
})

test_that("render_report handles missing template", {
  skip_if_not_installed("rmarkdown")
  expect_error(render_report("nonexistent.Rmd"), "cannot open")
})

test_that("report generation returns path", {
  out <- generate_report("executive", params = list(highlights = data.frame(metric = 1)), output_file = tempfile(fileext = ".html"))
  expect_true(grepl(".html$", out))
})

test_that("visualization helpers produce widgets", {
  skip_if_not_installed("plotly")
  p <- plot_interactive(mtcars, "mpg", "wt")
  expect_true("plotly" %in% class(p))
})

test_that("insight helpers work", {
  model <- lm(mpg ~ wt, data = mtcars)
  sig <- detect_significance(model)
  expect_true("term" %in% names(sig))
  tr <- analyze_trends(mtcars$mpg)
  expect_true(tr$trend %in% c("increasing", "decreasing"))
  anom <- detect_anomalies(c(1,2,100))
  expect_true(length(anom) >= 1)
})
