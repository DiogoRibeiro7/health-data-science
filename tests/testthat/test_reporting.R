test_that("format_table returns knitr_kable", {
  skip_if_not_installed("knitr")
  tab <- format_table(head(mtcars))
  expect_s3_class(tab, "knitr_kable")
})

test_that("render_report handles missing template", {
  skip_if_not_installed("rmarkdown")
  expect_error(render_report("nonexistent.Rmd"), "cannot open")
})
