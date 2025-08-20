test_that("Shiny app loads", {
  app <- shiny::shinyAppDir("app")
  expect_s3_class(app, "shiny.appobj")
})

test_that("Plumber API loads", {
  pr <- plumber::plumb("api/plumber.R")
  expect_s3_class(pr, "plumber")
})
