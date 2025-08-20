source(file.path("..", "..", "R", "regression.R"))
library(testthat)

test_that("fit_min_treatment_model returns glm", {
  df <- data.frame(
    mintreat = c(1, 0, 1, 0),
    LCAprofile = factor(c("1", "1", "2", "2")),
    ANALYTIC_STAGE_GROUP = factor(c("I", "II", "I", "II")),
    facility = factor(c("A", "A", "B", "B")),
    CDCC_TOTAL_BEST = c(0, 1, 0, 1),
    FACILITY_LOCATION_CD = factor(c("X", "X", "Y", "Y")),
    YEAR_OF_DIAGNOSIS = c(2010, 2011, 2010, 2011),
    optcare = c(1, 0, 1, 0)
  )
  model <- fit_min_treatment_model(df)
  expect_s3_class(model, "glm")
})
