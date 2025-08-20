source(file.path("..", "..", "R", "lca.R"))


 test_that("prepare_lca_data encodes factors", {
  df <- data.frame(
    race3 = 1,
    urbandwell = 2,
    hispanic3 = 1,
    insurancetype = 2,
    age4 = 1,
    SES = 2,
    FACILITY_TYPE_CD = 1,
    PUF_CASE_ID = 1,
    optcare = 1,
    SEX = 1,
    DX_RX_STARTED_DAYS = 1,
    CROWFLY = 1,
    CDCC_TOTAL_BEST = 0
  )
  prepared <- prepare_lca_data(df)
  expect_s3_class(prepared$race3, "factor")
  expect_equal(levels(prepared$race3), c("White", "Black", "Other"))
  expect_s3_class(prepared$facility, "factor")
})
