source(file.path("..", "..", "R", "audit.R"))

library(testthat)

test_that("audit log writes entries", {
  tmp <- tempfile()
  audit_log("login", "alice", path = tmp)
  expect_true(file.exists(tmp))
  expect_gt(length(readLines(tmp)), 0)
})

test_that("lineage tracking writes entries", {
  tmp <- tempfile()
  track_lineage("src.csv", "transform", "out.csv", path = tmp)
  expect_true(file.exists(tmp))
  expect_gt(nrow(read.csv(tmp)), 0)
})
