test_that("0.3.0 release surface is internally consistent", {
  description <- read.dcf("DESCRIPTION")
  expect_equal(unname(description[1, "Version"]), "0.3.0")

  namespace <- readLines("NAMESPACE", warn = FALSE)
  exports <- namespace[grepl("^export\\(", namespace)]
  expect_length(exports, length(unique(exports)))

  expect_true(file.exists("docs/api_inventory.md"))
  expect_true(file.exists("docs/release_readiness_0.3.0.md"))
  expect_true(file.exists("docs/README.md"))
  expect_true(file.exists("NEXT_STEPS.md"))
})
