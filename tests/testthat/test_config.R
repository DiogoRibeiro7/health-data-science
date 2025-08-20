source(file.path("..", "..", "R", "utils.R"))
source(file.path("..", "..", "R", "config.R"))

test_that("load_config merges overrides", {
  root <- normalizePath("..", mustWork = TRUE)
  cfg <- load_config("testing", root = root)
  expect_equal(cfg$lca$nclass, 3)
  expect_equal(cfg$regression$confidence_level, 0.8)
})
