source(file.path("..", "..", "R", "lca.R"))
source(file.path("..", "..", "R", "optimization.R"))
source(file.path("..", "..", "R", "model_validation.R"))
source(file.path("..", "..", "R", "utils.R"))
source(file.path("..", "..", "R", "config.R"))

# use mock data generator
source("helper-mock-data.R")

# create temp config
mock_cfg <- list(lca = list(nclass = 2, maxiter = 10, tol = 0.1, nrep = 1, verbose = FALSE, auto_classes = FALSE))

skip_if_not_installed("poLCA")


test_that("run_lca completes end-to-end", {
  df <- mock_ncdb_data(20)
  tmp_in <- tempfile(fileext = ".csv")
  tmp_out <- tempfile(fileext = ".RData")
  write.csv(df, tmp_in, row.names = FALSE)
  res <- run_lca(tmp_in, tmp_out, list(lca = mock_cfg$lca), verbose = FALSE, show_progress = FALSE)
  expect_s3_class(res, "poLCA")
  expect_true(file.exists(tmp_out))
})


test_that("lca_stability returns correct rows", {
  df <- mock_ncdb_data(30)
  prepared <- prepare_lca_data(df)
  vars <- select_lca_variables(prepared)
  f <- with(vars, cbind(race3, hispanic3, urbandwell, age4, SES, insurancetype) ~ 1)
  res <- lca_stability(f, prepared, 2, seeds = 1:3)
  expect_equal(nrow(res), 3)
})
