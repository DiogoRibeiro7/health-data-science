source(file.path("..", "..", "R", "lca.R"))
source(file.path("..", "..", "R", "optimization.R"))
source(file.path("..", "..", "R", "model_validation.R"))
source(file.path("..", "..", "R", "utils.R"))
source(file.path("..", "..", "R", "config.R"))

# use mock data generator
source("helper-mock-data.R")

# create temp config
mock_cfg <- list(lca = list(nclass = 2, maxiter = 10, tol = 0.1, nrep = 1, verbose = FALSE, auto_classes = FALSE))


test_that("run_lca completes end-to-end", {
  skip_if_not_installed("poLCA")
  df <- mock_ncdb_data(20)
  tmp_in <- tempfile(fileext = ".csv")
  tmp_out <- tempfile(fileext = ".RData")
  write.csv(df, tmp_in, row.names = FALSE)
  res <- run_lca(tmp_in, tmp_out, list(lca = mock_cfg$lca), verbose = FALSE, show_progress = FALSE)
  expect_s3_class(res, "poLCA")
  expect_true(file.exists(tmp_out))
})


test_that("lca_stability returns correct rows", {
  skip_if_not_installed("poLCA")
  df <- mock_ncdb_data(30)
  prepared <- prepare_lca_data(df)
  vars <- select_lca_variables(prepared)
  f <- with(vars, cbind(race3, hispanic3, urbandwell, age4, SES, insurancetype) ~ 1)
  res <- lca_stability(f, prepared, 2, seeds = 1:3)
  expect_equal(nrow(res), 3)
})

test_that("validate_lca_data detects issues", {
  df <- mock_ncdb_data(10)
  df$race3[1] <- NA
  prepared <- prepare_lca_data(df)
  vars <- select_lca_variables(prepared)
  issues <- validate_lca_data(vars, 2)
  expect_true("missing" %in% names(issues))
  expect_error(validate_lca_data(vars[1:2, ], 3), "Sample size is likely inadequate")
})

test_that("fit_lca_model falls back on convergence failure", {
  skip_if_not_installed("poLCA")
  df <- mock_ncdb_data(40)
  prepared <- prepare_lca_data(df)
  vars <- select_lca_variables(prepared)
  cfg <- list(nclass = 6, maxiter = 5, tol = 0.1, nrep = 2, verbose = FALSE, auto_classes = FALSE)
  model <- fit_lca_model(prepared, vars, cfg)
  expect_true(model$K <= 6)
})
