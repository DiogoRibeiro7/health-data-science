test_that("advanced statistics monolith stays split", {
  r_dir <- testthat::test_path("..", "..", "R")

  expect_false(file.exists(file.path(r_dir, "advanced_statistics.R")))
  expect_true(all(file.exists(file.path(
    r_dir,
    c(
      "advanced_latent_models.R",
      "advanced_survival.R",
      "statistical_learning.R",
      "clinical_analytics.R",
      "bayesian_methods.R"
    )
  ))))
})

test_that("cost effectiveness has one source definition", {
  r_dir <- testthat::test_path("..", "..", "R")
  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)

  definitions <- unlist(lapply(r_files, function(path) {
    lines <- readLines(path, warn = FALSE)
    hits <- grep("^cost_effectiveness\\s*<-\\s*function", lines, value = TRUE)
    if (length(hits) > 0L) basename(path) else character()
  }))

  expect_identical(definitions, "zzz_api_canonical.R")
})

test_that("moved statistical APIs remain available", {
  expected <- c(
    "fit_latent_transition",
    "fit_mixture_covariates",
    "fit_bayesian_lca",
    "fit_multilevel_lca",
    "fit_competing_risks",
    "tidy_competing_risks",
    "fit_time_varying_cox",
    "fit_frailty_cox",
    "landmark_cox",
    "train_rf_missing",
    "train_gbm",
    "train_neural_net",
    "detect_pharmacovigilance",
    "develop_clinical_prediction_rule",
    "biomarker_discovery",
    "run_mcmc",
    "bayesian_model_averaging",
    "fit_hierarchical_bayes",
    "posterior_predictive_check"
  )

  expect_true(all(vapply(expected, exists, logical(1), mode = "function")))
})
