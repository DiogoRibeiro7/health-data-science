context("machine learning utilities")

library(testthat)

# auto_select_lca --------------------------------------------------------------

test_that("auto_select_lca returns best_k", {
  skip_if_not_installed("poLCA")
  df <- data.frame(race3 = sample(1:3, 30, TRUE),
                   hispanic3 = sample(1:3, 30, TRUE),
                   urbandwell = sample(1:3, 30, TRUE),
                   age4 = sample(1:4, 30, TRUE),
                   SES = sample(1:3, 30, TRUE),
                   insurancetype = sample(1:6, 30, TRUE))
  df_prep <- prepare_lca_data(df)
  sel <- auto_select_lca(df_prep, df_prep, k_range = 2:3, folds = 2)
  expect_true(sel$best_k %in% 2:3)
  expect_s3_class(sel$scores, "data.frame")
})

# train_ensemble ---------------------------------------------------------------

test_that("train_ensemble produces predictions", {
  skip_if_not_installed("randomForest")
  ens <- train_ensemble(mtcars, "mpg", models = c("rf", "lm"))
  preds <- ens$predict(mtcars[1:5, ])
  expect_length(preds, 5)
})

# register_model --------------------------------------------------------------

test_that("register_model saves file", {
  tmp <- tempfile()
  model <- lm(mpg ~ wt, mtcars)
  path <- register_model(model, "test", registry = tmp)
  expect_true(file.exists(path))
})

# compute_shap ----------------------------------------------------------------

test_that("compute_shap returns data frame", {
  skip_if_not_installed("iml")
  skip_if_not_installed("randomForest")
  rf <- randomForest::randomForest(Species ~ ., data = iris)
  shap <- compute_shap(rf, iris[, -5], sample_size = 5)
  expect_s3_class(shap, "data.frame")
})

# batch_predict ---------------------------------------------------------------

test_that("batch_predict splits data", {
  model <- lm(mpg ~ wt, mtcars)
  preds <- batch_predict(model, mtcars, chunk_size = 10)
  expect_length(preds, nrow(mtcars))
})

# detect_model_drift ----------------------------------------------------------

test_that("detect_model_drift flags changes", {
  expect_false(detect_model_drift(c(0.1, 0.15), threshold = 0.2))
  expect_true(detect_model_drift(c(0.1, 0.4), threshold = 0.2))
})
