test_that("train_rf_missing exposes task and supported missing-value handling", {
  skip_if_not_installed("ranger")

  set.seed(1)
  data <- data.frame(
    y = rnorm(40),
    x1 = rnorm(40),
    x2 = rnorm(40)
  )
  data$x1[c(3, 7)] <- NA_real_

  fit <- train_rf_missing(
    data,
    response = "y",
    missing_action = "learn",
    num.trees = 50,
    seed = 11
  )

  expect_s3_class(fit, "ranger")
  meta <- statistical_learning_metadata(fit)
  expect_equal(meta$task, "regression")
  expect_equal(meta$missing_action, "learn")
  expect_equal(meta$performance$assessment, "out_of_bag")
  expect_equal(meta$performance$metric, "oob_mse")

  expect_error(
    train_rf_missing(
      transform(data, y = factor(ifelse(y > 0, "yes", "no"))),
      response = "y",
      probability = TRUE,
      mtry = 99
    ),
    "mtry"
  )
})

test_that("train_rf_missing distinguishes classification and probability forests", {
  skip_if_not_installed("ranger")

  set.seed(2)
  data <- data.frame(
    y = factor(rep(c("control", "case"), each = 20)),
    x1 = rnorm(40),
    x2 = rnorm(40)
  )

  fit <- train_rf_missing(
    data,
    response = "y",
    probability = TRUE,
    num.trees = 50,
    seed = 12
  )

  meta <- statistical_learning_metadata(fit)
  expect_equal(meta$task, "classification")
  expect_equal(meta$class_levels, c("case", "control"))
  expect_true(meta$probability)
  expect_equal(meta$performance$metric, "oob_brier_score")
})

test_that("train_gbm uses explicit regression and binary objectives", {
  skip_if_not_installed("xgboost")

  set.seed(3)
  x <- matrix(rnorm(100), nrow = 50)
  y_reg <- 2 * x[, 1] + rnorm(50, sd = 0.2)

  reg <- train_gbm(
    x,
    y_reg,
    nrounds = 5,
    task = "regression",
    seed = 13
  )
  reg_meta <- statistical_learning_metadata(reg)
  expect_equal(reg_meta$objective, "reg:squarederror")
  expect_equal(reg_meta$performance$assessment, "apparent_training")
  expect_true(all(c("rmse", "mae", "r_squared") %in% names(reg_meta$performance)))

  y_bin <- factor(ifelse(x[, 1] > 0, "yes", "no"))
  cls <- train_gbm(
    x,
    y_bin,
    nrounds = 5,
    task = "binary",
    seed = 14
  )
  cls_meta <- statistical_learning_metadata(cls)
  expect_equal(cls_meta$objective, "binary:logistic")
  expect_true(all(c("auc", "log_loss", "accuracy") %in% names(cls_meta$performance)))
  expect_equal(length(cls_meta$class_levels), 2)
})

test_that("train_gbm validates binary labels and dimensions", {
  skip_if_not_installed("xgboost")

  x <- matrix(rnorm(30), nrow = 10)

  expect_error(
    train_gbm(x, rnorm(9)),
    "one outcome"
  )

  expect_error(
    train_gbm(x, c(rep(0, 8), 1, 2), task = "binary"),
    "coded 0/1"
  )
})

test_that("train_neural_net resolves task and records preprocessing", {
  skip_if_not_installed("nnet")

  set.seed(4)
  x <- matrix(rnorm(120), nrow = 60)
  y_reg <- 1.5 * x[, 1] - x[, 2] + rnorm(60, sd = 0.4)

  reg <- train_neural_net(
    x,
    y_reg,
    size = 3,
    task = "regression",
    scale_inputs = TRUE,
    maxit = 50,
    seed = 15
  )
  reg_meta <- statistical_learning_metadata(reg)
  expect_equal(reg_meta$task, "regression")
  expect_true(reg_meta$scale_inputs)
  expect_equal(length(reg_meta$center), ncol(x))
  expect_true(all(c("rmse", "mae", "r_squared") %in% names(reg_meta$performance)))

  y_bin <- factor(ifelse(x[, 1] + x[, 2] > 0, "event", "no_event"))
  cls <- train_neural_net(
    x,
    y_bin,
    size = 3,
    task = "auto",
    maxit = 50,
    seed = 16
  )
  cls_meta <- statistical_learning_metadata(cls)
  expect_equal(cls_meta$task, "binary")
  expect_equal(length(cls_meta$class_levels), 2)
  expect_true(all(c("auc", "log_loss", "accuracy") %in% names(cls_meta$performance)))
})

test_that("performance helper rejects unrelated models", {
  fit <- stats::lm(mpg ~ wt, data = mtcars)

  expect_error(
    statistical_learning_performance(fit),
    "does not contain"
  )
})
