# Statistical Learning Helpers

The statistical-learning helpers now make task type and preprocessing assumptions explicit rather than relying on engine defaults.

`train_rf_missing()` supports regression and classification through `ranger`. The task can be inferred from the response or specified directly. Missing-predictor handling is explicit through ranger's supported `na.learn`, `na.omit`, or `na.fail` behavior. The returned ranger object keeps its native class and receives a `healthdatascience` metadata attribute containing the task, response, predictors, hyperparameters, class levels when relevant, and out-of-bag performance.

`train_gbm()` uses xgboost with an explicit objective: `reg:squarederror` for regression and `binary:logistic` for binary classification. It validates the outcome coding and records the selected hyperparameters. The reported RMSE/MAE/R² or AUC/log-loss/accuracy are computed on the same data used to fit the model. They are therefore apparent training metrics, not validation results.

`train_neural_net()` uses `nnet` with linear output for regression and logistic/entropy output for binary classification. Optional predictor standardization records the exact center and scale vectors in the model metadata so preprocessing is inspectable rather than implicit.

`statistical_learning_metadata()` exposes the retained metadata and `statistical_learning_performance()` returns the stored performance summary.

These wrappers remain intentionally narrow. They do not perform hyperparameter tuning, feature selection, nested cross-validation, probability calibration, or external validation automatically. Those steps should remain explicit parts of the analysis design rather than being hidden inside training helpers.
