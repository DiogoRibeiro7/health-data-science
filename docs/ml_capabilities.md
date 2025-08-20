# Machine Learning Capabilities

This package now includes a suite of machine learning helpers for automated model
selection, advanced modelling techniques and MLOps workflows.

## Automated Model Selection
- `auto_select_lca()` performs cross-validated grid search to choose the optimal
  number of latent classes.
- `auto_feature_engineering()` and `select_important_features()` streamline data
  preparation and feature selection.

## Advanced Models
- `train_ensemble()` combines multiple algorithms for improved predictions.
- `fit_time_series()`, `fit_survival_model()` and `estimate_causal_effect()`
  provide specialised analyses for longitudinal, time-to-event and causal
  inference scenarios.

## MLOps and Deployment
- Functions such as `register_model()`, `validate_model()` and
  `detect_model_drift()` support reproducibility and monitoring.
- `score_model_api()` and `batch_predict()` enable both online and offline
  prediction workflows.

## Interpretability
- Tools like `compute_shap()`, `feature_importance()` and
  `partial_dependence_data()` help explain model behaviour.
