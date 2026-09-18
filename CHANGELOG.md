# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Added `tidy_competing_risks()` for interpretable Fine-Gray coefficient tables with Wald confidence intervals.
- Added recurrent-event Cox modelling with Andersen-Gill and PWP total-time risk-set definitions.
- Added `tidy_recurrent_events()` for robust Wald summaries and hazard-ratio confidence intervals.
- Added validated multi-state transition structures and transition-specific cause-specific Cox models.
- Added `tidy_multistate_cox()` for transition-level robust Wald summaries.
- Added nonparametric Aalen-Johansen state-occupation probabilities.
- Added longitudinal Gaussian mixed-effects modelling with random-intercept and random-slope structures.
- Added `tidy_longitudinal_mixed()` for fixed-effect Wald summaries without implicit denominator-df approximations.
- Added `longitudinal_variance_components()` for random-effect and residual variance summaries.
- Added delta-adjusted pattern-mixture sensitivity analysis for originally missing continuous outcomes.
- Added subgroup-specific delta adjustment and Rubin-rule pooling diagnostics.
- Added `find_delta_tipping_point()` for explicit grid-based tipping-point analysis.
- Added explicit propensity-score designs for ATE, ATT, and overlap-population estimands.
- Added propensity-score overlap, effective-sample-size, and standardized-mean-difference balance diagnostics.
- Added weighted marginal outcome contrasts with fixed-weight Wald inference.
- Added doubly robust ATE and ATT estimation with augmented inverse-probability scores.
- Added influence-function Wald inference for doubly robust treatment-effect estimates.
- Added explicit two-stage least-squares IV models with first-stage partial F and partial R-squared diagnostics.
- Added `instrumental_variable_diagnostics()` and `tidy_instrumental_variable()` with weak-instrument and overidentification reporting.
- Added group-time difference-in-differences for staggered adoption with never-treated and not-yet-treated controls.
- Added cohort-time ATT tables, event-study/group/calendar aggregations, and pre-treatment Wald diagnostics.
- Added explicit sharp regression-discontinuity modelling with bandwidth, kernel, and local-polynomial controls.
- Added RD covariate-continuity and running-variable density diagnostics.
- Added `tidy_regression_discontinuity()` for transparent local-effect inference at the cutoff.
- Added `rd_bandwidth_sensitivity()` for prespecified local-bandwidth robustness grids.
- Added `rd_placebo_cutoffs()` for prespecified placebo-threshold diagnostics.
- Added a causal-API migration guide covering propensity, IV, DiD, and RD workflows.
- Added API-integrity tests that reject duplicate export directives and protect historical ICER argument aliases.
- Added structural tests that keep advanced statistical methods split into focused source modules.
- Added `tidy_cox_model()` as a common fixed-effect summary for advanced Cox models.
- Added dedicated documentation for time-varying, frailty, and landmark Cox analyses.
- Added `pharmacovigilance_signal()` with PRR, ROR, approximate confidence intervals, zero-cell correction, chi-square diagnostics, and a descriptive Evans-style screening flag.
- Added structured clinical prediction rules with `fit_clinical_prediction_rule()`, `tidy_clinical_prediction_rule()`, and `clinical_prediction_performance()`.
- Added apparent clinical-prediction performance summaries including AUC, Brier score, mean calibration error, sensitivity, specificity, PPV, and NPV.
- Added dedicated clinical-analytics documentation covering pharmacovigilance and prediction-model interpretation.
- Added explicit pairwise biomarker analysis with comparison metadata, multiplicity control, fold-change thresholds, tidy results, and discovery summaries.
- Added dedicated biomarker-analysis documentation.
- Added explicit statistical-learning metadata and performance helpers for ranger, xgboost, and nnet models.
- Added dedicated documentation for statistical-learning task, preprocessing, and performance contracts.
- Added Bayesian posterior summaries, sampling diagnostics, posterior predictive draws, and tidy BMA coefficient summaries.
- Added dedicated Bayesian-methods documentation covering sampling controls, diagnostics, and predictive checking.

### Changed
- Re-baselined package metadata for the 0.2.0 development line.
- Standardised the `testthat` package test entry point and enabled testthat edition 3.
- Added `R CMD check --as-cran` to CI on the current and previous R releases.
- Kept coverage enforcement as a separate release-version check with an 80% minimum.
- Removed the placeholder deployment job until a real container registry and release process are configured.
- Replaced the obsolete next-steps checklist with a statistical roadmap through version 1.0.0.
- Restored `tests/` to source-package builds so `R CMD check` exercises the package test suite.
- Declared optional dependencies used by advanced statistical methods in `Suggests`.
- Hardened `fit_competing_risks()` with explicit event and censoring codes, input validation, missing-value policy, and fitted-sample metadata.
- Deprecated the legacy causal wrappers `estimate_causal_effect()`, `match_cohort()`, `propensity_stratification()`, `instrumental_variable()`, `difference_in_differences()`, and `regression_discontinuity()` while preserving their historical behavior during the migration window.
- Removed duplicate `NAMESPACE` exports for `init_logging()`, `collect_diagnostics()`, and `cost_effectiveness()`.
- Canonicalised `cost_effectiveness()` so package load order no longer determines its public argument names; both historical naming conventions remain accepted.
- Split the former `advanced_statistics.R` monolith into latent-model, survival, statistical-learning, clinical-analytics, and Bayesian modules without changing the public function names.
- Removed shadowed `cost_effectiveness()` definitions from advanced statistics and population-health sources; the canonical API now has one executable definition.
- Hardened `fit_time_varying_cox()` with explicit `tt()` validation, time-transform handling, tie/missingness controls, and fitted-sample metadata.
- Hardened `fit_frailty_cox()` with explicit `frailty()` validation, tie/missingness controls, and fitted-sample metadata.
- Hardened `landmark_cox()` with an explicit landmark-time column, landmark risk-set validation, optional time-origin reset, and rejection of counting-process responses.
- Hardened `detect_pharmacovigilance()` with explicit 2 x 2 count validation while preserving its scalar PRR return type.
- Reimplemented `develop_clinical_prediction_rule()` on top of the structured prediction-rule API while preserving its historical coefficient-vector return type.
- Hardened biomarker discovery with explicit feature/sample validation and a structured pairwise limma workflow while preserving the legacy `topTable()` helper.
- Hardened `train_rf_missing()` with explicit task resolution, supported ranger missing-data modes, hyperparameter validation, and out-of-bag metadata.
- Hardened `train_gbm()` with explicit regression/binary objectives, validated outcomes, controlled hyperparameters, and apparent training metrics.
- Hardened `train_neural_net()` with explicit regression/binary outputs, optional predictor scaling, validated outcomes, and retained preprocessing metadata.
- Hardened `run_mcmc()` with explicit chains, warmup, thinning, seed, NUTS adaptation, and tree-depth controls.
- Fixed `bayesian_model_averaging()` to require/pass an explicit GLM family and added model-search/missingness controls.
- Hardened `fit_hierarchical_bayes()` with explicit sampling controls, hierarchical-formula validation, and fitted-sample metadata.
- Clarified posterior predictive checking by separating observed-data PPCs from new-data posterior predictive draws.

## [0.1.0]

### Added
- Initial package structure and health-data-science utilities.
- Data-processing, database, ETL, reporting, visualisation, monitoring, security, and deployment helpers.
- Statistical functionality for latent-class analysis, regression, model validation, population health, and real-world evidence workflows.
- Test suite, documentation, examples, container configuration, and infrastructure scaffolding.
