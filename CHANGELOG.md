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

## [0.1.0]

### Added
- Initial package structure and health-data-science utilities.
- Data-processing, database, ETL, reporting, visualisation, monitoring, security, and deployment helpers.
- Statistical functionality for latent-class analysis, regression, model validation, population health, and real-world evidence workflows.
- Test suite, documentation, examples, container configuration, and infrastructure scaffolding.
