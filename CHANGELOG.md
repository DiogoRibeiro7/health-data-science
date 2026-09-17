# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Added `tidy_competing_risks()` for interpretable Fine-Gray coefficient tables with Wald confidence intervals.
- Added recurrent-event Cox modelling with Andersen-Gill and PWP total-time risk-set definitions.
- Added `tidy_recurrent_events()` for robust Wald summaries and hazard-ratio confidence intervals.

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
