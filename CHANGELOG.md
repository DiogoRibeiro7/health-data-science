# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Changed
- Re-baselined package metadata for the 0.2.0 development line.
- Standardised the `testthat` package test entry point and enabled testthat edition 3.
- Added `R CMD check --as-cran` to CI on the current and previous R releases.
- Kept coverage enforcement as a separate release-version check with an 80% minimum.
- Removed the placeholder deployment job until a real container registry and release process are configured.
- Replaced the obsolete next-steps checklist with a statistical roadmap through version 1.0.0.
- Restored `tests/` to source-package builds so `R CMD check` exercises the package test suite.
- Declared optional dependencies used by advanced statistical methods in `Suggests`.

## [0.1.0]

### Added
- Initial package structure and health-data-science utilities.
- Data-processing, database, ETL, reporting, visualisation, monitoring, security, and deployment helpers.
- Statistical functionality for latent-class analysis, regression, model validation, population health, and real-world evidence workflows.
- Test suite, documentation, examples, container configuration, and infrastructure scaffolding.
