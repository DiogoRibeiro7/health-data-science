# 0.3.0 Release Readiness

This page tracks whether `healthdatascience` is ready to be tagged as version 0.3.0.

## Current status

**Status: release candidate, not yet tag-ready.**

The package metadata has moved to 0.3.0 and the main statistical API audit is substantially complete. A small number of correctness and release-engineering checks remain before a 0.3.0 tag should be created.

## Completed

- Package version metadata updated to 0.3.0.
- Duplicate exports and duplicate statistical definitions removed.
- Legacy causal wrappers deprecated with documented migration paths.
- Advanced statistical code split into focused modules.
- Survival, recurrent-event, multi-state, longitudinal, missing-data sensitivity, propensity, doubly robust, IV, DiD, RD, clinical, biomarker, statistical-learning, Bayesian, and latent-model APIs added or hardened.
- ATT doubly robust inference corrected to use the ratio-estimator influence function.
- IV over-identification rank corrected to use incremental estimable rank conditional on included covariates.
- RD sensitivity now selects the named robust bias-corrected inference row explicitly.
- Fixed-bandwidth placebo RD windows that touch or cross the true cutoff are rejected.
- CI runs changed-file linting rather than failing new pull requests on unrelated historical lint debt.
- Normal `main` CI builds, installs, and loads the package with only the four hard dependencies; the previous-R matrix, full optional dependencies, tests/examples/vignettes, coverage, and security scan run in the separate Release Gate workflow.
- The exported API contains no duplicate `NAMESPACE` entries.
- Documentation index and public API inventory added.
- Removed the stale incomplete `renv` snapshot and unified local, CI, and container dependency installation around `DESCRIPTION`.

## Release-engineering gates

A 0.3.0 tag should be created only after all of the following are true:

- [x] IV rank/over-identification audit completed and regression-tested.
- [x] RD robust-row extraction made explicit and regression-tested.
- [x] Fixed-bandwidth placebo windows cannot cross the true cutoff.
- [ ] Release Gate `R CMD check --as-cran` is clean on the supported R versions.
- [ ] Release Gate test coverage is at least 80%.
- [ ] Release Gate container vulnerability scan passes.
- [ ] Documentation links in `README.md` and `docs/README.md` are valid.
- [ ] `DESCRIPTION`, `NAMESPACE`, `CHANGELOG.md`, and the API inventory agree on the release surface.
- [x] Dependency management has one source of truth: `DESCRIPTION`; the stale incomplete `renv` snapshot has been removed.
- [ ] No deprecated causal wrapper is removed before the documented major-release migration point.

## Release policy

Version 0.3.0 is an API-hardening release, not a claim that every historical utility is already part of the eventual 1.0 stable surface. New statistical interfaces should continue to make estimands, assumptions, missing-value handling, seeds, diagnostics, and uncertainty explicit.
