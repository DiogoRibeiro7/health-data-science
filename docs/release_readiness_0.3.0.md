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
- CI runs changed-file linting rather than failing new pull requests on unrelated historical lint debt.
- Current and previous R releases remain in the CI matrix.
- The exported API contains no duplicate `NAMESPACE` entries.
- Documentation index and public API inventory added.

## Blocking correctness audits

### Instrumental-variable over-identification rank

The current Sargan degrees of freedom are based on the raw excluded-instrument model-matrix rank. Before release, this should be audited against the **incremental estimable rank conditional on included covariates**. The first-stage nested-model numerator degrees of freedom are already available and should be reconciled with the over-identification calculation.

### Regression-discontinuity result extraction

The RD sensitivity helper currently extracts the final common row across `coef`, `se`, `pv`, and `ci`. Before release, this must be checked against the documented `rdrobust` result structure so the code explicitly selects robust bias-corrected inference rather than depending on row position.

### RD placebo windows

For an analyst-supplied fixed placebo bandwidth, a placebo window can currently overlap the true treatment cutoff. Before release, fixed-bandwidth placebo checks should reject windows that cross the true discontinuity, because such a placebo estimate can mechanically contain the true treatment jump.

## Release-engineering gates

A 0.3.0 tag should be created only after all of the following are true:

- [ ] IV rank/over-identification audit completed and regression-tested.
- [ ] RD robust-row extraction made explicit and regression-tested.
- [ ] Fixed-bandwidth placebo windows cannot cross the true cutoff.
- [ ] `R CMD check --as-cran` is clean on the supported CI R versions.
- [ ] Test coverage is at least 80%.
- [ ] Container vulnerability scan passes.
- [ ] Documentation links in `README.md` and `docs/README.md` are valid.
- [ ] `DESCRIPTION`, `NAMESPACE`, `CHANGELOG.md`, and the API inventory agree on the release surface.
- [ ] `renv.lock` is synchronized with the declared package dependencies; the current lockfile is incomplete and must not be treated as a reproducible release snapshot.
- [ ] No deprecated causal wrapper is removed before the documented major-release migration point.

## Release policy

Version 0.3.0 is an API-hardening release, not a claim that every historical utility is already part of the eventual 1.0 stable surface. New statistical interfaces should continue to make estimands, assumptions, missing-value handling, seeds, diagnostics, and uncertainty explicit.
