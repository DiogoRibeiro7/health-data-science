# Roadmap to 1.0.0

This roadmap reflects the package state after the 0.3.0 API-hardening work.

## 0.3.0 — Statistical API hardening

Delivered in this release line:

- package-baseline and CI reliability work
- duplicate export/definition cleanup
- explicit causal estimands and migration away from ambiguous legacy wrappers
- competing risks, recurrent events, multi-state models, and advanced Cox workflows
- longitudinal mixed-effects models
- missing-data delta sensitivity and tipping-point analysis
- propensity-score design diagnostics and doubly robust ATE/ATT estimation
- instrumental variables, staggered-adoption DiD, and regression discontinuity
- clinical prediction, pharmacovigilance, biomarker, statistical-learning, Bayesian, and latent-model wrapper hardening
- explicit metadata, diagnostics, missing-value policies, seeds, and tidy summaries across the modern statistical API

The 0.3.0 tag remains blocked until the release-readiness checklist is complete.

## 0.4.x — Correctness and validation

- complete the IV incremental-rank/over-identification audit
- make rdrobust robust-bias-corrected result extraction explicit
- prevent placebo RD bandwidth windows from crossing the true cutoff
- add simulation/reference tests for the principal causal, survival, longitudinal, and missing-data estimators
- add edge-case tests for sparse events, separation, singular fits, weak identification, degenerate covariance matrices, and extreme propensity scores
- audit remaining historical utilities for silent engine-default assumptions
- keep `R CMD check --as-cran` clean on supported R versions

## 0.5.x — Population health and survey consolidation

- standardise survey-weighted estimators and design-based uncertainty
- add direct and indirect standardisation utilities
- add rate ratios, rate differences, attributable fractions, and uncertainty intervals
- improve spatial epidemiology interfaces with explicit neighbourhood structures
- separate descriptive population-health utilities from causal estimators
- add reference/simulation tests for population-health methods

## 0.6.x — Reproducible workflows and validation

- add end-to-end synthetic-data workflows for clinical, epidemiological, and real-world evidence analyses
- add internal-validation workflows for prediction models, including bootstrap/cross-validation where appropriate
- add calibration summaries and decision-relevant prediction diagnostics without hiding resampling
- improve simulation utilities for event-history, causal, and longitudinal methods
- record reproducibility metadata consistently across stochastic workflows

## 0.7.x — API consolidation

- remove or further isolate historical operational wrappers that do not belong in the statistical core
- standardise result classes and metadata conventions where native engine classes are retained
- reduce the exported surface where functions are implementation details rather than public API
- continue the documented causal-wrapper deprecation window
- reconcile documentation examples with the API inventory

## 0.8.x — Documentation and package boundaries

- reorganise vignettes around statistical questions rather than repository features
- document assumptions, estimands, diagnostics, and failure modes for every modelling family
- clarify dependency boundaries between statistics, visualisation, storage, APIs, security, and deployment
- audit examples under a clean R environment
- document semantic versioning and release procedure

## 0.9.x — Release candidate

- freeze the public API intended for 1.0.0
- remove deprecated interfaces scheduled for removal
- run full reference/simulation validation
- verify all vignettes and examples from a clean environment
- complete final dependency and package-size audits
- require clean CI, coverage, and security checks

## 1.0.0 — Stable statistical toolkit

Version 1.0.0 should be released only when:

- `R CMD check --as-cran` is clean on supported R versions.
- The public API is documented and internally consistent.
- Core statistical methods have analytical, simulation, or trusted-reference validation.
- Test coverage remains at least 80% with meaningful branch and edge-case coverage.
- Reproducible vignettes cover the principal modelling workflows.
- Dependency boundaries between statistics, visualisation, storage, APIs, and deployment are clear.
- The package has a documented release process and semantic-versioning policy.

## Statistical design principles

Across all milestones:

- Prefer identifiable estimands and explicit assumptions over automated modelling.
- Prefer interpretable statistical models over machine learning when both answer the same scientific question.
- Expose diagnostics rather than silently correcting problematic data or model fits.
- Keep statistical computation independent from presentation and infrastructure layers.
- Validate new methods with mathematics, simulation, and external reference implementations where appropriate.
