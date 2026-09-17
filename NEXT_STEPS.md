# Roadmap to 1.0.0

This roadmap focuses on turning `healthdatascience` into a coherent, statistically rigorous R package rather than a collection of loosely connected health-data utilities.

## 0.2.x — Package baseline and reliability

### Package structure
- Keep `main` as the single long-lived branch.
- Maintain a valid `DESCRIPTION`, `NAMESPACE`, licence declaration, and standard `testthat` entry point.
- Ensure all exported functions are documented and all imports are explicit.
- Remove obsolete scripts, duplicated helpers, and dead configuration as they are identified.

### Continuous integration
- Run linting, `R CMD check --as-cran`, and unit tests on the current R release and previous release.
- Keep test coverage at or above 80% while prioritising meaningful statistical tests over line-count inflation.
- Retain container vulnerability scanning, but only add deployment automation when a real registry and release process exist.
- Add package build artefacts or check logs to CI only where they materially improve debugging.

### Reproducibility
- Keep `renv.lock` synchronized with package requirements.
- Document system-level dependencies required by spatial, database, Arrow, and reporting features.
- Provide deterministic examples and small synthetic datasets for tests and vignettes.

## 0.3.x — Statistical API audit

### API consistency
- Review exported functions for naming, argument conventions, return structures, error handling, and missing-value behaviour.
- Define consistent S3 result objects for model-fitting functions where appropriate.
- Separate analysis functions from plotting, reporting, storage, and operational infrastructure.
- Reduce hidden side effects and make seeds, parallelism, and logging explicit.

### Validation
- Add analytical tests against known closed-form results where possible.
- Add regression tests against trusted R packages for established estimators.
- Add edge-case tests for empty strata, sparse events, separation, singular fits, non-positive times, and degenerate covariance matrices.

## 0.4.x — Survival and event-history methods

- Add competing-risks analysis with cumulative-incidence estimation and cause-specific modelling.
- Add recurrent-event models, including Andersen-Gill style workflows and event-number-aware summaries.
- Add multi-state modelling utilities with explicit transition matrices and transition-probability summaries.
- Standardise censoring, event coding, time scales, diagnostics, and plotting across event-history functions.
- Add simulation-based tests with known generating mechanisms.

## 0.5.x — Longitudinal and repeated-measures analysis

- Add longitudinal mixed-effects workflows for continuous and non-Gaussian outcomes.
- Add subject-level trajectory summaries and covariance diagnostics.
- Add tools for irregular observation times and informative visit patterns where feasible.
- Define interfaces that keep model specification transparent rather than hiding formulas behind automated model selection.

## 0.6.x — Missing-data sensitivity analysis

- Extend multiple-imputation support beyond a single completed-analysis workflow.
- Add pooling helpers with explicit estimands and Rubin-style uncertainty summaries.
- Add delta-adjustment and tipping-point style sensitivity analyses for departures from missing at random.
- Record imputation diagnostics, convergence information, and between-imputation variability in structured outputs.

## 0.7.x — Causal and real-world evidence methods

- Define causal estimands explicitly: ATE, ATT, risk difference, risk ratio, odds ratio, and time-to-event contrasts where supported.
- Add inverse-probability weighting diagnostics, overlap checks, effective sample size, and weight truncation utilities.
- Add doubly robust estimators where assumptions and implementation can be made transparent.
- Add target-trial emulation helpers for eligibility, treatment assignment, time zero, follow-up, outcome definition, and censoring.
- Expand negative-control and sensitivity-analysis utilities for observational studies.

## 0.8.x — Population-health and survey methods

- Consolidate survey-weighted estimators and design-based variance calculations.
- Add direct and indirect standardisation utilities.
- Add rate ratios, rate differences, attributable fractions, and uncertainty intervals.
- Improve spatial epidemiology support with explicit neighbourhood structures and reproducible spatial diagnostics.

## 0.9.x — Documentation and release candidate

- Reorganise vignettes around statistical questions rather than repository features.
- Add end-to-end examples for clinical, epidemiological, and real-world evidence workflows using synthetic or redistributable data.
- Document assumptions, estimands, diagnostics, and failure modes for every modelling family.
- Audit examples for reproducibility under a clean R environment.
- Remove deprecated interfaces and freeze the public API intended for 1.0.0.

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
