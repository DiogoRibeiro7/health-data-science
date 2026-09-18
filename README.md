# Health Data Science

[![CI](https://github.com/DiogoRibeiro7/health-data-science/actions/workflows/ci.yml/badge.svg)](https://github.com/DiogoRibeiro7/health-data-science/actions/workflows/ci.yml)
[![Release Gate](https://github.com/DiogoRibeiro7/health-data-science/actions/workflows/release-gate.yml/badge.svg)](https://github.com/DiogoRibeiro7/health-data-science/actions/workflows/release-gate.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

`healthdatascience` is an R toolkit for clinical, epidemiological, and
real-world health-data analysis.

Its main goal is not to automate statistical decisions. It is to make them
**explicit**: the estimand, assumptions, risk set, missing-data policy,
uncertainty calculation, diagnostic checks, and model limitations should be
visible in the analysis rather than hidden behind a convenience wrapper.

The package grew from reproducible health-data workflows into a broader
statistical toolkit covering event-history analysis, longitudinal data,
missing-data sensitivity, causal inference, latent-variable models, Bayesian
methods, clinical prediction, biomarkers, statistical learning, and
population-health analysis.

> **Current status:** version 0.3.0 is the API-hardening release line. The
> statistical correctness blockers identified for the release candidate have
> been addressed; release-engineering gates are tracked separately in the
> [0.3.0 release-readiness checklist](docs/release_readiness_0.3.0.md).

## Why this package exists

Health-data analysis often fails in fairly ordinary ways: the target estimand
is left implicit, a model is chosen because a package makes it convenient,
missing observations disappear silently, diagnostics are treated as optional,
or a fitted coefficient is interpreted more broadly than the design permits.

This project takes the opposite approach.

- **Define the scientific target first.** ATE and ATT, cumulative incidence,
  cause-specific hazards, recurrent-event risk sets, state occupancy, and
  local RD effects are different quantities and should remain different in the
  API.
- **Expose assumptions and diagnostics.** Weak instruments, propensity-score
  overlap, singular mixed models, MCMC diagnostics, RD placebo checks, and
  missing-data sensitivity are part of the analysis, not afterthoughts.
- **Prefer transparent models.** Interpretable statistical models are preferred
  when they answer the scientific question adequately. Machine learning is
  available where prediction is the actual goal.
- **Keep uncertainty attached to the estimand.** Standard errors, confidence or
  credible intervals, influence functions, and robust covariance choices are
  treated as first-class outputs.
- **Avoid silent repair.** The package generally fails explicitly on invalid
  data structures or ambiguous model assumptions rather than guessing what the
  analyst intended.

## What you can study

The toolkit is organised around statistical questions rather than around a
single modelling framework.

### Event histories and disease trajectories

Analyse time-to-event data with competing risks, recurrent events, multi-state
models, time-varying Cox effects, frailty models, and landmark analyses. The
interfaces distinguish cause-specific hazards, subdistribution hazards, and
state-occupation probabilities rather than presenting them as interchangeable
survival summaries.

### Longitudinal and incomplete data

Fit repeated-measures models with explicit random-effects structures and inspect
variance components and singularity. Explore departures from missing at random
with delta-adjusted pattern-mixture sensitivity analyses and tipping-point
workflows.

### Causal and real-world evidence

Design propensity-score analyses for explicit target populations, assess
overlap and balance, estimate weighted and doubly robust treatment effects, and
work with instrumental variables, staggered-adoption difference in differences,
and regression discontinuity designs. Diagnostics are designed to expose weak
identification and design sensitivity rather than certify causal validity.

### Latent structure and Bayesian modelling

Work with latent transition models, finite mixtures, latent class models, and
hierarchical Bayesian models while retaining engine-native fitted objects and
explicit metadata. Bayesian helpers expose sampling controls, posterior
summaries, MCMC diagnostics, and posterior predictive checks.

### Clinical prediction and biomarkers

Develop binary clinical prediction models with transparent apparent-performance
summaries, analyse pharmacovigilance disproportionality without presenting it as
causal evidence, and run explicit pairwise biomarker analyses with multiplicity
control and effect-size thresholds.

### Statistical learning and population health

Use random forests, gradient boosting, and neural networks when prediction is
the objective, with task type and preprocessing recorded explicitly. The wider
toolkit also contains survey, spatial, epidemiological, data-quality, reporting,
and operational utilities used in health-data workflows.

For the complete method catalogue, assumptions, and implementation notes, see
the [documentation hub](docs/README.md).

## A small example

The following synthetic example targets an average treatment effect. The
propensity design and the treatment-effect estimator are separate on purpose:
the design can be inspected before an outcome estimate is interpreted.

```r
library(healthdatascience)

set.seed(42)

n <- 1000
x <- rnorm(n)
z <- rnorm(n)

p <- plogis(-0.2 + 0.8 * x - 0.4 * z)
treatment <- rbinom(n, 1, p)
outcome <- 2 * treatment + 1.2 * x - 0.7 * z + rnorm(n)

dat <- data.frame(
  treatment = treatment,
  outcome = outcome,
  x = x,
  z = z
)

design <- fit_propensity_design(
  dat,
  treatment = "treatment",
  covariates = c("x", "z"),
  estimand = "ATE"
)

propensity_overlap(design)
propensity_balance(design)

estimate_doubly_robust(
  design,
  outcome = "outcome"
)
```

The point is not that this is the only way to estimate an ATE. The point is
that the target population, treatment design, diagnostics, nuisance models, and
uncertainty calculation are inspectable and testable.

## Installation

The project is currently developed from GitHub.

```bash
git clone https://github.com/DiogoRibeiro7/health-data-science.git
cd health-data-science
R CMD INSTALL .
```

For development work, system dependencies, synthetic example data, and the
repository setup workflow, start with the
[Quickstart Guide](docs/quickstart.md).

The project contains an `renv.lock`, but the lockfile is still being reconciled
with the 0.3.0 dependency surface and should not yet be treated as the final
release snapshot.

## Documentation

The README is intentionally an overview. Detailed usage belongs in the
documentation.

- **Start here:** [Documentation hub](docs/README.md)
- **Methods and assumptions:** [Statistical methods](docs/statistical_methods.md)
- **First repository workflow:** [Quickstart](docs/quickstart.md)
- **Causal API changes:** [Causal API migration](docs/causal_api_migration.md)
- **Everything currently exported:** [Public API inventory](docs/api_inventory.md)
- **Release status:** [0.3.0 release readiness](docs/release_readiness_0.3.0.md)
- **Long-term direction:** [Roadmap to 1.0.0](NEXT_STEPS.md)

Method-specific notes cover survival analysis, instrumental variables,
difference in differences, regression discontinuity, Bayesian methods, latent
models, clinical analytics, biomarkers, statistical learning, population
health, and real-world evidence.

## Project origins

The repository originally included reproducible scripts for studying disparities
in care among early-stage pancreatic-cancer patients using latent class analysis
and regression. Those workflows remain available as examples, but the package
is no longer organised around that single study.

Synthetic data can be generated locally, so the repository can be explored
without access to restricted NCDB data.

## Interfaces beyond R scripts

The repository also contains a Shiny application and a Plumber API used to test
interactive and service-oriented workflows. They are deliberately secondary to
the statistical core.

Operational material — API deployment, monitoring, database/ETL workflows,
security, and backup — is documented under
[Package use and operations](docs/README.md#package-use-and-operations).

## Development and releases

Development uses `main` as the single long-lived branch.

Pull requests run lightweight syntax and diff checks. Merges to `main` run the
current-R package check. The heavier compatibility matrix, full optional
dependencies, coverage, container build, and vulnerability scan live in the
separate **Release Gate** workflow and are run when preparing a release.

The public surface is still being consolidated before 1.0.0. Exported functions
in 0.3.0 are listed in the
[API inventory](docs/api_inventory.md); that inventory is not a promise that
every historical utility will remain in the eventual 1.0 stable API.

## License

MIT. See [LICENSE](LICENSE).

## Author

**Diogo Ribeiro**
Faculty of Media Arts and Design, Technical University of Porto
ORCID: https://orcid.org/0009-0001-2022-7072
