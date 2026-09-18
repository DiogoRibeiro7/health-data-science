# Dependency Policy

`healthdatascience` separates a small package core from optional feature engines.

## Core dependencies

The package must be able to build, install, and load with only the packages listed in `Imports`.

For the 0.3.0 release line, the hard dependency surface is intentionally small:

- `logger`
- `yaml`
- `uuid`
- `jsonlite`

These packages support package startup and the logging/configuration layer.

## Optional feature engines

Method-specific and operational packages live in `Suggests`. Examples include:

- survival and event-history engines such as `survival` and `cmprsk`
- causal engines such as `AER`, `did`, `rdrobust`, and `rddensity`
- Bayesian engines such as `rstan`, `rstanarm`, `BMA`, and `bayesplot`
- latent-model engines such as `LMest`, `flexmix`, `BayesLCA`, and `randomLCA`
- statistical-learning engines such as `ranger`, `xgboost`, and `nnet`
- reporting, Shiny, database, spatial, and visualization packages

Calling a function that needs an optional package should fail clearly when that package is unavailable. Package functions do not install dependencies or attach namespaces automatically.

## Installation modes

A minimal package installation installs only hard dependencies:

```bash
R CMD INSTALL .
```

Repository setup installs the complete optional toolkit:

```bash
bash scripts/setup.sh
```

The shared installer can also be called directly:

```bash
# Core dependencies only
Rscript scripts/install_r_deps.R

# Full optional feature surface
Rscript scripts/install_r_deps.R --all

# Full feature surface plus developer tooling
Rscript scripts/install_r_deps.R --all --dev
```

## CI contract

Pull requests perform syntax and diff checks only.

Pushes to `main` validate that the package builds, installs, and loads with the hard dependency set. Optional examples, vignettes, tests, compatibility checks, coverage, Docker builds, and vulnerability scanning belong to the separate Release Gate workflow.

This distinction is deliberate: a package should not require every possible statistical or operational engine merely to load its core namespace.

## Adding a dependency

When adding a package dependency:

1. add it to `Imports` only if package startup or unconditional core behavior requires it;
2. otherwise add it to `Suggests`;
3. namespace-qualify external calls with `pkg::fun`;
4. use `ensure_packages()` when a clearer optional-dependency error is useful;
5. do not install or attach packages dynamically inside package functions;
6. update tests and method documentation for the feature that requires it.

The authoritative dependency declaration is `DESCRIPTION`.
