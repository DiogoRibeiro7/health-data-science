# Contributing

Thank you for your interest in contributing to this project! Please follow these guidelines to keep the workflow smooth:

## Getting Started
- Fork the repository and create your branch from `main`.
- Install system dependencies with `bash scripts/install_system_deps.sh`.
- Install R dependencies and developer tooling with `Rscript scripts/install_r_deps.R --dev`.

## Code Style
- Run `pre-commit run --files <changed files>` before committing to apply linters and formatting.
- Write tests for new functions using [testthat](https://testthat.r-lib.org/).

## Pull Requests
- Include a clear description of the changes and any relevant issues.
- Ensure all tests pass and CI checks are green.

For questions, please contact Diogo Ribeiro at [diogo.debastos.ribeiro@gmail.com](mailto:diogo.debastos.ribeiro@gmail.com) or [dfr@esmad.ipp.pt](mailto:dfr@esmad.ipp.pt).
