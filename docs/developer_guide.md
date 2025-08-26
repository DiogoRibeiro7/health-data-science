# Developer Guide

This document outlines key conventions for contributors extending the package.

## Coding Style

* Follow the tidyverse style guide for R code.
* Use roxygen2 comments to document all exported functions with examples and
  parameter descriptions.

## Testing

* Place unit tests under `tests/testthat/` using `testthat`.
* Use the helper functions in `tests/testthat/helper-mock-data.R` to generate
  mock datasets rather than relying on real NCDB data.

## Documentation

* Vignettes live in `vignettes/` and are built with knitr.
* Long-form technical notes or troubleshooting content should be placed in the
  `docs/` directory.

## Pull Requests

* Run `renv::snapshot()` after adding or removing dependencies.
* Ensure `R -q -e 'testthat::test_dir("tests/testthat")'` passes before
  submitting.

