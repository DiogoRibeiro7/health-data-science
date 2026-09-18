# Troubleshooting Guide

This guide lists common issues encountered when using the toolkit and offers
suggestions for recovery.

## Missing Packages

If you encounter errors about missing R packages, run:

```bash
Rscript scripts/install_r_deps.R
```

This installs the required dependencies declared in `DESCRIPTION`. Use
`Rscript scripts/install_r_deps.R --dev` for developer tooling. Internet
access is required.

## File Not Found

Functions such as `read_csv_safely()` and `run_lca()` verify that input files
exist. Ensure paths are correct and relative to the project root. Use
`require_data_file()` to validate paths explicitly.

## LCA Model Fails to Converge

Convergence failures often stem from poorly scaled variables or insufficient
classes. Review the configuration passed to `run_lca()` and consider adjusting
`nclass`, `maxiter`, or removing problematic variables. The log output will
contain diagnostic hints.

## API Authentication Errors

When working with the plumber API, confirm that valid credentials are supplied
and that the authentication service is reachable. Error responses intentionally
avoid exposing internal details; consult `collect_diagnostics()` for session
info if issues persist.

