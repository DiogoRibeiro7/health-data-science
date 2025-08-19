# Quickstart Guide

This guide walks you through installing dependencies and running the sample workflow without needing access to real NCDB data.

## 1. Set up the environment
```bash
bash scripts/setup.sh
```
This script verifies system requirements, installs missing libraries, and restores the R package library via `renv`.

## 2. Generate synthetic sample data
```bash
Rscript scripts/generate_sample_data.R
```
The generated CSV in `data/puf_early.csv` contains only synthetic records.

## 3. Run the latent class analysis
```bash
Rscript scripts/ncdb_LCA.R --input data/puf_early.csv --output data/lca_earlypuf.RData
```

## 4. Run the regression models
```bash
Rscript scripts/ncdbearly_Regression.R --input data/lca_earlypuf.RData
```

For a more detailed walkthrough, see [tutorials/basic_workflow.md](tutorials/basic_workflow.md).
