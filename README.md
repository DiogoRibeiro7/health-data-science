# Health Data Science

This repository provides a health data science toolkit with reproducible workflows.
It currently includes scripts for reproducing analyses from the study
"Cluster identification of early-stage pancreas cancer patients at greatest risk for disparities of care" by Ugwuji N. Maduekwe,
Briana J. K. Stephenson, Jen Jen Yeh, Melissa Troester and Hanna K. Sanoff.

## Repository structure
- `scripts/ncdb_LCA.R` – example latent class analysis on early-stage NCDB data, saving results to `data/lca_earlypuf.RData`.
- `scripts/ncdbearly_Regression.R` – example regression models using the latent class assignments.
- `R/utils.R` – helper functions for package installation, directory creation, and data validation.
- `R/data_processing.R` – data quality assessment, imputation, outlier handling, feature engineering, and ETL utilities.
- `R/model_validation.R` – residual analysis, goodness-of-fit tests, bootstrap confidence intervals, cross-validation, and LCA diagnostics.
- `R/database.R` – database connectivity helpers with connection pooling and streaming queries.
- `R/etl.R` – incremental ETL pipelines and data lineage logging.
- `R/storage.R` – Parquet and cloud storage utilities for scalable datasets.
- `R/advanced_models.R` – ensemble, time-series, survival and causal modelling helpers.
- `R/ml_features.R` – automated feature engineering and selection utilities.
- `R/mlops.R` – model registry, validation and monitoring helpers.
- `R/interpretability.R` – SHAP values, feature importance and partial dependence.
- `R/deployment.R` – REST API scoring, batch prediction and deployment stubs.
- `R/population_health.R` – epidemiological and population health analysis utilities.
- `data/` – contains example input data (`puf_early.csv`) and stores intermediate outputs. See `data/README.md`.
- `app/` – Shiny application for interactive analysis with file upload and download.
- `api/` – Plumber REST API exposing latent class analysis endpoints.

## Prerequisites
Install R, pre-commit, and the system libraries required to compile common R packages:

```bash
sudo apt-get update -y
sudo apt-get install -y r-base python3-pip pre-commit \\
    libcurl4-openssl-dev libssl-dev libxml2-dev \\
    gdal-bin libgdal-dev libgeos-dev libproj-dev libicu-dev cmake
```

This repository uses [renv](https://rstudio.github.io/renv/) to manage package versions. After cloning the project, restore the R library with:

```bash
R -q -e 'renv::restore()'
```

A helper script is available at `scripts/install_system_deps.sh` to install these tools and libraries.
For a one-step setup that verifies your R version and restores packages, run:

```bash
bash scripts/setup.sh
```


## Getting Started
1. Clone the repository:
```bash
git clone <repo-url>
cd health-data-science
```
2. Install system libraries:
```bash
bash scripts/install_system_deps.sh
```
3. Restore R packages:
```bash
R -q -e 'renv::restore()'
```
4. Install pre-commit hooks:
```bash
pre-commit install
```
5. Generate the sample dataset:
```bash
Rscript scripts/generate_sample_data.R
```

For a condensed walkthrough, see the [Quickstart Guide](docs/quickstart.md). A step-by-step tutorial lives in `docs/tutorials/`.

## Documentation
Detailed references are provided in the `docs/` directory:

- [Data Requirements](docs/data_requirements.md)
- [Statistical Methods](docs/statistical_methods.md)
- [Performance Considerations](docs/performance.md)
- [Troubleshooting Guide](docs/troubleshooting.md)
- [Developer Guide](docs/developer_guide.md)
- [Database Connectivity and ETL](docs/database_etl.md)
- [Security and Compliance](docs/security_compliance.md)
- [Monitoring and Observability](docs/observability.md)
- [Deployment and CI/CD](docs/deployment_ci_cd.md)
- [Backup and Disaster Recovery](docs/backup_dr.md)

Package vignettes offer fully worked examples and can be viewed with:

```r
 browseVignettes('healthdatascience')
```

## Usage
1. Ensure `data/puf_early.csv` is present. A small synthetic sample is provided and can be regenerated with `scripts/generate_sample_data.R`. Replace it with the official NCDB PUF file for real analyses.
2. Run the latent class analysis (override defaults with `--input` and `--output` if desired):
   ```bash
   Rscript scripts/ncdb_LCA.R --input data/puf_early.csv --output data/lca_earlypuf.RData
   ```
   Use `--dry-run` to validate arguments without executing, `--quiet` to suppress
   progress output, or `--version` to print the script version.
   Parallel execution can be enabled by setting `parallel = TRUE` in the
   configuration file to accelerate model fitting on multi-core machines.
3. Run the regression models (accepts `--input` to specify the LCA results):
   ```bash
  Rscript scripts/ncdbearly_Regression.R --input data/lca_earlypuf.RData
  ```
   These scripts determine the project root automatically, so the default
   paths work even when invoked from outside the repository using absolute
   script locations. Progress bars and verbose messages provide feedback during
   lengthy operations. Execution time and memory usage are reported for major
  steps, and intermediate results are cached to speed up repeated runs. The
  optimization helpers support parallel processing for computationally
  intensive tasks.
The scripts attempt to install any missing R packages automatically.
Package installation is verified, the latent class analysis uses a fixed
random seed for reproducible results, and output directories are created
automatically when needed.

## Web interfaces

Launch the Shiny application for interactive analysis:

```bash
Rscript scripts/run_app.R
```

The dashboard offers a responsive layout with dark or light themes, interactive plotly charts, real-time metrics, collaborative notes, and role-aware access controls for healthcare analysts.

Start the REST API to run analyses programmatically:

```bash
Rscript scripts/run_api.R
```
Endpoints support either an `X-API-Key` header or an OAuth2 `Authorization`
Bearer token. Keys and roles are managed with `verify_api_key()` and
`rotate_api_key()` utilities. All routes are versioned under `/v1`; liveness and
readiness probes live at `/v1/health/live` and `/v1/health/ready`. The `/v1/lca`
endpoint accepts CSV uploads and returns a job identifier for asynchronous
processing, while websocket updates are available at `/v1/updates`.

## Deployment

Build and run the containerised environment:

```bash
bash scripts/deploy.sh
docker run -p 3838:3838 -p 8000:8000 health-data-science
```

Backup and restore the project data:

```bash
scripts/backup.sh data backup.tar.gz
scripts/restore.sh backup.tar.gz
```

Monitor the API in production with a simple health check:

```bash
scripts/monitor_api.sh
```

## Model validation
The `R/model_validation.R` module provides tools to assess model quality:

- Residual diagnostics and goodness-of-fit statistics for regression models.
- Bootstrap confidence intervals and k-fold cross validation utilities.
- Posterior probability summaries and class-quality metrics for latent class models.
- Latent class profile plotting and stability assessments across random seeds.

## Data Processing

The `data_processing` module provides production-ready data wrangling helpers:

- assess data quality with missingness and uniqueness summaries
- impute missing values (multiple imputation when the `mice` package is available)
- detect and cap outliers using the IQR rule
- engineer interaction and polynomial features
- read from CSV, Excel, or SQLite sources and export to common formats
- run transformation pipelines with validation checkpoints and record data lineage

## Visualization and Reporting
- `R/visualization.R` offers a publication-ready theme, interactive Plotly helpers, forest plots, and network diagrams for exploring variable relationships.
- `R/reporting.R` with templates in `templates/` produces parameterized reports, formatted tables, executive summaries, and model comparison documents.


## Configuration

Default parameters for the analyses live in `config/default.yaml`. Environment-
specific overrides (`development.yaml`, `production.yaml`, `testing.yaml`) can
adjust settings such as the number of latent classes or regression family.
Use the `--config` flag on command-line scripts to select the environment.
Configuration files are validated for type and range correctness via
`load_config()` before execution.

## License
This project is licensed under the [MIT License](LICENSE).

## Contact
Diogo Ribeiro
ESMAD - Instituto Politécnico do Porto
diogo.debastos.ribeiro@gmail.com | dfr@esmad.ipp.pt
ORCID: https://orcid.org/0009-0001-2022-7072
