# Health Data Science

This repository provides a health data science toolkit with reproducible workflows.
It currently includes scripts for reproducing analyses from the study
"Cluster identification of early-stage pancreas cancer patients at greatest risk for disparities of care" by Ugwuji N. Maduekwe,
Briana J. K. Stephenson, Jen Jen Yeh, Melissa Troester and Hanna K. Sanoff.

## Repository structure
- `scripts/ncdb_LCA.R` – example latent class analysis on early-stage NCDB data, saving results to `data/lca_earlypuf.RData`.
- `scripts/ncdbearly_Regression.R` – example regression models using the latent class assignments.
- `R/utils.R` – helper functions for package installation, directory creation, and data validation.
- `data/` – contains example input data (`puf_early.csv`) and stores intermediate outputs. See `data/README.md`.

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

## Usage
1. Ensure `data/puf_early.csv` is present. A small synthetic sample is provided and can be regenerated with `scripts/generate_sample_data.R`. Replace it with the official NCDB PUF file for real analyses.
2. Run the latent class analysis (override defaults with `--input` and `--output` if desired):
   ```bash
   Rscript scripts/ncdb_LCA.R --input data/puf_early.csv --output data/lca_earlypuf.RData
   ```
   Use `--dry-run` to validate arguments without executing, `--quiet` to suppress
   progress output, or `--version` to print the script version.
3. Run the regression models (accepts `--input` to specify the LCA results):
   ```bash
   Rscript scripts/ncdbearly_Regression.R --input data/lca_earlypuf.RData
   ```
   These scripts determine the project root automatically, so the default
   paths work even when invoked from outside the repository using absolute
   script locations. Progress bars and verbose messages provide feedback during
   lengthy operations.
The scripts attempt to install any missing R packages automatically.
Package installation is verified, the latent class analysis uses a fixed
random seed for reproducible results, and output directories are created
automatically when needed.

## License
This project is licensed under the [MIT License](LICENSE).

## Contact
Diogo Ribeiro
ESMAD - Instituto Politécnico do Porto
diogo.debastos.ribeiro@gmail.com | dfr@esmad.ipp.pt
ORCID: https://orcid.org/0009-0001-2022-7072
