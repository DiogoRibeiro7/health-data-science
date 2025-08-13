# NCDB_LCA

This repository contains scripts for reproducing analyses from the study
"Cluster identification of early-stage pancreas cancer patients at greatest risk for disparities of care" by Ugwuji N. Maduekwe,
Briana J. K. Stephenson, Jen Jen Yeh, Melissa Troester and Hanna K. Sanoff.

## Repository structure
- `scripts/ncdb_LCA.R` – performs latent class analysis on early-stage NCDB data and saves results to `data/lca_earlypuf.RData`.
- `scripts/ncdbearly_Regression.R` – runs regression models using the latent class assignments.
- `scripts/utils.R` – helper functions for package installation, directory creation, and data validation.
- `data/` – place input files such as `puf_early.sas7bdat`; intermediate outputs are also stored here.

## Prerequisites
Install the system libraries required to compile common R packages:

```bash
sudo apt-get update -y
sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev
```

This repository uses [renv](https://rstudio.github.io/renv/) to manage package versions. After cloning the project, restore the R library with:

```bash
R -q -e 'renv::restore()'
```

A helper script is available at `scripts/install_system_deps.sh` to install the system packages above.

## Usage
1. Add `puf_early.sas7bdat` to the `data/` directory.
2. Run the latent class analysis (override defaults with `--input` and `--output` if desired):
   ```bash
   Rscript scripts/ncdb_LCA.R --input data/puf_early.sas7bdat --output data/lca_earlypuf.RData
   ```
3. Run the regression models (accepts `--input` to specify the LCA results):
   ```bash
   Rscript scripts/ncdbearly_Regression.R --input data/lca_earlypuf.RData
   ```
   These scripts determine the project root automatically, so the default
   paths work even when invoked from outside the repository using absolute
   script locations.
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
