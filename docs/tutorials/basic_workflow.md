# Basic Workflow Tutorial

This tutorial demonstrates a complete run of the latent class analysis and regression models using the bundled synthetic data.

1. **Install dependencies**
   ```bash
   bash scripts/setup.sh
   ```
2. **Generate the sample dataset**
   ```bash
   Rscript scripts/generate_sample_data.R
   ```
3. **Run the latent class analysis**
   ```bash
   Rscript scripts/ncdb_LCA.R --input data/puf_early.csv --output data/lca_earlypuf.RData
   ```
4. **Run the regression models**
   ```bash
   Rscript scripts/ncdbearly_Regression.R --input data/lca_earlypuf.RData
   ```
5. **Inspect results**
   The regression script prints model summaries to the console and the LCA results are saved to `data/lca_earlypuf.RData`.
