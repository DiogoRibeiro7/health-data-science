# NCDB_LCA

This repository provides supporting code needed to reproduce analysis from "Cluster identification of early-stage pancreas cancer patients at greatest risk for disparities of care"
Authors: Ugwuji N. Maduekwe, MD MMSc MPH; Briana J. K. Stephenson, MPH, PhD; Jen Jen Yeh, MD PhD; Melissa Troester, PhD; Hanna K Sanoff, MD.

## R Codes to reproduce analysis
1. NCDB_LCA.R - using early PUF data received from NCDB, latent class analysis model is performed and summarized
- Input: puf_early.sas7bdat (SAS data file containing early stage PUF data according to inclusion/exclusion criteria listed in manuscript)
- Output: lca_earlypuf.RData (lca.pufdata R Data frame containing original puf_early data and latent class assignment as a new variable)
2. NCDBearly_Regression.R - output from NCDB latent class models 
- Input: lca_earlypuf.RData
- Output: N/A - summary results detailed in R output





