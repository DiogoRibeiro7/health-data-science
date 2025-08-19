# Full workflow using bundled sample data
source("../R/lca.R")
source("../R/regression.R")

# Run LCA and save results
run_lca(
  data_path = "../data/puf_early.csv",
  output_path = "../data/lca_earlypuf.RData",
  verbose = FALSE,
  show_progress = FALSE
)

# Run regression models using the saved LCA output
run_regression(
  "../data/lca_earlypuf.RData",
  verbose = FALSE,
  show_progress = FALSE
)
