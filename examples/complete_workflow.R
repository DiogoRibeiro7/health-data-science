# Full workflow using bundled sample data
source("../R/utils.R")
source("../R/config.R")
source("../R/lca.R")
source("../R/regression.R")

cfg <- load_config(root = "..")

# Run LCA and save results
run_lca(
  data_path = "../data/puf_early.csv",
  output_path = "../data/lca_earlypuf.RData",
  config = cfg,
  verbose = TRUE,
  show_progress = TRUE
)

# Run regression models using the saved LCA output
run_regression(
  "../data/lca_earlypuf.RData",
  config = cfg,
  verbose = TRUE,
  show_progress = TRUE
)
