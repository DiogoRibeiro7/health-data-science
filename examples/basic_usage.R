# Example latent class analysis using bundled sample data
source("../R/utils.R")
source("../R/config.R")
source("../R/lca.R")

cfg <- load_config(root = "..")
init_logging("INFO")
run_lca("../data/puf_early.csv", "../data/lca_earlypuf.RData", cfg, dry_run = TRUE,
        verbose = TRUE, show_progress = FALSE)
