# Example latent class analysis using bundled sample data
source("../R/lca.R")

sample_data <- read.csv("../data/puf_early.csv")
fit <- run_lca(sample_data, nclass = 2)
print(fit)
