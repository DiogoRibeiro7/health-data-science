#!/usr/bin/env Rscript
pr <- plumber::plumb("api/plumber.R")
pr$run(host = "0.0.0.0", port = 8000)
