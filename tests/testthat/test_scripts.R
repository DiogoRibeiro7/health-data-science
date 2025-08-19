run <- function(...) {
  out <- system2("Rscript", c(...), stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  expect_true(is.null(status) || status == 0)
}

test_that("analysis scripts execute", {
  pkgs <- c("optparse", "poLCA", "ggplot2", "aod", "survival", "survminer", "ranger", "ggfortify")
  for (p in pkgs) skip_if_not_installed(p)
  run(file.path("scripts", "generate_sample_data.R"))
  run(file.path("scripts", "ncdb_LCA.R"), "--input", "data/puf_early.csv", "--output", "data/lca_earlypuf.RData")
  expect_true(file.exists(file.path("data", "lca_earlypuf.RData")))
  load(file.path("data", "lca_earlypuf.RData"))
  expect_true(exists("lc7"))
  expect_s3_class(lca.pufdata, "data.frame")
  expect_true("class7" %in% names(lca.pufdata))
  run(file.path("scripts", "ncdbearly_Regression.R"), "--input", "data/lca_earlypuf.RData")
})
