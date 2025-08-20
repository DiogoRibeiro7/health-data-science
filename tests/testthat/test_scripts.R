run <- function(...) {
  out <- system2("Rscript", c(...), stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  expect_true(is.null(status) || status == 0)
}

test_that("scripts expose version and dry-run options", {
  out <- system2("Rscript", c(file.path("scripts", "ncdb_LCA.R"), "--version"),
                 stdout = TRUE, stderr = TRUE)
  expect_match(out, "ncdb_LCA.R version")
  run(file.path("scripts", "ncdb_LCA.R"), "--dry-run")

  out <- system2("Rscript", c(file.path("scripts", "ncdbearly_Regression.R"), "--version"),
                 stdout = TRUE, stderr = TRUE)
  expect_match(out, "ncdbearly_Regression.R version")
  run(file.path("scripts", "ncdbearly_Regression.R"), "--dry-run")
})

test_that("analysis scripts execute", {
  pkgs <- c("optparse", "poLCA", "ggplot2", "aod", "survival", "survminer", "ranger", "ggfortify")
  for (p in pkgs) skip_if_not_installed(p)
  run(file.path("scripts", "generate_sample_data.R"))
  run(file.path("scripts", "ncdb_LCA.R"), "--input", "data/puf_early.csv", "--output", "data/lca_earlypuf.RData", "--config", "testing")
  expect_true(file.exists(file.path("data", "lca_earlypuf.RData")))
  load(file.path("data", "lca_earlypuf.RData"))
  expect_true(exists("lc7"))
  expect_s3_class(lca.pufdata, "data.frame")
  expect_true("class7" %in% names(lca.pufdata))
  run(file.path("scripts", "ncdbearly_Regression.R"), "--input", "data/lca_earlypuf.RData", "--config", "testing")
})
