source(file.path("..", "..", "R", "config.R"))


test_that("validate_config catches bad nclass", {
  bad <- list(lca = list(nclass = 0, maxiter = 10, tol = 0.1, nrep = 1, auto_classes = FALSE),
              regression = list(family = "gaussian"),
              survival = list(method = "cox"))
  expect_error(validate_config(bad))
})
