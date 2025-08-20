if (requireNamespace("renv", quietly = TRUE)) {
  tryCatch(
    renv::restore(prompt = FALSE),
    error = function(e) message("renv restore failed: ", e$message)
  )
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  message("testthat not installed, skipping tests")
  quit(save = "no")
}

library(testthat)
testthat::test_dir("tests/testthat")
