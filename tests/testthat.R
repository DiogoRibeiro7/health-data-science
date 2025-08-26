if (requireNamespace("renv", quietly = TRUE)) {
  tryCatch(
    renv::restore(prompt = FALSE),
    error = function(e) healthdatascience::log_error(paste("renv restore failed:", e$message), component = "tests")
  )
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  healthdatascience::log_warn("testthat not installed, skipping tests", component = "tests")
  quit(save = "no")
}

library(testthat)
testthat::test_dir("tests/testthat")
