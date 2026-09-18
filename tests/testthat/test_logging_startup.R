test_that("logging defaults are self-contained", {
  expect_equal(.hds_or(NULL, "fallback"), "fallback")
  expect_equal(.hds_or("value", "fallback"), "value")

  old_level <- Sys.getenv("LOG_LEVEL", unset = NA_character_)
  on.exit({
    if (is.na(old_level)) {
      Sys.unsetenv("LOG_LEVEL")
    } else {
      Sys.setenv(LOG_LEVEL = old_level)
    }
  }, add = TRUE)

  Sys.setenv(LOG_LEVEL = "INFO")
  expect_true(init_logging())
  expect_true(nzchar(.hds_log_env$session_id))
})
