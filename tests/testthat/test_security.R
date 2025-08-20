source(file.path("..", "..", "R", "security.R"))

library(testthat)

test_that("encryption round trip works", {
  key <- generate_key()
  tmp <- tempfile()
  save_encrypted_rds(mtcars, tmp, key)
  df <- read_encrypted_rds(tmp, key)
  expect_equal(nrow(df), nrow(mtcars))
})

test_that("API key verification and roles", {
  store <- tempfile()
  yaml::write_yaml(list(keys = list(list(user = "u", key = "k", role = "user"))), store)
  expect_true(verify_api_key("k", "user", store))
  expect_false(verify_api_key("k", "admin", store))
})

test_that("pseudonymisation alters identifiers", {
  df <- data.frame(id = 1:3)
  out <- pseudonymize_data(df, "id")
  expect_false(any(out$id == df$id))
})

test_that("input sanitisation removes special chars", {
  expect_equal(sanitize_input("DROP TABLE; --"), "DROP TABLE --")
})
