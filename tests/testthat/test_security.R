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
  hash <- hash_api_key("k")
  yaml::write_yaml(list(keys = list(list(user = "u", hash = hash, role = "user"))), store)
  expect_true(verify_api_key("k", "user", store))
  expect_false(verify_api_key("k", "admin", store))
})

test_that("JWT signature validation", {
  key <- openssl::rsa_keygen()
  token <- jose::jwt_encode_sig(list(sub = "u", role = "user"), key)
  pub <- openssl::pem_write(openssl::as.list(key)$pubkey)
  expect_true(validate_oidc_token(token, public_key = pub))
  tampered <- paste0(token, "x")
  expect_false(validate_oidc_token(tampered, public_key = pub))
})

test_that("pseudonymisation alters identifiers", {
  df <- data.frame(id = 1:3)
  out <- pseudonymize_data(df, "id")
  expect_false(any(out$id == df$id))
})

test_that("input sanitisation removes special chars", {
  expect_equal(sanitize_input("DROP TABLE; --"), "DROP TABLE --")
})
