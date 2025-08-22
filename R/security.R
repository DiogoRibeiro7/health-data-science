# ------------------------------------------------------------------------------
# File: security.R
# Purpose: Security utilities for encryption, authentication, and privacy
#   controls in regulated healthcare environments.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Generate a random 256-bit encryption key
#'
#' @return Raw vector of length 32 suitable for AES-256 operations.
#' @examples
#' key <- generate_key()
#' @export
generate_key <- function() {
  openssl::rand_bytes(32)
}

#' Encrypt an R object
#'
#' Serializes and encrypts an object using AES-CBC with the provided key. The
#' initialization vector is prepended to the ciphertext to allow later
#' decryption.
#'
#' @param object R object to serialize and encrypt.
#' @param key Raw vector produced by [generate_key()].
#' @return Raw vector containing the IV and ciphertext.
#' @examples
#' key <- generate_key()
#' data <- encrypt_data(list(x = 1), key)
#' @export
encrypt_data <- function(object, key) {
  raw <- serialize(object, NULL)
  iv <- openssl::rand_bytes(16)
  cipher <- openssl::aes_cbc_encrypt(raw, key = key, iv = iv)
  c(iv, cipher)
}

#' Decrypt a previously encrypted object
#'
#' @param data Raw vector returned by [encrypt_data()].
#' @param key Encryption key used during encryption.
#' @return The original R object.
#' @examples
#' key <- generate_key()
#' cipher <- encrypt_data(list(x = 1), key)
#' decrypt_data(cipher, key)
#' @export
decrypt_data <- function(data, key) {
  iv <- data[1:16]
  cipher <- data[-(1:16)]
  raw <- openssl::aes_cbc_decrypt(cipher, key = key, iv = iv)
  unserialize(raw)
}

#' Save an object to an encrypted RDS file
#'
#' @param object R object to save.
#' @param path File path for the encrypted output.
#' @param key Raw encryption key.
#' @return The input `path` invisibly.
#' @examples
#' key <- generate_key()
#' save_encrypted_rds(mtcars, tempfile(), key)
#' @export
save_encrypted_rds <- function(object, path, key) {
  bin <- encrypt_data(object, key)
  writeBin(bin, path)
  Sys.chmod(path, mode = "600")
  invisible(path)
}

#' Read an object from an encrypted RDS file
#'
#' @param path File path of the encrypted RDS.
#' @param key Raw encryption key.
#' @return The decrypted R object.
#' @examples
#' key <- generate_key()
#' tmp <- tempfile()
#' save_encrypted_rds(mtcars, tmp, key)
#' df <- read_encrypted_rds(tmp, key)
#' @export
read_encrypted_rds <- function(path, key) {
  bin <- readBin(path, "raw", n = file.info(path)$size)
  decrypt_data(bin, key)
}

#' Create a secure temporary file
#'
#' Generates a temporary file with restrictive permissions (read/write for the
#' owner only) to avoid leaking sensitive data.
#'
#' @param pattern Filename prefix.
#' @return Path to the temporary file.
#' @export
secure_tempfile <- function(pattern = "tmp") {
  tf <- tempfile(pattern = pattern)
  Sys.chmod(tf, mode = "600")
  tf
}

#' Sanitize user input
#'
#' Removes non-alphanumeric characters to mitigate injection attacks.
#'
#' @param x Character vector to sanitize.
#' @return Sanitized character vector.
#' @examples
#' sanitize_input("DROP TABLE; --")
#' @export
sanitize_input <- function(x) {
  gsub("[^[:alnum:] _-]", "", x)
}

#' Verify an API key and optional role
#'
#' Checks the provided `key` against a YAML key store. The YAML file should
#' contain a `keys` field with entries of `user`, `key`, and `role`.
#'
#' @param key API key supplied by the client.
#' @param required_role Minimum role required for access.
#' @param store Path to YAML key store.
#' @return Logical indicating whether the key (and role) is valid.
#' @examples
#' store <- tempfile()
#' yaml::write_yaml(list(keys = list(list(user = "alice", key = "123", role = "admin"))), store)
#' verify_api_key("123", "admin", store)
#' @export
verify_api_key <- function(key, required_role = NULL, store = "config/api_keys.yaml") {
  if (!file.exists(store)) return(FALSE)
  dat <- yaml::read_yaml(store)$keys
  idx <- which(vapply(dat, function(x) x$key == key, logical(1)))
  if (length(idx) == 0) return(FALSE)
  role <- dat[[idx]]$role
  if (is.null(required_role)) return(TRUE)
  roles <- c("user", "admin")
  match(role, roles) >= match(required_role, roles)
}

#' Rotate an API key for a given user
#'
#' Generates and stores a new random key for `user` inside the YAML key store.
#'
#' @param user Username whose key should be rotated.
#' @param store Path to YAML key store.
#' @return The newly generated API key.
#' @export
rotate_api_key <- function(user, store = "config/api_keys.yaml") {
  dat <- if (file.exists(store)) yaml::read_yaml(store) else list(keys = list())
  idx <- which(vapply(dat$keys, function(x) x$user == user, logical(1)))
  new_key <- uuid::UUIDgenerate()
  entry <- list(user = user, key = new_key, role = "user")
  if (length(idx) == 0) {
    dat$keys[[length(dat$keys) + 1]] <- entry
  } else {
    dat$keys[[idx]] <- entry
  }
  yaml::write_yaml(dat, store)
  new_key
}

#' Check if a user role is authorised
#'
#' @param user_role Role attached to the user.
#' @param allowed_roles Character vector of permitted roles.
#' @return Logical.
#' @export
check_role <- function(user_role, allowed_roles) {
  user_role %in% allowed_roles
}

#' Pseudonymise identifying columns
#'
#' Hashes specified columns using SHA-256 to remove direct identifiers while
#' retaining linkability for research purposes.
#'
#' @param data Data frame containing identifiers.
#' @param cols Character vector of column names to hash.
#' @return Data frame with hashed columns.
#' @export
pseudonymize_data <- function(data, cols) {
  data[cols] <- lapply(data[cols], digest::digest, algo = "sha256")
  data
}

#' Add Laplace noise for differential privacy
#'
#' Adds symmetric Laplace noise to numeric vectors, satisfying epsilon-differential
#' privacy for release of aggregated metrics.
#'
#' @param x Numeric vector.
#' @param epsilon Privacy budget; smaller values add more noise.
#' @return Numeric vector with noise added.
#' @export
add_dp_noise <- function(x, epsilon = 1) {
  u <- stats::runif(length(x)) - 0.5
  noise <- -sign(u) * log(1 - 2 * abs(u)) / epsilon
  x + noise
}

#' Enforce data retention policy
#'
#' Deletes files within `dir` older than `days` to satisfy data minimisation
#' requirements.
#'
#' @param dir Directory containing files to evaluate.
#' @param days Files older than this many days are removed.
#' @return Invisibly returns `TRUE`.
#' @export
enforce_retention_policy <- function(dir, days = 30) {
  files <- list.files(dir, full.names = TRUE)
  now <- Sys.time()
  for (f in files) {
    age <- as.numeric(difftime(now, file.info(f)$mtime, units = "days"))
    if (!is.na(age) && age > days) try(unlink(f))
  }
  invisible(TRUE)
}

#' Record user consent
#'
#' Appends a consent record to a CSV log with timestamp.
#'
#' @param user_id Identifier for the data subject.
#' @param consent Logical indicating whether consent was granted.
#' @param path Log file path.
#' @return Invisibly returns `TRUE`.
#' @export
record_consent <- function(user_id, consent, path = "consent_log.csv") {
  entry <- data.frame(user_id = user_id, consent = consent, timestamp = Sys.time())
  if (file.exists(path)) {
    utils::write.table(entry, path, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
  } else {
    utils::write.csv(entry, path, row.names = FALSE)
    Sys.chmod(path, mode = "600")
  }
  invisible(TRUE)
}

#' Run an API with TLS encryption
#'
#' Convenience wrapper around `plumber::pr_run` that supplies certificate and
#' key files to enable HTTPS communication.
#'
#' @param pr Plumber router object.
#' @param cert Path to PEM formatted certificate.
#' @param key Path to PEM formatted private key.
#' @param port Port to bind.
#' @param host Interface to bind.
#' @return Invisibly returns the result of `plumber::pr_run`.
#' @export
run_secure_api <- function(pr, cert, key, port = 8000, host = "0.0.0.0") {
  plumber::pr_run(pr, host = host, port = port, certfile = cert, keyfile = key)
}

#' Validate an OAuth2/OpenID Connect token
#'
#' Performs basic JWT validation for access control. The token's payload is
#' decoded without signature verification and the `exp` claim is honoured. A
#' `role` claim may optionally be supplied to enforce role-based access.
#'
#' @param token Bearer token provided by the client.
#' @param required_role Minimum role required for the request.
#' @return Logical indicating whether the token is valid and authorised.
#' @export
validate_oidc_token <- function(token, required_role = "user") {
  tryCatch({
    parts <- strsplit(token, "\\.", fixed = TRUE)[[1]]
    if (length(parts) < 2) return(FALSE)
    payload <- jsonlite::fromJSON(rawToChar(openssl::base64_decode(parts[2])))
    if (!is.null(payload$exp) && Sys.time() > as.POSIXct(payload$exp, origin = "1970-01-01")) return(FALSE)
    role <- if (!is.null(payload$role)) payload$role else "user"
    roles <- c("user", "admin")
    match(role, roles) >= match(required_role, roles)
  }, error = function(e) FALSE)
}

#' Simple in-memory rate limiter
#'
#' Tracks request counts per key within a sliding time window. Exceeding the
#' limit results in a `FALSE` return value that callers can use to throttle
#' traffic.
#'
#' @param key Identifier for the client (e.g. API key or user id).
#' @param limit Maximum number of requests allowed within `window` seconds.
#' @param window Time window in seconds for the rate limit.
#' @return Logical indicating whether the request is permitted.
#' @export
check_rate_limit <- local({
  bucket <- list()
  function(key, limit = 100, window = 60) {
    now <- Sys.time()
    rec <- bucket[[key]]
    if (is.null(rec) || difftime(now, rec$start, units = "secs") > window) {
      bucket[[key]] <<- list(start = now, n = 1)
      return(TRUE)
    }
    if (rec$n >= limit) return(FALSE)
    rec$n <- rec$n + 1
    bucket[[key]] <<- rec
    TRUE
  }
})
