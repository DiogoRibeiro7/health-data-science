source(file.path("..", "..", "R", "lca.R"))
source(file.path("..", "..", "R", "config.R"))
source(file.path("..", "..", "R", "security.R"))

skip_if_not_installed("yaml")
skip_if_not_installed("plumber")

# provide read_config used in plumber script
read_config <- function(path) yaml::read_yaml(path)

test_that("/health/live endpoint works", {
  pr <- plumber::plumb("api/plumber.R")
  live <- Filter(function(r) r$path == "/health/live", pr$routes)[[1]]
  res <- live$handler()
  expect_equal(res$status, "ok")
})

test_that("auth filter enforces API key", {
  pr <- plumber::plumb("api/plumber.R")
  filt <- pr$filters$auth
  req <- list(REQUEST_METHOD = "GET", PATH_INFO = "/health/live", HTTP_X_API_KEY = "bad")
  res <- list(status = 200)
  out <- filt(req, res)
  expect_equal(out$status, 401)
})
