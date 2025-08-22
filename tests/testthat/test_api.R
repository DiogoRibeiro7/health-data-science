source(file.path("..", "..", "R", "lca.R"))
source(file.path("..", "..", "R", "config.R"))
source(file.path("..", "..", "R", "security.R"))

skip_if_not_installed("yaml")
skip_if_not_installed("plumber")

# provide read_config used in plumber script
read_config <- function(path) yaml::read_yaml(path)

test_that("versioned health endpoint works", {
  pr <- plumber::plumb("api/plumber.R")
  live <- Filter(function(r) r$path == "/v1/health/live", pr$routes)[[1]]
  res <- live$handler()
  expect_equal(res$status, "ok")
})

test_that("auth filter enforces API key and token", {
  pr <- plumber::plumb("api/plumber.R")
  filt <- pr$filters$auth
  req <- list(REQUEST_METHOD = "GET", PATH_INFO = "/v1/health/live", HTTP_X_API_KEY = "bad")
  res <- list(status = 200)
  out <- filt(req, res)
  expect_equal(out$status, 401)
  req2 <- list(REQUEST_METHOD = "GET", PATH_INFO = "/v1/health/live", HTTP_X_API_KEY = "demo-key")
  res2 <- list(status = 200)
  expect_null(filt(req2, res2))
})

test_that("job submission returns id", {
  pr <- plumber::plumb("api/plumber.R")
  route <- Filter(function(r) r$path == "/v1/lca" && r$verb == "POST", pr$routes)[[1]]
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(head(mtcars, 2), tmp, row.names = FALSE)
  res <- list(status = 200)
  out <- route$handler(list(datapath = tmp), res)
  expect_true(!is.null(out$job_id))
})
