context("monitoring utilities")

test_that("health_check returns ok by default", {
  res <- health_check()
  expect_equal(res$status, "ok")
  expect_true(res$database)
})

test_that("init_monitoring creates metrics", {
  m <- init_monitoring()
  expect_true(inherits(m$registry, "Registry"))
  record_metric(m$http_requests, labels = list(method = "GET", endpoint = "/"))
  out <- .hds_render_metrics(m$registry)
  expect_true(grepl("http_requests_total", out))
})
