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


test_that("monitoring records gauges and histograms", {
  m <- init_monitoring()

  record_metric(m$request_latency, 0.25, labels = list(endpoint = "/x"))
  monitor_resources(m)

  expect_true(inherits(m$request_latency, "Histogram"))
  expect_true(inherits(m$cpu_usage, "Gauge"))

  rendered <- .hds_render_metrics(m$registry)
  expect_true(grepl("http_request_latency_seconds_count", rendered, fixed = TRUE))
  expect_true(grepl("process_cpu_seconds_total", rendered, fixed = TRUE))
})

test_that("record_metric rejects unsupported objects", {
  expect_error(record_metric(list()), "Unsupported metric object")
})
