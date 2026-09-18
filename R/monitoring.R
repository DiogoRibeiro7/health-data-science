# ------------------------------------------------------------------------------
# File: monitoring.R
# Purpose: Monitoring, tracing, and health check utilities for the health data
#   science toolkit.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-04-??
# ------------------------------------------------------------------------------

#' Initialize monitoring metrics
#'
#' Sets up default Prometheus counters, gauges, and histograms for tracking
#' request throughput, latency, errors, and resource utilisation. Returns a list
#' containing the registry and metric objects for further use.
#'
#' @param registry Optional existing `prometheus::Registry` object.
#'
#' @return A list with the metrics registry and created metric objects.
#' @examples
#' \dontrun{
#' metrics <- init_monitoring()
#' }
#' @export
init_monitoring <- function(registry = NULL) {
  if (!requireNamespace("prometheus", quietly = TRUE)) {
    stop("prometheus package required for monitoring")
  }
  if (is.null(registry)) registry <- prometheus::Registry$new()

  http_requests <- prometheus::Counter$new(
    name = "http_requests_total",
    help = "Total HTTP requests",
    labels = c("method", "endpoint"),
    registry = registry
  )

  request_latency <- prometheus::Histogram$new(
    name = "http_request_latency_seconds",
    help = "Request latency",
    buckets = c(0.1, 0.3, 1, 3, 5),
    labels = c("endpoint"),
    registry = registry
  )

  errors_total <- prometheus::Counter$new(
    name = "http_errors_total",
    help = "Total HTTP errors",
    labels = c("endpoint"),
    registry = registry
  )

  cpu_usage <- prometheus::Gauge$new(
    name = "process_cpu_seconds_total",
    help = "Total user and system CPU time consumed",
    registry = registry
  )

  memory_usage <- prometheus::Gauge$new(
    name = "process_memory_bytes",
    help = "Approximate memory usage in bytes",
    registry = registry
  )

  list(
    registry = registry,
    http_requests = http_requests,
    request_latency = request_latency,
    errors_total = errors_total,
    cpu_usage = cpu_usage,
    memory_usage = memory_usage
  )
}

#' Record a custom metric value
#'
#' Convenience wrapper to increment counters or observe values for histograms and
#' gauges. Accepts a Prometheus metric object and updates it accordingly.
#'
#' @param metric A Prometheus metric object such as a `Counter`, `Gauge`, or
#'   `Histogram`.
#' @param value Numeric value to record. For counters this increments the count;
#'   for gauges and histograms it records the observation.
#' @param labels Optional named list of labels to attach to the metric
#'   observation.
#'
#' @return Invisible `NULL` used for its side effect.
#' @examples
#' \dontrun{
#' m <- init_monitoring()
#' record_metric(m$http_requests, labels = list(method = "GET", endpoint = "/"))
#' }
#' @export
record_metric <- function(metric, value = 1, labels = list()) {
  if (inherits(metric, "Counter")) {
    metric$inc(value, labels = labels)
  } else if (inherits(metric, c("Gauge", "Histogram"))) {
    metric$observe(value, labels = labels)
  }
  invisible(NULL)
}

#' Update resource usage gauges
#'
#' Captures coarse CPU and memory usage statistics and updates the corresponding
#' gauge metrics created by `init_monitoring()`.
#'
#' @param metrics Metric list returned by `init_monitoring()`.
#'
#' @return Invisible `NULL`.
#' @export
monitor_resources <- function(metrics) {
  if (is.null(metrics)) return(invisible(NULL))
  cpu <- sum(proc.time()[1:2])
  mem <- sum(gc()[, 2]) * 1024 * 1024  # convert from MB to bytes
  metrics$cpu_usage$set(cpu)
  metrics$memory_usage$set(mem)
  invisible(NULL)
}

#' Start a trace span
#'
#' Utilises the `otel` package when available to start a new span for
#' distributed tracing. When the package is absent, returns `NULL` allowing the
#' caller to proceed without tracing.
#'
#' @param name Descriptive span name.
#'
#' @return Span object or `NULL` if tracing is unavailable.
#' @export
start_trace <- function(name) {
  if (requireNamespace("otel", quietly = TRUE)) {
    return(otel::start_span(name = name))
  }
  NULL
}

#' End a trace span
#'
#' Finalises an active span created by `start_trace()`. If an error message is
#' provided it is recorded on the span before closing.
#'
#' @param span Span object returned by `start_trace()`.
#' @param error Optional error message character string.
#'
#' @return Invisible `NULL`.
#' @export
end_trace <- function(span, error = NULL) {
  if (!is.null(span) && requireNamespace("otel", quietly = TRUE)) {
    if (!is.null(error) && is.function(span$set_status)) {
      span$set_status("error", description = error)
    }
    otel::end_span(span)
  }
  invisible(NULL)
}

#' Perform application health checks
#'
#' Aggregates basic health information including database connectivity and
#' custom dependency checks. Dependency functions should return `TRUE` when the
#' component is healthy.
#'
#' @param db Optional DBI connection object or pool.
#' @param dependencies Named list of functions returning logical values.
#'
#' @return List detailing overall status, database connectivity, and dependency
#'   results.
#' @export
health_check <- function(db = NULL, dependencies = list()) {
  status <- TRUE
  db_ok <- TRUE
  if (!is.null(db)) {
    db_ok <- tryCatch(DBI::dbIsValid(db), error = function(e) FALSE)
    status <- status && db_ok
  }
  deps <- lapply(dependencies, function(f) {
    ok <- tryCatch(isTRUE(f()), error = function(e) FALSE)
    status <<- status && ok
    ok
  })
  list(status = if (status) "ok" else "fail", database = db_ok, dependencies = deps)
}

#' Send alert notifications
#'
#' Provides a simple interface for emitting alert messages via supported
#' channels. Slack webhooks are supported when an appropriate webhook URL is
#' supplied. Email alerts are logged as warnings by default and can be extended
#' using specialised mailing packages.
#'
#' @param channel Alert channel, one of "email" or "slack".
#' @param message Message text to send.
#' @param config Optional list containing channel-specific configuration such as
#'   a Slack `webhook` URL.
#'
#' @return Invisible `TRUE` indicating the alert was processed.
#' @export
send_alert <- function(channel, message, config = list()) {
  if (channel == "slack" && requireNamespace("httr", quietly = TRUE)) {
    if (is.null(config$webhook)) stop("Slack webhook required")
    httr::POST(config$webhook, body = list(text = message), encode = "json")
  } else if (channel == "email") {
    logger::log_warn(sprintf("Email alert: %s", message))
  } else {
    logger::log_warn(sprintf("Unsupported alert channel: %s", channel))
  }
  invisible(TRUE)
}

#' Summarise metrics for dashboards
#'
#' Converts selected Prometheus metrics into a simple data frame for use in
#' dashboards or status pages. Only counter values are summarised.
#'
#' @param metrics Metric list returned by `init_monitoring()`.
#'
#' @return Data frame with metric names and their current values.
#' @export
dashboard_status <- function(metrics) {
  if (is.null(metrics)) return(data.frame())
  ms <- metrics[names(metrics) != "registry"]
  data.frame(
    metric = names(ms),
    value = vapply(ms, function(m) {
      if (inherits(m, "Counter")) {
        sum(m$get()$samples$value)
      } else NA_real_
    }, numeric(1)),
    row.names = NULL
  )
}

