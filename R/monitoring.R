# ------------------------------------------------------------------------------
# File: monitoring.R
# Purpose: Monitoring, tracing, and health check utilities for the health data
#   science toolkit.
# ------------------------------------------------------------------------------

.hds_metric_registry <- function() {
  registry <- new.env(parent = emptyenv())
  registry$metrics <- list()
  class(registry) <- "Registry"
  registry
}

.hds_metric_key <- function(labels, label_names) {
  if (length(label_names) == 0L) return("")
  values <- labels[label_names]
  if (length(values) != length(label_names) || any(vapply(values, is.null, logical(1)))) {
    stop(
      paste("Metric labels required:", paste(label_names, collapse = ", ")),
      call. = FALSE
    )
  }
  paste(
    paste0(label_names, "=", vapply(values, as.character, character(1))),
    collapse = ","
  )
}

.hds_metric <- function(type, name, help, labels = character(),
                        registry = NULL, buckets = NULL) {
  metric <- new.env(parent = emptyenv())
  metric$type <- type
  metric$name <- name
  metric$help <- help
  metric$label_names <- labels
  metric$buckets <- buckets
  metric$samples <- list()

  metric$inc <- function(value = 1, labels = list()) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0) {
      stop("Counter increments must be one non-negative finite number", call. = FALSE)
    }
    key <- .hds_metric_key(labels, metric$label_names)
    current <- metric$samples[[key]]
    if (is.null(current)) current <- 0
    metric$samples[[key]] <- current + value
    invisible(NULL)
  }

  metric$set <- function(value, labels = list()) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      stop("Gauge values must be one finite number", call. = FALSE)
    }
    key <- .hds_metric_key(labels, metric$label_names)
    metric$samples[[key]] <- value
    invisible(NULL)
  }

  metric$observe <- function(value, labels = list()) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      stop("Histogram observations must be one finite number", call. = FALSE)
    }
    key <- .hds_metric_key(labels, metric$label_names)
    values <- metric$samples[[key]]
    metric$samples[[key]] <- c(values, value)
    invisible(NULL)
  }

  metric$get <- function() {
    values <- if (length(metric$samples) == 0L) numeric() else {
      if (identical(metric$type, "Histogram")) {
        vapply(metric$samples, function(x) sum(x), numeric(1))
      } else {
        unlist(metric$samples, use.names = FALSE)
      }
    }
    list(samples = data.frame(value = values))
  }

  class(metric) <- type
  if (!is.null(registry)) {
    registry$metrics[[name]] <- metric
  }
  metric
}

.hds_prometheus_labels <- function(key, extra = NULL) {
  labels <- character()

  if (nzchar(key)) {
    pairs <- strsplit(key, ",", fixed = TRUE)[[1L]]
    parsed <- strsplit(pairs, "=", fixed = TRUE)
    labels <- vapply(
      parsed,
      function(x) sprintf('%s="%s"', x[[1L]], x[[2L]]),
      character(1)
    )
  }

  if (!is.null(extra)) {
    labels <- c(labels, extra)
  }

  if (length(labels) == 0L) return("")
  paste0("{", paste(labels, collapse = ","), "}")
}

.hds_render_metrics <- function(registry) {
  if (!inherits(registry, "Registry")) {
    stop("Invalid metrics registry", call. = FALSE)
  }

  lines <- character()

  for (metric in registry$metrics) {
    lines <- c(
      lines,
      paste0("# HELP ", metric$name, " ", metric$help),
      paste0("# TYPE ", metric$name, " ", tolower(metric$type))
    )

    if (length(metric$samples) == 0L) {
      next
    }

    for (key in names(metric$samples)) {
      labels <- .hds_prometheus_labels(key)

      if (identical(metric$type, "Histogram")) {
        values <- metric$samples[[key]]
        for (bucket in metric$buckets) {
          count <- sum(values <= bucket)
          bucket_labels <- .hds_prometheus_labels(
            key,
            extra = paste0('le="', bucket, '"')
          )
          lines <- c(
            lines,
            paste0(metric$name, "_bucket", bucket_labels, " ", count)
          )
        }
        lines <- c(
          lines,
          paste0(metric$name, "_count", labels, " ", length(values)),
          paste0(metric$name, "_sum", labels, " ", sum(values))
        )
      } else {
        lines <- c(
          lines,
          paste0(metric$name, labels, " ", metric$samples[[key]])
        )
      }
    }
  }

  paste(lines, collapse = "\n")
}

#' Initialize monitoring metrics
#'
#' Sets up default Prometheus counters, gauges, and histograms for tracking
#' request throughput, latency, errors, and resource utilisation. Returns a list
#' containing the registry and metric objects for further use.
#'
#' @param registry Optional existing registry returned by `init_monitoring()`.
#'
#' @return A list with the metrics registry and created metric objects.
#' @examples
#' \dontrun{
#' metrics <- init_monitoring()
#' }
#' @export
init_monitoring <- function(registry = NULL) {
  if (is.null(registry)) registry <- .hds_metric_registry()
  if (!inherits(registry, "Registry")) {
    stop("`registry` must be a Registry object", call. = FALSE)
  }

  http_requests <- .hds_metric(
    type = "Counter",
    name = "http_requests_total",
    help = "Total HTTP requests",
    labels = c("method", "endpoint"),
    registry = registry
  )

  request_latency <- .hds_metric(
    type = "Histogram",
    name = "http_request_latency_seconds",
    help = "Request latency",
    buckets = c(0.1, 0.3, 1, 3, 5),
    labels = c("endpoint"),
    registry = registry
  )

  errors_total <- .hds_metric(
    type = "Counter",
    name = "http_errors_total",
    help = "Total HTTP errors",
    labels = c("endpoint"),
    registry = registry
  )

  cpu_usage <- .hds_metric(
    type = "Gauge",
    name = "process_cpu_seconds_total",
    help = "Total user and system CPU time consumed",
    registry = registry
  )

  memory_usage <- .hds_metric(
    type = "Gauge",
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
#' Convenience wrapper to increment counters or record gauge/histogram values.
#'
#' @param metric A `Counter`, `Gauge`, or `Histogram` returned by `init_monitoring()`.
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
  } else if (inherits(metric, "Gauge")) {
    metric$set(value, labels = labels)
  } else if (inherits(metric, "Histogram")) {
    metric$observe(value, labels = labels)
  } else {
    stop("Unsupported metric object", call. = FALSE)
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
#' Converts selected monitoring metrics into a simple data frame for use in
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

