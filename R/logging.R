# ------------------------------------------------------------------------------
# File: logging.R
# Purpose: Structured logging and diagnostics helpers for the health data
#   science toolkit.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

# Package-local environment for session state
.hds_log_env <- new.env(parent = emptyenv())

#' Initialize structured logging
#'
#' Sets up a JSON-based logging framework with configurable thresholds and
#' multiple output appenders. Configuration is read from `config/logging.yaml`
#' and can be overridden by the `LOG_LEVEL` environment variable. A unique
#' session identifier is generated on initialisation to correlate log entries.
#'
#' The configuration file supports the fields:
#' * `level` – minimum log level (DEBUG, INFO, WARN, ERROR)
#' * `file` – path to a log file (optional)
#' * `rotate_size` – maximum size in bytes before log rotation
#' * `max_backups` – number of rotated log files to retain
#' * `console` – logical; emit logs to the console
#' * `remote_url` – optional HTTP endpoint for remote log collection
#'
#' @param level Optional character string specifying the minimum logging level.
#'   If `NULL`, the value from configuration or the `LOG_LEVEL` environment
#'   variable is used.
#' @return Invisible `TRUE`.
#' @examples
#' init_logging()
#' log_info("Logging configured")
#' @export
init_logging <- function(level = NULL) {
  ensure_packages(c("jsonlite", "yaml", "logger"))

  cfg_path <- file.path("config", "logging.yaml")
  cfg <- if (file.exists(cfg_path)) yaml::read_yaml(cfg_path) else list()

  lvl <- toupper(level %||% Sys.getenv("LOG_LEVEL", cfg$level %||% "INFO"))

  log_file <- cfg$file
  rotate_size <- cfg$rotate_size %||% (5 * 1024 ^ 2) # 5 MB default
  max_backups <- cfg$max_backups %||% 5
  remote_url <- cfg$remote_url
  console <- isTRUE(cfg$console %||% TRUE)

  app <- list()
  if (!is.null(log_file)) {
    dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
    app <- c(app, list(rotating_appender(log_file, rotate_size, max_backups)))
  }
  if (console) {
    app <- c(app, list(logger::appender_console))
  }
  if (!is.null(remote_url)) {
    app <- c(app, list(http_appender(remote_url)))
  }
  logger::log_appender(appender_chain(app))
  logger::log_layout(layout_json)
  logger::log_threshold(lvl)

  .hds_log_env$session_id <- generate_session_id()
  invisible(TRUE)
}

# Internal helper: generate unique session identifier
generate_session_id <- function() {
  uuid::UUIDgenerate()
}

# Internal helper: chain multiple appenders
appender_chain <- function(appenders) {
  function(line) {
    for (a in appenders) a(line)
  }
}

# Internal appender with basic size-based rotation
rotating_appender <- function(file, max_bytes, backups) {
  function(line) {
    if (file.exists(file) && file.info(file)$size > max_bytes) {
      for (i in rev(seq_len(backups))) {
        src <- if (i == 1) file else sprintf("%s.%d", file, i - 1)
        dst <- sprintf("%s.%d", file, i)
        if (file.exists(src)) file.rename(src, dst)
      }
    }
    cat(line, file = file, append = TRUE)
  }
}

# Internal appender for remote HTTP logging
http_appender <- function(url) {
  function(line) {
    try(httr::POST(url, body = line, encode = "raw"), silent = TRUE)
  }
}

# Custom JSON layout
layout_json <- function(level, msg, namespace, .logcall, .topcall, .topenv, ...) {
  rec <- list(
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    level = level,
    component = namespace %||% "general",
    message = msg,
    session = .hds_log_env$session_id,
    caller = as.character(sys.call(-1)[[1]]),
    user = .hds_log_env$user_id %||% NA
  )
  jsonlite::toJSON(rec, auto_unbox = TRUE)
}

#' Adjust logging level at runtime
#'
#' @param level Character string of the new threshold.
#' @return Invisible `TRUE`.
#' @export
set_log_level <- function(level = "INFO") {
  logger::log_threshold(level)
  invisible(TRUE)
}

#' Track current user for log correlation
#'
#' Associates log messages with a user identifier for audit purposes.
#' @param user_id Character identifier of the active user.
#' @return Invisible `TRUE`.
#' @export
set_log_user <- function(user_id) {
  .hds_log_env$user_id <- user_id
  invisible(TRUE)
}

#' Generic logging wrapper
log_message <- function(level, msg, component = "general", context = list()) {
  logger::log_level(level, msg, namespace = component, context = context)
}

#' Log at DEBUG level
#' @param msg Message string
#' @param component Component name
#' @param context Named list of extra fields
#' @export
log_debug <- function(msg, component = "general", context = list()) {
  log_message("DEBUG", sanitize_message(msg), component, context)
}

#' Log at INFO level
#' @export
log_info <- function(msg, component = "general", context = list()) {
  log_message("INFO", sanitize_message(msg), component, context)
}

#' Log at WARN level
#' @export
log_warn <- function(msg, component = "general", context = list()) {
  log_message("WARN", sanitize_message(msg), component, context)
}

#' Log at ERROR level
#' @export
log_error <- function(msg, component = "general", context = list()) {
  log_message("ERROR", sanitize_message(msg), component, context)
}

#' Sanitize messages for HIPAA compliance
#'
#' Removes obvious identifiers (dates, long numeric strings) from log messages.
#' @param msg Message to sanitize.
#' @return Sanitized character string.
sanitize_message <- function(msg) {
  gsub("\\b(\\d{4}-\\d{2}-\\d{2}|\\d{9,})\\b", "[REDACTED]", msg)
}

#' Log access events for audit trails
#' @param user_id User initiating the action
#' @param resource Resource being accessed
#' @param action Performed action
#' @export
log_audit <- function(user_id, resource, action) {
  log_info(
    sprintf("audit event: %s %s", user_id, action),
    component = "audit",
    context = list(resource = resource, user = user_id)
  )
}

#' Log model performance metrics
#' @param model Name of the model
#' @param metrics Named list of metric values
#' @export
log_model_metrics <- function(model, metrics) {
  log_info(
    sprintf("model metrics recorded for %s", model),
    component = "model",
    context = metrics
  )
}

#' Execute expression with performance logging
#' @param label Descriptive label for the operation
#' @param expr Expression to evaluate
#' @param component Component name
#' @export
log_performance <- function(label, expr, component = "general") {
  start <- proc.time()[["elapsed"]]
  mem_before <- sum(gc()[, 2])
  result <- eval.parent(substitute(expr))
  elapsed <- proc.time()[["elapsed"]] - start
  mem_after <- sum(gc()[, 2])
  log_debug(
    sprintf("%s completed", label),
    component = component,
    context = list(duration = elapsed, mem_mb = mem_after - mem_before)
  )
  result
}

#' Collect diagnostic information
#'
#' Gathers lightweight session metadata useful for debugging error reports while
#' respecting privacy constraints.
#' @return Named list with session info.
#' @export
collect_diagnostics <- function() {
  list(session = utils::sessionInfo(), session_id = .hds_log_env$session_id)
}

# Initialise logger when package loads
.onLoad <- function(libname, pkgname) {
  init_logging()
}
