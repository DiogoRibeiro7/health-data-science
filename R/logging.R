# ------------------------------------------------------------------------------
# File: logging.R
# Purpose: Structured logging and diagnostics helpers for the health data
#   science toolkit.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Initialize structured logging
#'
#' Configures the global logger threshold and output layout. The logger writes
#' human-readable messages to the console with timestamps and severity levels.
#' Supported levels include `DEBUG`, `INFO`, `WARN`, and `ERROR` in increasing
#' order of verbosity.
#'
#' @param level Character string indicating the minimum logging level to
#'   display. One of "DEBUG", "INFO", "WARN", or "ERROR". Levels are
#'   case-insensitive and unrecognised values default to "INFO".
#'
#' @return Invisible `TRUE` used for its side effect of configuring the global
#'   logger.
#' @examples
#' # Show all log messages for debugging
#' init_logging("DEBUG")
#'
#' # Suppress debug output in production workflows
#' init_logging("INFO")
#' @export
init_logging <- function(level = "INFO") {
  logger::log_appender(logger::appender_console)
  logger::log_layout(logger::layout_glue_generator(
    format = '[{format(Sys.time(), "%Y-%m-%d %H:%M:%S")}] [{level}] {msg}'
  ))
  logger::log_threshold(level)
  invisible(TRUE)
}

#' Collect diagnostic information
#'
#' Gathers lightweight session metadata useful for debugging error reports
#' without exposing sensitive internal details. The returned list includes the
#' output of `utils::sessionInfo()` which captures R version, attached packages
#' and platform information.
#'
#' @return A named list containing session information that can be safely
#'   serialized for troubleshooting.
#' @examples
#' diagnostics <- collect_diagnostics()
#' str(diagnostics)
#' @export
collect_diagnostics <- function() {
  list(session = utils::sessionInfo())
}

