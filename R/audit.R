# ------------------------------------------------------------------------------
# File: audit.R
# Purpose: Audit trail and lineage tracking utilities for compliance monitoring.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Write an audit log entry
#'
#' Records a user action with timestamp and optional contextual details. Logs
#' are appended to `path` and stored with restrictive permissions.
#'
#' @param action Character description of the action performed.
#' @param user Identifier of the actor.
#' @param details Optional named list with additional metadata.
#' @param path Log file path.
#' @return Invisibly returns `TRUE`.
#' @examples
#' audit_log("login", "alice")
#' @export
audit_log <- function(action, user, details = NULL, path = "audit.log") {
  entry <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    user = user,
    action = action,
    details = if (is.null(details)) "" else jsonlite::toJSON(details, auto_unbox = TRUE)
  )
  if (file.exists(path)) {
    utils::write.table(entry, path, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
  } else {
    utils::write.csv(entry, path, row.names = FALSE)
    Sys.chmod(path, mode = "600")
  }
  invisible(TRUE)
}

#' Track data lineage
#'
#' Appends a lineage record capturing source, transformation, and output with a
#' timestamp to facilitate downstream auditing.
#'
#' @param source Description of the data source.
#' @param transformation Transformation applied.
#' @param output Description of resulting dataset or file.
#' @param path File to which lineage is appended.
#' @return Invisibly returns `TRUE`.
#' @examples
#' track_lineage("raw.csv", "filter NA", "clean.csv")
#' @export
track_lineage <- function(source, transformation, output, path = "lineage.log") {
  entry <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    source = source,
    transformation = transformation,
    output = output
  )
  if (file.exists(path)) {
    utils::write.table(entry, path, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
  } else {
    utils::write.csv(entry, path, row.names = FALSE)
    Sys.chmod(path, mode = "600")
  }
  invisible(TRUE)
}

