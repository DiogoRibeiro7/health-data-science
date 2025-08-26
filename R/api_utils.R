# ------------------------------------------------------------------------------
# File: api_utils.R
# Purpose: Helper utilities for the Plumber API including asynchronous job
#          management, pagination helpers, webhook registration, and FHIR
#          translation stubs.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# ------------------------------------------------------------------------------

# Environment for tracking asynchronous jobs
jobs <- new.env(parent = emptyenv())

#' Submit a background job
#'
#' Runs `func` asynchronously using `future::future` and returns a job id that
#' can be queried later.
#'
#' @param func Function to execute.
#' @param ... Arguments passed to `func`.
#' @return A character job identifier.
#' @export
submit_job <- function(func, ...) {
  id <- uuid::UUIDgenerate()
  jobs[[id]] <<- list(future = future::future(func(...)), created = Sys.time())
  id
}

#' Retrieve background job status
#'
#' @param id Job identifier returned by [submit_job()].
#' @return List with status (`running`, `done`, or `error`) and result if
#'   available.
#' @export
get_job_status <- function(id) {
  job <- jobs[[id]]
  if (is.null(job)) return(list(status = "unknown"))
  if (!future::resolved(job$future)) return(list(status = "running"))
  res <- try(future::value(job$future), silent = TRUE)
  if (inherits(res, "try-error")) {
    list(status = "error", message = as.character(res))
  } else {
    list(status = "done", result = res)
  }
}

#' Paginate a data frame
#'
#' @param data Data frame to paginate.
#' @param page Page number (1-indexed).
#' @param per_page Number of records per page.
#' @return List containing `data` subset and pagination metadata.
#' @export
paginate <- function(data, page = 1, per_page = 50) {
  total <- nrow(data)
  start <- (page - 1) * per_page + 1
  end <- min(start + per_page - 1, total)
  subset <- if (start <= total) data[start:end, , drop = FALSE] else data[0, ]
  list(
    data = subset,
    page = page,
    per_page = per_page,
    total = total
  )
}

#' Register a webhook URL
#'
#' Stores a callback URL that will be invoked when [emit_event()] is called.
#'
#' @param url Webhook endpoint URL.
#' @return Invisibly returns `TRUE`.
#' @export
register_webhook <- function(url) {
  hooks <- if (file.exists("webhooks.yaml")) yaml::read_yaml("webhooks.yaml") else list(urls = list())
  hooks$urls <- unique(c(hooks$urls, url))
  yaml::write_yaml(hooks, "webhooks.yaml")
  invisible(TRUE)
}

#' Emit an event to registered webhooks
#'
#' @param event Event name.
#' @param payload Named list to send as JSON body.
#' @return Invisibly returns `TRUE`.
#' @export
emit_event <- function(event, payload = list()) {
  if (!file.exists("webhooks.yaml")) return(invisible(TRUE))
  hooks <- yaml::read_yaml("webhooks.yaml")$urls
  for (u in hooks) {
    try(httr::POST(u, body = list(event = event, payload = payload), encode = "json"))
  }
  invisible(TRUE)
}

#' Convert a simple patient record to FHIR format
#'
#' @param record Data frame row with patient information.
#' @return List representing a minimal FHIR Patient resource.
#' @export
to_fhir_patient <- function(record) {
  list(
    resourceType = "Patient",
    id = as.character(record$id),
    gender = record$gender,
    birthDate = as.character(record$birth_date)
  )
}
