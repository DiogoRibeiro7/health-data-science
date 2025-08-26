# ------------------------------------------------------------------------------
# File: reporting.R
# Purpose: Utilities for automated reporting using parameterized R Markdown
#   templates and formatted tables.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# ------------------------------------------------------------------------------

#' Render a parameterized R Markdown report
#'
#' @param template Path to an Rmd template file.
#' @param params List of parameters to pass to the template.
#' @param output_file Optional path for the rendered report.
#'
#' @return Path to the rendered output file.
#' @export
render_report <- function(template, params = list(), output_file = NULL) {
  rmarkdown::render(template, params = params, output_file = output_file, quiet = TRUE)
}

#' Format a data frame as a publication-ready table
#'
#' @param data Data frame to format.
#' @param ... Additional arguments passed to `knitr::kable`.
#'
#' @return A `knitr_kable` object.
#' @export
format_table <- function(data, ...) {
  knitr::kable(data, ...)
}

#' Generate an executive summary report
#'
#' @param metrics Named list of key metrics to include in the summary.
#' @param output_file Output path for the generated report.
#'
#' @return Path to the rendered report.
#' @export
generate_executive_summary <- function(metrics, output_file) {
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp), add = TRUE)
  lines <- c(
    "---",
    "title: 'Executive Summary'",
    "output: html_document",
    "params:",
    "  metrics: NULL",
    "---",
    "",
    "```{r, echo=FALSE}",
    "knitr::kable(as.data.frame(params$metrics), col.names = c('Metric', 'Value'))",
    "```"
  )
  writeLines(lines, tmp)
  rmarkdown::render(tmp, params = list(metrics = metrics), output_file = output_file, quiet = TRUE)
  output_file
}

#' Create a comparison report between model runs
#'
#' @param summaries List of data frames containing model summaries.
#' @param output_file Output path for the generated report.
#'
#' @return Path to the rendered report.
#' @export
compare_models <- function(summaries, output_file) {
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp), add = TRUE)
  body <- unlist(lapply(names(summaries), function(name) {
    c(sprintf("## %s", name), "", "```{r, echo=FALSE}",
      sprintf("knitr::kable(summaries[['%s']])", name), "```")
  }))
  lines <- c("---", "title: 'Model Comparison'", "output: html_document", "---", "", body)
  writeLines(lines, tmp)
  rmarkdown::render(tmp, output_file = output_file, quiet = TRUE)
  output_file
}

# New reporting helpers -------------------------------------------------------

#' Generate analysis reports
#'
#' Render a report from built-in templates.
#' @param type Report type: "publication", "executive", "compliance", or "interactive".
#' @param params List of parameters passed to the template.
#' @param output_file Path to output file. If NULL, a temporary file is created.
#' @return Path to rendered report.
#' @export
generate_report <- function(type = c("publication", "executive", "compliance", "interactive"),
                             params = list(), output_file = NULL) {
  type <- match.arg(type)
  template <- switch(type,
                     publication = "templates/publication_report.Rmd",
                     executive = "templates/executive_summary.Rmd",
                     compliance = "templates/compliance_report.Rmd",
                     interactive = "templates/interactive_dashboard.Rmd")
  if (is.null(output_file)) {
    ext <- if (type == "publication") ".pdf" else ".html"
    output_file <- tempfile(fileext = ext)
  }
  rmarkdown::render(template, params = params, output_file = output_file, quiet = TRUE)
  output_file
}

#' Schedule a report to be generated
#'
#' This is a placeholder that records the intent to schedule rendering.
#' @param rmd Path to template.
#' @param cron Cron-like schedule expression.
#' @param output_dir Directory where reports should be stored.
#' @export
schedule_report <- function(rmd, cron, output_dir = "reports") {
  ensure_dir(output_dir)
  entry <- sprintf("%s,%s,%s", rmd, cron, output_dir)
  utils::write(entry, file.path(output_dir, "schedule.txt"), append = TRUE)
  invisible(TRUE)
}

#' Share a report to an external destination
#'
#' Logs a share action for audit purposes.
#' @param file Path to report file.
#' @param destination Destination identifier (e.g., "sharepoint", "email").
#' @export
share_report <- function(file, destination) {
  logger::log_info("Sharing {file} via {destination}")
  TRUE
}
