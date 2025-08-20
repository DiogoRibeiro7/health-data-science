# ------------------------------------------------------------------------------
# File: reporting.R
# Purpose: Utilities for automated reporting using parameterized R Markdown
#   templates and formatted tables.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Date Created: 2025-03-??
# ------------------------------------------------------------------------------

#' Render a parameterized R Markdown report
#'
#' @param template Path to an Rmd template file.
#' @param params List of parameters to pass to the template.
#' @param output_file Optional path for the rendered report.
#'
#' @return Path to the rendered output file.
#'
#' @examples
#' \dontrun{
#' render_report("templates/lca_report.Rmd", params = list(input = "data/lca.RData"))
#' }
render_report <- function(template, params = list(), output_file = NULL) {
  rmarkdown::render(template, params = params, output_file = output_file, quiet = TRUE)
}

#' Format a data frame as a publication-ready table
#'
#' @param data Data frame to format.
#' @param ... Additional arguments passed to `knitr::kable`.
#'
#' @return A `knitr_kable` object.
#'
#' @examples
#' format_table(head(mtcars))
format_table <- function(data, ...) {
  knitr::kable(data, ...)
}

#' Generate an executive summary report
#'
#' @param metrics Named list of key metrics to include in the summary.
#' @param output_file Output path for the generated report.
#'
#' @return Path to the rendered report.
#'
#' @examples
#' \dontrun{
#' generate_executive_summary(list(Accuracy = 0.9), "summary.html")
#' }
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
}

#' Create a comparison report between model runs
#'
#' @param summaries List of data frames containing model summaries.
#' @param output_file Output path for the generated report.
#'
#' @return Path to the rendered report.
#'
#' @examples
#' \dontrun{
#' compare_models(list(model1 = data.frame(a = 1), model2 = data.frame(a = 2)),
#'                "compare.html")
#' }
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
}

