# ------------------------------------------------------------------------------
# File: visualization.R
# Purpose: Visualization helpers for interactive and publication-ready plots
#   including forest plots and network diagrams.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Date Created: 2025-03-??
# ------------------------------------------------------------------------------

#' Apply a publication-ready theme to ggplot2 figures
#'
#' Provides consistent styling with minimal grid lines and larger fonts suitable
#' for manuscripts and presentations.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
#'     ggplot2::geom_point()
#'   p + theme_publication()
#' }
theme_publication <- function() {
  ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "grey90"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
    )
}

#' Convert a ggplot object to an interactive Plotly figure
#'
#' @param plot A ggplot2 plot object.
#'
#' @return A plotly object representing the interactive plot.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("plotly", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
#'     ggplot2::geom_point()
#'   make_interactive(p)
#' }
make_interactive <- function(plot) {
  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object", call. = FALSE)
  }
  plotly::ggplotly(plot)
}

#' Create a forest plot for effect estimates
#'
#' @param data A data frame containing estimate and confidence interval columns.
#' @param estimate Column name for point estimates.
#' @param lower Column name for lower confidence bounds.
#' @param upper Column name for upper confidence bounds.
#' @param label Column name for term labels.
#' @param title Plot title.
#'
#' @return A ggplot object displaying the forest plot.
#'
#' @examples
#' df <- data.frame(
#'   term = c("A", "B"),
#'   estimate = c(1.2, 0.8),
#'   lcl = c(0.9, 0.5),
#'   ucl = c(1.5, 1.1)
#' )
#' forest_plot(df, estimate, lcl, ucl, term)
forest_plot <- function(data, estimate, lower, upper, label, title = "Forest Plot") {
  ggplot2::ggplot(data, ggplot2::aes(x = {{ label }}, y = {{ estimate }})) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = {{ lower }}, ymax = {{ upper }}),
                           width = 0.1) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed") +
    ggplot2::labs(x = NULL, y = "Estimate", title = title) +
    theme_publication()
}

#' Create a network diagram of variable relationships
#'
#' @param edges A data frame with `from` and `to` columns defining edges.
#' @param from Column name for source nodes.
#' @param to Column name for target nodes.
#'
#' @return A ggplot object representing the network diagram.
#'
#' @examples
#' edges <- data.frame(from = c("A", "A", "B"), to = c("B", "C", "C"))
#' variable_network(edges, from, to)
variable_network <- function(edges, from, to) {
  g <- igraph::graph_from_data_frame(edges, directed = FALSE)
  ggraph::ggraph(g, layout = "fr") +
    ggraph::geom_edge_link(alpha = 0.8) +
    ggraph::geom_node_point(size = 5) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE) +
    theme_publication()
}

