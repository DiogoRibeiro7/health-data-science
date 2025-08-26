#' Interactive scatter plot
#'
#' Creates an interactive scatter plot using ggplot2 and plotly.
#' @param data Data frame containing variables.
#' @param x Name of x variable.
#' @param y Name of y variable.
#' @return A plotly object.
#' @export
plot_interactive <- function(data, x, y) {
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
    ggplot2::geom_point()
  plotly::ggplotly(p)
}

#' Network visualization
#'
#' Render a network graph for node and edge data.
#' @param nodes Data frame of nodes with id and label.
#' @param edges Data frame of edges with from and to columns.
#' @return A visNetwork htmlwidget.
#' @export
plot_network <- function(nodes, edges) {
  visNetwork::visNetwork(nodes, edges)
}

#' Geographic map visualization
#'
#' Plot geographic points on an interactive map.
#' @param data Data frame containing latitude and longitude columns.
#' @param lat Name of latitude column.
#' @param lon Name of longitude column.
#' @param value Optional column for popup values.
#' @return A leaflet map widget.
#' @export
plot_geo <- function(data, lat, lon, value = NULL) {
  m <- leaflet::leaflet(data)
  m <- leaflet::addTiles(m)
  popup <- if (!is.null(value)) data[[value]] else NULL
  leaflet::addCircleMarkers(m, data[[lon]], data[[lat]], popup = popup)
}

#' Time series visualization
#'
#' Create an interactive time series plot.
#' @param data Data frame with time and value columns.
#' @param time Name of time variable.
#' @param value Name of value variable.
#' @return A plotly object.
#' @export
plot_timeseries <- function(data, time, value) {
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = time, y = value)) +
    ggplot2::geom_line()
  plotly::ggplotly(p)
}
