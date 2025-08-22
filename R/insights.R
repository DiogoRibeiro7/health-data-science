#' Detect statistical significance
#'
#' Evaluate model coefficients and flag significant terms.
#' @param model Fitted model object supporting summary().
#' @return Data frame with estimates, p-values, and significance flag.
#' @export
detect_significance <- function(model) {
  sm <- summary(model)
  coefs <- as.data.frame(sm$coefficients)
  coefs$term <- rownames(sm$coefficients)
  coefs$significant <- coefs[[ncol(coefs)]] < 0.05
  coefs
}

#' Analyze trends in a numeric vector
#'
#' Fits a linear model to estimate trend direction.
#' @param x Numeric vector.
#' @return List with slope and trend label.
#' @export
analyze_trends <- function(x) {
  fit <- stats::lm(x ~ seq_along(x))
  slope <- coef(fit)[2]
  list(slope = slope, trend = ifelse(slope > 0, "increasing", "decreasing"))
}

#' Detect simple anomalies
#'
#' Identify values beyond k standard deviations from the mean.
#' @param x Numeric vector.
#' @param k Threshold in standard deviations.
#' @return Indices of anomalous observations.
#' @export
detect_anomalies <- function(x, k = 3) {
  mu <- mean(x)
  sdv <- stats::sd(x)
  which(abs(x - mu) > k * sdv)
}

#' Generate narrative from metrics
#'
#' Produce a short text summary highlighting trends.
#' @param metrics List containing `n` and `trend` elements.
#' @return Character string.
#' @export
generate_narrative <- function(metrics) {
  sprintf("Dataset with %d records shows a %s trend.", metrics$n, metrics$trend)
}

#' Calculate simple KPIs
#'
#' Compute mean values for selected columns.
#' @param data Data frame.
#' @param cols Character vector of column names.
#' @return Named numeric vector of means.
#' @export
calculate_kpis <- function(data, cols) {
  sapply(cols, function(c) mean(data[[c]], na.rm = TRUE))
}
