# ------------------------------------------------------------------------------
# File: population_health.R
# Purpose: Population health and epidemiological analysis utilities
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Case-control study analysis with optional matching
#'
#' Fits a logistic regression to estimate the association between an exposure
#' and outcome. When `match_var` is provided, a conditional logistic regression
#' is approximated by including the matching factor as a fixed effect.
#'
#' @param data Data frame containing outcome, exposure and optional matching variable.
#' @param outcome Name of binary outcome variable.
#' @param exposure Name of exposure variable.
#' @param match_var Optional name of matching variable or stratum identifier.
#'
#' @return Fitted `glm` object.
#' @export
#'
#' @examples
#' df <- data.frame(y = c(1,0,1,0), x = c(1,0,0,1), pair = rep(1:2, each = 2))
#' fit <- case_control_match(df, "y", "x", match_var = "pair")
case_control_match <- function(data, outcome, exposure, match_var = NULL) {
  if (!outcome %in% names(data) || !exposure %in% names(data)) {
    stop("Outcome and exposure must be columns in data", call. = FALSE)
  }
  formula <- if (is.null(match_var)) {
    stats::as.formula(paste(outcome, "~", exposure))
  } else {
    stats::as.formula(paste(outcome, "~", exposure, "+ factor(", match_var, ")"))
  }
  stats::glm(formula, data = data, family = stats::binomial())
}

#' Cohort study with time-to-event outcome
#'
#' Fits a Cox proportional hazards model for cohort data.
#'
#' @param data Data frame containing time, status and exposure.
#' @param time Name of time-to-event variable.
#' @param status Name of event indicator (1 = event, 0 = censor).
#' @param exposure Exposure variable to estimate hazard ratio for.
#'
#' @return Fitted `coxph` model.
#' @export
cohort_time_to_event <- function(data, time, status, exposure) {
  ensure_packages("survival")
  formula <- stats::as.formula(paste0("survival::Surv(", time, ",", status, ") ~ ", exposure))
  survival::coxph(formula, data = data)
}

#' Cross-sectional survey analysis with complex sampling
#'
#' Uses the [survey] package to fit a design-based GLM.
#'
#' @param formula Model formula.
#' @param data Survey data frame.
#' @param weights Sampling weights variable name.
#' @param ids Cluster id variable name (optional).
#' @param strata Strata variable name (optional).
#'
#' @return Fitted `svyglm` object.
#' @export
cross_sectional_survey <- function(formula, data, weights, ids = NULL, strata = NULL) {
  ensure_packages("survey")
  design <- survey::svydesign(
    ids = if (is.null(ids)) ~1 else stats::as.formula(paste0("~", ids)),
    strata = if (is.null(strata)) NULL else stats::as.formula(paste0("~", strata)),
    weights = stats::as.formula(paste0("~", weights)),
    data = data
  )
  survey::svyglm(formula, design = design)
}

#' Ecological study analysis with spatial correlation
#'
#' Computes Moran's I statistic to assess spatial autocorrelation in regional
#' outcome rates.
#'
#' @param data Data frame with outcome and region identifiers.
#' @param outcome Name of outcome variable.
#' @param region Name of region identifier.
#' @param neighbors List-style object of regional neighbors as produced by
#'   [spdep::poly2nb] or similar.
#'
#' @return Result of [spdep::moran.test].
#' @export
ecological_spatial_analysis <- function(data, outcome, region, neighbors) {
  ensure_packages("spdep")
  if (!all(c(outcome, region) %in% names(data))) {
    stop("Outcome and region must be present in data", call. = FALSE)
  }
  w <- spdep::nb2listw(neighbors)
  spdep::moran.test(data[[outcome]], w)
}

#' Detect spatial clusters using local Moran's I
#'
#' Identifies areas with unusually high or low values by calculating local
#' Moran's I statistics.
#'
#' @param data Data frame with value column and coordinates.
#' @param value Column name containing rates or counts.
#' @param coords Matrix or two-column data frame of coordinates.
#'
#' @return Data frame with local Moran's I values.
#' @export
spatial_cluster_scan <- function(data, value, coords) {
  ensure_packages("spdep")
  if (is.null(dim(coords)) || ncol(coords) != 2) {
    stop("coords must be a two-column matrix or data frame", call. = FALSE)
  }
  nb <- spdep::knearneigh(coords, k = 5)
  lw <- spdep::nb2listw(spdep::knn2nb(nb))
  lm <- spdep::localmoran(data[[value]], lw)
  cbind(data, lm)
}

#' Join data to spatial polygons
#'
#' Reads a shapefile and merges it with epidemiological data using key columns.
#'
#' @param data Data frame with region identifiers and values.
#' @param shapefile Path to a polygon shapefile.
#' @param key_data Column in `data` matching `key_shape`.
#' @param key_shape Column in shapefile matching `key_data`.
#'
#' @return sf object with merged data.
#' @export
gis_join <- function(data, shapefile, key_data, key_shape) {
  ensure_packages("sf")
  shp <- sf::st_read(shapefile, quiet = TRUE)
  merge(shp, data, by.x = key_shape, by.y = key_data)
}

#' Create a choropleth disease map
#'
#' Plots an sf object coloured by a specified value column.
#'
#' @param sf_data sf object with geometry and values.
#' @param value Column name with values to map.
#'
#' @return ggplot object.
#' @export
disease_map <- function(sf_data, value) {
  ensure_packages(c("sf", "ggplot2"))
  ggplot2::ggplot(sf_data) +
    ggplot2::geom_sf(ggplot2::aes_string(fill = value)) +
    ggplot2::theme_void()
}

#' Simulate a basic SIR infectious disease model
#'
#' Uses ordinary differential equations solved via [deSolve::ode].
#'
#' @param beta Transmission rate.
#' @param gamma Recovery rate.
#' @param S0,I0,R0 Initial susceptible, infected and recovered counts.
#' @param times Time points to simulate.
#'
#' @return Matrix of compartment sizes over time.
#' @export
simulate_sir <- function(beta, gamma, S0, I0, R0, times) {
  ensure_packages("deSolve")
  sir <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
      dS <- -beta * S * I
      dI <- beta * S * I - gamma * I
      dR <- gamma * I
      list(c(dS, dI, dR))
    })
  }
  state <- c(S = S0, I = I0, R = R0)
  params <- c(beta = beta, gamma = gamma)
  out <- deSolve::ode(y = state, times = times, func = sir, parms = params)
  as.data.frame(out)
}

#' Simple health disparity index
#'
#' Calculates risk ratio between two groups for a binary outcome.
#'
#' @param data Data frame containing outcome and group variables.
#' @param outcome Name of binary outcome variable.
#' @param group Grouping variable (must have exactly two levels).
#'
#' @return Risk ratio estimate.
#' @export
health_disparity_index <- function(data, outcome, group) {
  tab <- table(data[[group]], data[[outcome]])
  if (nrow(tab) != 2 || ncol(tab) != 2) {
    stop("Group and outcome must both be binary", call. = FALSE)
  }
  risk1 <- tab[1, 2] / sum(tab[1, ])
  risk2 <- tab[2, 2] / sum(tab[2, ])
  risk1 / risk2
}

#' Syndromic surveillance using rolling averages
#'
#' Flags observations that deviate from a rolling mean by more than `sd_limit`
#' standard deviations.
#'
#' @param counts Numeric vector of case counts over time.
#' @param window Rolling window size.
#' @param sd_limit Threshold in standard deviations.
#'
#' @return Logical vector indicating anomalies.
#' @export
syndromic_surveillance <- function(counts, window = 7, sd_limit = 3) {
  if (length(counts) < window) {
    stop("`counts` length must be at least as large as `window`", call. = FALSE)
  }
  ma <- stats::filter(counts, rep(1 / window, window), sides = 1)
  sd_roll <- sqrt(stats::filter((counts - ma)^2, rep(1 / window, window), sides = 1))
  (counts - ma) > sd_limit * sd_roll
}

#' Simple air pollution exposure model
#'
#' Estimates health outcome associations with pollutant exposure via linear
#' regression.
#'
#' @param data Data frame containing outcome and pollutant concentration.
#' @param outcome Name of numeric outcome variable.
#' @param pollutant Name of pollutant concentration variable.
#'
#' @return Fitted `lm` object.
#' @export
model_air_pollution <- function(data, outcome, pollutant) {
  formula <- stats::as.formula(paste(outcome, "~", pollutant))
  stats::lm(formula, data = data)
}
