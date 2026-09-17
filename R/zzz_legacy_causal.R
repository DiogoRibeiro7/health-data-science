# ------------------------------------------------------------------------------
# File: zzz_legacy_causal.R
# Purpose: Backward-compatible deprecation shims for legacy causal APIs.
# ------------------------------------------------------------------------------

.legacy_causal_deprecation <- function(old, replacement, note = NULL) {
  message <- paste0(
    "`", old, "()` is deprecated and will be removed in a future major release. ",
    "Use `", replacement, "()` instead."
  )
  if (!is.null(note)) {
    message <- paste(message, note)
  }
  warning(message, call. = FALSE)
}

#' Legacy propensity-score matching helper
#'
#' @description
#' Deprecated compatibility wrapper. It preserves the historical return value
#' from `MatchIt::match.data()`. The modern propensity API is estimand-first and
#' is not a drop-in replacement for nearest-neighbour matching.
#'
#' @inheritParams estimate_causal_effect
#' @return Matched data as in the legacy implementation.
#' @export
estimate_causal_effect <- function(formula, data) {
  .legacy_causal_deprecation(
    "estimate_causal_effect",
    "fit_propensity_design",
    paste(
      "The modern API targets explicit ATE/ATT/ATO weighting estimands;",
      "matching is not assumed to be equivalent to weighting."
    )
  )
  ensure_packages("MatchIt")
  matched <- MatchIt::matchit(formula, data = data)
  MatchIt::match.data(matched)
}

#' Legacy nearest-neighbour cohort matcher
#'
#' @description
#' Deprecated compatibility wrapper preserving the original hand-written
#' nearest-neighbour matching behavior.
#'
#' @param data Data frame with numeric `propensity` and binary `treat` columns.
#' @param caliper Maximum absolute propensity-score distance.
#' @return Matched pairs in the historical wide format.
#' @export
match_cohort <- function(data, caliper = 0.2) {
  .legacy_causal_deprecation(
    "match_cohort",
    "fit_propensity_design",
    "There is no one-to-one replacement because the new API is weighting-based."
  )
  stopifnot(all(c("propensity", "treat") %in% names(data)))
  treated <- data[data$treat == 1, ]
  control <- data[data$treat == 0, ]
  matches <- lapply(seq_len(nrow(treated)), function(i) {
    distances <- abs(control$propensity - treated$propensity[i])
    j <- which.min(distances)
    if (length(j) && distances[j] <= caliper) {
      cbind(treated[i, ], control[j, ])
    }
  })
  do.call(rbind, matches)
}

#' Legacy propensity-score subclassification helper
#'
#' @description
#' Deprecated compatibility wrapper preserving the historical MatchIt object.
#'
#' @param formula Treatment ~ covariates formula.
#' @param data Data frame.
#' @param subclass Number of subclasses.
#' @return A MatchIt object from subclassification.
#' @export
propensity_stratification <- function(formula, data, subclass = 5) {
  .legacy_causal_deprecation(
    "propensity_stratification",
    "fit_propensity_design",
    paste(
      "Subclassification and weighting target different designs; choose the",
      "modern estimand explicitly rather than treating them as interchangeable."
    )
  )
  ensure_packages("MatchIt")
  MatchIt::matchit(formula, data = data, method = "subclass", subclass = subclass)
}

#' Legacy formula-based instrumental-variable helper
#'
#' @description
#' Deprecated compatibility wrapper preserving the historical `ivreg` return
#' object. The modern API separates outcome, treatment, instruments, diagnostics,
#' and effect interpretation.
#'
#' @param formula Formula accepted by `AER::ivreg()`.
#' @param data Data frame.
#' @return An `ivreg` model.
#' @export
instrumental_variable <- function(formula, data) {
  .legacy_causal_deprecation(
    "instrumental_variable",
    "fit_instrumental_variable"
  )
  ensure_packages("AER")
  AER::ivreg(formula, data = data)
}

#' Legacy two-way fixed-effects difference-in-differences helper
#'
#' @description
#' Deprecated compatibility wrapper preserving the historical `fixest::feols()`
#' return object. The modern API targets group-time ATT under staggered adoption.
#'
#' @param formula Formula accepted by `fixest::feols()`.
#' @param data Data frame.
#' @return A `fixest` model.
#' @export
 difference_in_differences <- function(formula, data) {
  .legacy_causal_deprecation(
    "difference_in_differences",
    "fit_group_time_did",
    paste(
      "The replacement is not a drop-in TWFE model; it targets group-time ATT",
      "and requires unit, period, and first-treatment cohort columns."
    )
  )
  ensure_packages("fixest")
  fixest::feols(formula, data = data)
}

#' Legacy vector-based regression-discontinuity helper
#'
#' @description
#' Deprecated compatibility wrapper preserving the historical `rdrobust` return
#' object. The modern API records cutoff design choices and diagnostics.
#'
#' @param y Numeric outcome vector.
#' @param x Numeric running variable.
#' @param c Cutoff.
#' @return An `rdrobust` object.
#' @export
regression_discontinuity <- function(y, x, c = 0) {
  .legacy_causal_deprecation(
    "regression_discontinuity",
    "fit_regression_discontinuity"
  )
  ensure_packages("rdrobust")
  rdrobust::rdrobust(y, x, c = c)
}
