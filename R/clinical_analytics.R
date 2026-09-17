# ------------------------------------------------------------------------------
# File: clinical_analytics.R
# Purpose: Clinical analytics and biomarker helpers.
# ------------------------------------------------------------------------------

#' Pharmacovigilance signal detection
#'
#' Computes proportional reporting ratio (PRR) for a drug-event pair.
#'
#' @param a Count of drug and event.
#' @param b Count of drug without event.
#' @param c Count of event without drug.
#' @param d Count of neither drug nor event.
#' @return PRR value.
#' @export
detect_pharmacovigilance <- function(a, b, c, d) {
  ((a / (a + b)) / (c / (c + d)))
}

#' Develop a clinical prediction rule
#'
#' Fits a logistic regression and returns coefficients as a rule.
#'
#' @param formula Formula for logistic regression.
#' @param data Data frame.
#' @return Named coefficient vector.
#' @export
develop_clinical_prediction_rule <- function(formula, data) {
  fit <- stats::glm(formula, data = data, family = stats::binomial())
  stats::coef(fit)
}

#' Biomarker discovery using limma
#'
#' Identifies differentially expressed biomarkers via [limma::eBayes].
#'
#' @param expression Matrix of expression values (genes x samples).
#' @param group Factor indicating sample groups.
#' @return Data frame of top biomarkers.
#' @export
biomarker_discovery <- function(expression, group) {
  ensure_packages("limma")
  design <- stats::model.matrix(~ group)
  fit <- limma::lmFit(expression, design)
  fit <- limma::eBayes(fit)
  limma::topTable(fit, coef = 2)
}
