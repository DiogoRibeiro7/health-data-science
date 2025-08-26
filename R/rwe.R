#' Real-World Evidence Utilities
#'
#' Tools for observational study design, EHR analytics, comparative effectiveness
#' research, regulatory compliance, data quality assessment and patient centred
#' outcomes.
#' These helpers focus on pragmatic healthcare research workflows.
#'
#' @name rwe_tools
NULL

# Study design --------------------------------------------------------------

#' Select a cohort based on inclusion and exclusion rules
#'
#' @param data Data frame of patient records
#' @param include Expression evaluated in the context of `data` defining rows to keep
#' @param exclude Expression defining rows to drop
#' @return Filtered data frame representing the cohort
#' @export
select_cohort <- function(data, include = TRUE, exclude = FALSE) {
  data <- data[eval(substitute(include), data, parent.frame()), , drop = FALSE]
  data <- data[!eval(substitute(exclude), data, parent.frame()), , drop = FALSE]
  data
}

#' Match treated and control patients using nearest neighbour propensity scores
#'
#' @param data Data frame containing a numeric `propensity` column and a
#'   binary `treat` column
#' @param caliper Maximum distance for matches
#' @return Data frame of matched pairs
#' @export
match_cohort <- function(data, caliper = 0.2) {
  stopifnot(all(c("propensity", "treat") %in% names(data)))
  treated <- data[data$treat == 1, ]
  control <- data[data$treat == 0, ]
  matches <- lapply(seq_len(nrow(treated)), function(i) {
    dists <- abs(control$propensity - treated$propensity[i])
    j <- which.min(dists)
    if (length(j) && dists[j] <= caliper) {
      cbind(treated[i, ], control[j, ])
    }
  })
  do.call(rbind, matches)
}

#' Power analysis for observational study
#'
#' Simple approximation using normal distribution assumptions.
#'
#' @param effect Expected effect size
#' @param alpha Significance level
#' @param power Desired power
#' @return Required sample size per group
#' @export
power_observational <- function(effect, alpha = 0.05, power = 0.8) {
  z_alpha <- stats::qnorm(1 - alpha / 2)
  z_beta <- stats::qnorm(power)
  n <- 2 * (z_alpha + z_beta)^2 / effect^2
  ceiling(n)
}

#' Sample size for non-randomised comparison
#'
#' @param effect Expected effect size
#' @param alpha Significance level
#' @param power Desired power
#' @param ratio Allocation ratio of control:treatment
#' @return Total sample size
#' @export
sample_size_nonrandom <- function(effect, alpha = 0.05, power = 0.8, ratio = 1) {
  n1 <- power_observational(effect, alpha, power)
  ceiling(n1 * (1 + ratio))
}

#' Simple bias sensitivity analysis
#'
#' Adds a bias term to an effect estimate across a range of possible values.
#'
#' @param estimate Observed effect estimate
#' @param bias_range Numeric vector of bias values
#' @return Data frame of adjusted estimates
#' @export
bias_sensitivity <- function(estimate, bias_range = seq(-0.1, 0.1, by = 0.05)) {
  data.frame(bias = bias_range, adjusted = estimate - bias_range)
}

# EHR analytics -------------------------------------------------------------

#' Map clinical data to a very small subset of the OMOP CDM
#'
#' @param data Data frame with columns `patient_id`, `code`, `value`
#' @return Data frame with standardised column names
#' @export
omop_standardize <- function(data) {
  names(data) <- tolower(names(data))
  data.frame(person_id = data$patient_id, concept_id = data$code, value = data$value)
}

#' Extract simple concepts from clinical note text
#'
#' @param notes Character vector of clinical notes
#' @return Data frame of detected terms and counts
#' @export
nlp_extract_concepts <- function(notes) {
  terms <- unlist(strsplit(tolower(paste(notes, collapse = " ")), "\\W+"))
  tbl <- table(terms[terms != ""])
  data.frame(term = names(tbl), count = as.integer(tbl))
}

#' Phenotype algorithm for diabetes using diagnostic codes
#'
#' @param data Data frame with column `code`
#' @return Logical vector indicating diabetes cases
#' @export
phenotype_diabetes <- function(data) {
  grepl("^E1[0-3]", data$code)
}

#' Summarise drug exposure from prescription records
#'
#' @param data Data frame with `patient_id`, `drug`, `dose`
#' @return Total exposure per patient
#' @export
drug_exposure_summary <- function(data) {
  aggregate(dose ~ patient_id, data = data, sum)
}

# Comparative effectiveness -------------------------------------------------

#' Select an active comparator cohort
#'
#' @param data Data frame with treatment column
#' @param treatment Name of treatment to compare against
#' @return Filtered data frame for comparator
#' @export
active_comparator <- function(data, treatment) {
  data[data$treatment == treatment, , drop = FALSE]
}

#' Adjust for confounding via simple outcome regression
#'
#' @param formula Model formula
#' @param data Data frame
#' @return Fitted model
#' @export
confounding_adjust <- function(formula, data) {
  stats::glm(formula, data = data, family = stats::gaussian())
}

#' Combine treatment effects from multiple studies
#'
#' @param effects Numeric vector of effect estimates
#' @param ses Numeric vector of standard errors
#' @return Pooled effect estimate
#' @export
multiple_treatment_compare <- function(effects, ses) {
  w <- 1 / ses^2
  sum(w * effects) / sum(w)
}

#' Basic health outcomes analysis
#'
#' @param outcome Numeric outcome vector
#' @return Mean outcome
#' @export
health_outcomes <- function(outcome) {
  mean(outcome, na.rm = TRUE)
}

# Regulatory compliance ----------------------------------------------------

#' Check minimal GVP compliance items
#'
#' @param report Character vector of report sections
#' @return Logical flag for presence of key sections
#' @export
gvp_compliance <- function(report) {
  all(c("safety", "efficacy", "population") %in% tolower(report))
}

#' Structure study metadata per FDA RWE framework
#'
#' @param study List containing elements `data`, `analysis`, `submission`
#' @return Logical indicating completeness
#' @export
fda_rwe_ready <- function(study) {
  all(c("data", "analysis", "submission") %in% names(study))
}

#' Create an EMA PICO record
#'
#' @param population Intervention comparator outcome strings
#' @return Named list representing PICO
#' @export
ema_pico <- function(population, intervention, comparator, outcome) {
  list(population = population, intervention = intervention,
       comparator = comparator, outcome = outcome)
}

#' Check STROBE item inclusion
#'
#' @param sections Named logical vector of STROBE sections
#' @return TRUE if all required sections are present
#' @export
strobe_compliant <- function(sections) {
  all(sections)
}

# Data quality --------------------------------------------------------------

#' Calculate completeness of each column
#'
#' @param data Data frame
#' @return Named numeric vector of completeness proportions
#' @export
data_completeness <- function(data) {
  sapply(data, function(x) mean(!is.na(x)))
}

#' Validate data against simple rules
#'
#' @param data Data frame
#' @param rules Named list of predicate expressions
#' @return Logical vector of rows passing all rules
#' @export
data_validate <- function(data, rules) {
  keep <- rep(TRUE, nrow(data))
  for (nm in names(rules)) {
    keep <- keep & eval(rules[[nm]], data, parent.frame())
  }
  keep
}

#' Detect outliers via IQR rule for numeric columns
#'
#' @param x Numeric vector
#' @return Logical vector indicating outliers
#' @export
detect_outliers_iqr <- function(x) {
  q <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(q)
  x < (q[1] - 1.5 * iqr) | x > (q[2] + 1.5 * iqr)
}

#' Check temporal consistency of observation times
#'
#' @param dates Vector of dates
#' @return TRUE if dates are non-decreasing
#' @export
temporal_consistency <- function(dates) {
  all(diff(as.numeric(dates)) >= 0)
}

# Patient-centred outcomes --------------------------------------------------

#' Analyse patient reported outcomes by computing mean scores
#'
#' @param scores Numeric vector of PROM scores
#' @return Mean score
#' @export
analyze_prom <- function(scores) {
  mean(scores, na.rm = TRUE)
}

#' Simple quality of life utility index
#'
#' @param responses Numeric matrix of item responses
#' @return Vector of respondent utilities
#' @export
quality_of_life_index <- function(responses) {
  rowMeans(responses, na.rm = TRUE)
}

#' Model health related quality of life with linear regression
#'
#' @param formula Model formula
#' @param data Data frame
#' @return Fitted model
#' @export
hrqol_model <- function(formula, data) {
  stats::lm(formula, data)
}

#' Summarise patient preference counts
#'
#' @param options Character vector of patient choices
#' @return Table of preference counts
#' @export
patient_preference <- function(options) {
  as.data.frame(table(options), stringsAsFactors = FALSE)
}
