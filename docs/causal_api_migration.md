# Causal API Migration Guide

The original causal helpers remain available for compatibility but are deprecated. They will emit warnings and are expected to be removed in a future major release.

The replacement APIs are intentionally more explicit about the estimand, design assumptions, diagnostics, and uncertainty. They are therefore not always drop-in substitutes.

| Legacy helper | Modern API | Migration note |
| --- | --- | --- |
| `estimate_causal_effect()` | `fit_propensity_design()` + `estimate_propensity_effect()` or `estimate_doubly_robust()` | The old function performs matching and returns matched rows. The modern API requires an explicit ATE, ATT, or ATO target and separates design from outcome analysis. |
| `match_cohort()` | `fit_propensity_design()` | There is no exact replacement for the hand-written nearest-neighbour matcher. Weighting and matching define different designs and should not be treated as interchangeable. |
| `propensity_stratification()` | `fit_propensity_design()` | Subclassification is not silently mapped to weighting. Choose the desired target population explicitly and inspect overlap and balance diagnostics. |
| `instrumental_variable()` | `fit_instrumental_variable()` + `instrumental_variable_diagnostics()` + `tidy_instrumental_variable()` | Specify outcome, endogenous treatment, excluded instruments, and exogenous covariates separately. Review first-stage strength before interpreting the structural coefficient. |
| `difference_in_differences()` | `fit_group_time_did()` + `aggregate_group_time_did()` | The replacement targets group-time ATT and requires unit ID, period, and first-treatment cohort. It is not a generic TWFE formula wrapper. |
| `regression_discontinuity()` | `fit_regression_discontinuity()` + RD diagnostics/sensitivity helpers | The replacement records the cutoff, bandwidth, kernel, polynomial order, density diagnostics, covariate continuity, and sensitivity analyses explicitly. |

## Propensity workflows

A modern propensity workflow should normally follow this order:

1. Define the treatment and baseline confounders.
2. Choose the target estimand (`ATE`, `ATT`, or `ATO`).
3. Fit `fit_propensity_design()`.
4. Inspect `propensity_overlap()` and `propensity_balance()` before outcome analysis.
5. Estimate the outcome contrast with `estimate_propensity_effect()` or, for supported ATE/ATT targets, `estimate_doubly_robust()`.

The deprecated matching helpers should not be mechanically translated into weighting without reconsidering the target population.

## Instrumental variables

Replace a compact formula-only IV call with explicit variable roles. The modern API reports the excluded-instrument partial first-stage F statistic, partial R-squared, instrument rank, and overidentification diagnostics when applicable. None of those diagnostics establishes the exclusion restriction or instrument independence.

## Difference in differences

For staggered adoption, migrate from a single fixed-effects interaction coefficient to cohort-by-time ATT estimation. Report the comparison group, treatment timing, anticipation assumptions, aggregation rule, and pre-treatment diagnostic alongside the estimates.

## Regression discontinuity

Move from vector-only calls to a recorded RD design. The modern API makes bandwidth and local-polynomial choices visible and supports density, covariate-continuity, bandwidth-sensitivity, and placebo-cutoff diagnostics. These diagnostics should be prespecified or scientifically motivated rather than searched for favorable results.

## Removal policy

The deprecated wrappers remain functional during the current development line. Removal should occur only in a future major release after downstream callers have had a documented migration window.
