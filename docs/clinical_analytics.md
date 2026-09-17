# Clinical Analytics

The clinical analytics helpers cover two distinct tasks: pharmacovigilance signal detection and binary clinical prediction. Both require careful interpretation because neither disproportionality metrics nor apparent predictive performance establish clinical causality or transportability.

## Pharmacovigilance disproportionality

`pharmacovigilance_signal()` accepts a 2 x 2 spontaneous-reporting table:

- `a`: reports containing both the exposure or drug and the event;
- `b`: reports containing the exposure or drug without the event;
- `c`: reports containing the event without the exposure or drug;
- `d`: reports containing neither.

The function reports the proportional reporting ratio (PRR), reporting odds ratio (ROR), approximate log-scale confidence intervals, and the Pearson chi-square statistic. If at least one observed cell is zero, a Haldane-Anscombe correction is applied to all four cells before calculating PRR/ROR and their confidence intervals.

The `evans_screening_flag` implements the common descriptive rule requiring at least three co-reports, PRR at least 2, and chi-square at least 4. It is a screening flag only. Spontaneous-reporting data are affected by reporting bias, stimulated reporting, notoriety effects, missing denominator information, duplicate reports, confounding by indication, co-medication, and coding practices. A disproportionality signal therefore supports further assessment; it is not evidence that the exposure caused the event.

`detect_pharmacovigilance()` remains available for backward compatibility and still returns only the scalar PRR.

## Clinical prediction rules

`fit_clinical_prediction_rule()` fits a logistic-regression model for a binary outcome and returns a structured `hds_clinical_prediction_rule` object containing the fitted model, metadata, and development-sample performance.

Accepted outcome encodings are:

- logical values;
- a two-level factor, where the second factor level is treated as the event;
- numeric `0/1` coding.

The model records the event definition in its metadata. Missing values fail by default and can instead be removed explicitly with `na_action = "omit"`.

`tidy_clinical_prediction_rule()` returns logistic coefficients, Wald standard errors, p-values, odds ratios, and confidence intervals.

`clinical_prediction_performance()` reports apparent development-sample metrics including:

- event count and event rate;
- AUC estimated from rank statistics;
- Brier score;
- mean predicted risk and mean calibration error;
- sensitivity, specificity, positive predictive value, and negative predictive value at the prespecified probability threshold.

These are **apparent** performance estimates measured on the same observations used to fit the model. They are usually optimistic. They should not be reported as validated performance without resampling-based internal validation or evaluation in an independent external dataset. Threshold-dependent classification metrics also depend on prevalence and the chosen clinical decision threshold.

`develop_clinical_prediction_rule()` remains backward compatible and returns the coefficient vector from the structured fitted model.

## Interpretation

A clinical prediction model estimates outcome risk; it does not estimate a causal treatment effect unless the design and model explicitly support causal identification. Likewise, a pharmacovigilance disproportionality signal identifies unusual reporting patterns, not biological causation.
