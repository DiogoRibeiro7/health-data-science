# Instrumental Variable Analysis

`fit_instrumental_variable()` fits a linear two-stage least-squares model with one endogenous treatment and one or more excluded instruments. The API makes the treatment, instruments, and included exogenous covariates explicit rather than encoding them only inside a formula.

## First-stage relevance

Instrument relevance is summarised by the partial first-stage F statistic and partial R-squared from nested first-stage regressions. The partial F statistic tests the excluded instruments jointly after conditioning on the included covariates. Partial R-squared describes the incremental share of residual treatment variation explained by those instruments.

The default `weak_f_threshold = 10` is a descriptive warning threshold only. It is not a universal weak-instrument critical value, and exceeding it does not establish that an instrument is valid. Instrument strength depends on the inferential problem, the number of instruments, and the estimator being used.

## Identification assumptions

A causal IV interpretation requires more than first-stage relevance. The analyst must justify instrument independence, the exclusion restriction, and the structural interpretation of the treatment coefficient. These assumptions are not identified by the observed data alone.

For a single binary instrument and binary treatment, the fitted object marks the design as potentially compatible with a local average treatment effect interpretation. That interpretation additionally requires monotonicity and applies to compliers rather than automatically to the full population.

## Overidentification

When the excluded instruments add estimable rank greater than one after conditioning on included covariates, the package reports a Sargan statistic. This is an overidentification specification check under homoskedastic linear-IV assumptions. A non-significant Sargan test does not prove the exclusion restriction or instrument independence. A significant result indicates that the collection of instruments and structural assumptions is difficult to reconcile with the observed residual restrictions.

## Inference

`tidy_instrumental_variable()` currently reports the conventional homoskedastic 2SLS covariance matrix supplied by `AER::ivreg`. Heteroskedasticity-robust, cluster-robust, and weak-instrument-robust inference are separate extensions and should not be inferred from the current Wald interval.

`instrumental_variable_diagnostics()` returns first-stage strength statistics, the descriptive weak-instrument flag, the excluded-instrument rank conditional on included covariates, and the overidentification diagnostic when available.
