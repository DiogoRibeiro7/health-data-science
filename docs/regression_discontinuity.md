# Regression Discontinuity

Regression discontinuity (RD) identifies a local treatment effect at a known assignment threshold when treatment status changes discontinuously with a continuously measured running variable and potential outcomes would otherwise evolve smoothly through the cutoff.

## Core estimator

`fit_regression_discontinuity()` fits a sharp local-polynomial RD model with `rdrobust::rdrobust()`. The user specifies the outcome, running variable, cutoff, polynomial order, kernel, and either an explicit bandwidth or a bandwidth-selection rule.

The estimand is local: it concerns units near the cutoff rather than the full observed population. Changing the bandwidth, polynomial order, kernel, or cutoff changes the effective local comparison and should therefore be treated as part of the design rather than as a cosmetic tuning step.

The default local-linear specification uses `p = 1` and bias correction with `q = 2`. `tidy_regression_discontinuity()` exposes the inference rows returned by `rdrobust`, including bias-corrected/robust inference when available.

## Bandwidths and functional form

A narrow bandwidth reduces reliance on extrapolation but can increase variance. A wider bandwidth increases effective sample size while making local-polynomial approximation more consequential. Bandwidth selectors should be reported explicitly, and sensitivity to reasonable alternatives is more informative than choosing the bandwidth that produces the most favorable estimate.

High-order global polynomials are not used by this API. The intended design is local polynomial estimation around the threshold.

## Covariate continuity

`rd_covariate_balance()` fits the same local RD specification to prespecified numeric baseline covariates. Large discontinuities can indicate that units immediately above and below the cutoff are not locally comparable.

A non-significant covariate discontinuity is not proof of balance or exchangeability. Covariates should be chosen because they are substantively predetermined, not because they produce convenient diagnostic results.

## Running-variable density

`rd_density_diagnostic()` wraps `rddensity::rddensity()` to examine whether the density of the running variable changes discontinuously at the cutoff. Such a discontinuity can be consistent with sorting or manipulation around the assignment threshold.

The density test is a diagnostic. Failure to reject continuity does not prove that units cannot manipulate the running variable, while rejection does not by itself establish the mechanism producing the discontinuity.

## Identification assumptions

A causal sharp-RD interpretation requires, at minimum:

- treatment assignment changes deterministically at the stated cutoff;
- potential outcomes are continuous in the running variable at the cutoff in the absence of treatment;
- units cannot precisely sort around the threshold in a way that breaks local comparability;
- the running variable and cutoff are measured correctly;
- no other intervention or institutional rule changes discontinuously at the same threshold in a way that confounds the treatment contrast.

The resulting coefficient is a local treatment effect at the cutoff. It should not be generalized away from the threshold without additional assumptions.
