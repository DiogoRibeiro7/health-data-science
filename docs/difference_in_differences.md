# Group-Time Difference-in-Differences

`fit_group_time_did()` implements staggered-adoption difference-in-differences
through cohort-by-period average treatment effects rather than collapsing the
design into a single two-way fixed-effects coefficient.

## Treatment timing

Each unit is assigned a first-treated period. Never-treated units are coded with
`first_treated = 0`. Treatment timing must be constant within unit and every
positive treatment cohort must correspond to an observed panel period.

The estimator supports two control-group definitions:

- `nevertreated`: units that are never treated provide the comparison group.
- `notyettreated`: units that have not yet adopted treatment may contribute as
  controls before their own treatment starts.

These choices define different comparisons and should be selected from the
study design rather than chosen by fit statistics.

## Group-time ATT

For treatment cohort `g` and period `t`, the fundamental estimand is

\[
\operatorname{ATT}(g,t)
= E\left[Y_t(1)-Y_t(0)\mid G=g\right].
\]

The package delegates estimation to `did::att_gt()`. Covariates may be supplied
for conditional parallel-trends adjustment. The default estimation method is
`dr`, with inverse-probability (`ipw`) and regression (`reg`) alternatives also
available.

The group-time formulation avoids interpreting a single TWFE coefficient as a
simple average treatment effect when adoption is staggered and treatment effects
vary across cohorts or exposure times.

## Event-study aggregation

`aggregate_group_time_did(type = "dynamic")` aggregates cohort-time effects by
exposure time relative to treatment. Other supported aggregations are:

- `simple`: one overall ATT summary;
- `group`: treatment-cohort summaries;
- `calendar`: calendar-period summaries.

Dynamic aggregation is the appropriate surface for studying how treatment
effects evolve before and after adoption. Event-time effects should still be
interpreted with attention to which cohorts contribute to each horizon.

## Parallel trends and pre-treatment diagnostics

`did_pretrend_diagnostic()` reports the joint pre-treatment Wald statistic and
p-value produced by the underlying group-time estimator.

A failure to reject this test does **not** establish parallel trends. Pre-trend
tests may have low power, particularly with few pre-treatment periods, noisy
outcomes, small cohorts, or heterogeneous trends. The identifying assumption is
substantive and should also be assessed using design knowledge, graphical
pre-treatment trajectories, and sensitivity analysis.

Likewise, rejecting the joint pre-treatment test indicates tension with the
specified design or model but does not by itself identify the source of that
failure.

## Anticipation

The `anticipation` argument allows the analyst to declare that treatment may
begin affecting outcomes before the recorded adoption period. This shifts which
periods are treated as uncontaminated pre-treatment comparisons and should be
specified from subject-matter knowledge rather than tuned after seeing the
results.

## Inference

The wrapper exposes the bootstrap and simultaneous-band options of the `did`
package. Pointwise normal intervals returned by `tidy_group_time_did()` and
`aggregate_group_time_did()` are compact summaries; simultaneous confidence
bands from the underlying estimator are preferable when making joint statements
across many event times.

## Identification assumptions

Causal interpretation requires a suitable version of parallel trends for the
chosen control group, no problematic anticipation beyond what is explicitly
modelled, consistent treatment definition, and sufficient overlap in the
covariate-adjusted design. Group-time estimation solves the weighting pathology
of naive staggered TWFE comparisons; it does not remove the need for a credible
research design.
