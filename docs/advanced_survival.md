# Advanced Cox Survival Models

The advanced Cox helpers expose model structure and risk-set assumptions rather than silently passing formulas to `survival::coxph()`.

## Time-varying coefficients

`fit_time_varying_cox()` requires at least one `tt()` term in the formula. An optional `tt` function or list of functions can be passed explicitly. Missingness and tie handling are controlled through `na_action` and `ties`, and the fitted object records fitted sample size, event count, omitted rows, tie method, and whether a custom time transformation was supplied.

A `tt()` model describes a covariate effect that changes with analysis time. The resulting coefficient should therefore be interpreted together with the time transformation; it is not a single constant proportional-hazards effect over follow-up.

## Frailty models

`fit_frailty_cox()` requires a formula containing `frailty()`. It records the fitted sample and event count and makes tie/missingness handling explicit.

Frailty terms represent latent heterogeneity or clustering through a random-effect-style survival component. The fixed-effect hazard ratios from the model should not be interpreted as if the frailty distribution were absent.

## Landmark analysis

`landmark_cox()` conditions the analysis on subjects still under observation at a prespecified landmark. The follow-up-time column is explicit through `time`, defaulting to `"time"` for compatibility.

The wrapper only supports a two-argument `Surv(time, status)` response. Counting-process `Surv(start, stop, status)` data are rejected because landmarking such data requires explicit construction of start-stop risk intervals rather than simple row filtering.

By default, retained follow-up times are shifted so the landmark is time zero. This does not change the Cox partial likelihood but makes the post-landmark time origin explicit. Metadata record the number of subjects before and at the landmark, exclusions before the landmark, event count, tie method, and omitted rows.

Landmark analysis estimates associations conditional on being alive/event-free and observed at the landmark. It does not recover a marginal baseline-treatment effect and can introduce selection if the landmark cohort is interpreted without that conditioning.

## Common coefficient summaries

`tidy_cox_model()` provides a shared fixed-effect summary for Cox models: coefficient, standard error, Wald statistic, p-value, hazard ratio, and confidence interval. When a model was fitted through one of the hardened wrappers, the output also reports the model type.

For time-varying effects, the reported exponentiated coefficient is tied to the chosen time transformation. For frailty models, the table summarizes fixed effects only. For landmark models, hazard ratios apply to the landmark-conditioned risk set.
