# Statistical Methods

The toolkit implements several statistical techniques commonly used in health
data science. Understanding their assumptions is essential for valid inference.

## Latent Class Analysis (LCA)

LCA identifies unobserved subgroups within a population by modeling the joint
distribution of categorical variables. Key assumptions include:

* **Local Independence** – observed indicators are independent given class
  membership.
* **Measurement Invariance** – class-specific response probabilities are
  constant across subpopulations.
* **Sufficient Sample Size** – each latent class must contain enough cases for
  stable estimation.

The package uses the `poLCA` implementation. Convergence is monitored and
warnings are emitted when the maximum number of iterations is reached.

## Regression Models

Regression utilities support generalized linear models and report standard
diagnostics. Assumptions vary by model family but generally include correct
specification, independence of observations, and homoscedastic residuals.

## Competing Risks

`fit_competing_risks()` fits a Fine-Gray subdistribution hazards model through
`cmprsk::crr()`. The event of interest and censoring codes are explicit, so the
input status variable does not need to follow a fixed `0/1/2` convention.

The model targets the cumulative incidence of a specified event in the presence
of competing events. A subdistribution hazard ratio therefore has a different
interpretation from a cause-specific Cox hazard ratio: it describes association
with the subdistribution hazard used to model cumulative incidence, not the
instantaneous cause-specific event rate among subjects who remain event-free.

The wrapper validates follow-up times, event codes, dimensions, numeric design
matrices, and missing values before calling `cmprsk`. Categorical predictors
should be expanded first with `stats::model.matrix()`. Missing values fail by
default and can instead be removed explicitly with `na_action = "omit"`.

Each fitted model receives a `healthdatascience` attribute containing the event
and censoring codes, observed competing-event codes, fitted sample size, event
counts, omitted-row count, and covariate names. `tidy_competing_risks()` converts
the fitted object into a compact Wald summary containing coefficients, standard
errors, z statistics, p-values, subdistribution hazard ratios, and confidence
intervals.

Fine-Gray models should not be used automatically whenever competing events are
present. Choice between cumulative-incidence modelling and cause-specific hazard
modelling depends on the scientific estimand. The event definition, competing
events, censoring mechanism, follow-up horizon, and covariate coding should be
specified before fitting the model.

## Bootstrap Confidence Intervals

Functions such as `bootstrap_ci()` draw repeated samples with replacement to
approximate confidence intervals. Results depend on the number of resamples and
the representativeness of the original sample.

