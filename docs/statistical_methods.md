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

## Recurrent Events

`fit_recurrent_events()` fits recurrent-event Cox models to start-stop
counting-process data and uses subject-level clustering to obtain robust standard
errors for repeated events within a subject.

Two models are available:

* **Andersen-Gill** treats recurrent events as repeated realizations of a common
  counting process with a shared baseline hazard. Subjects re-enter the risk set
  after an event and correlation between repeated event intervals is handled by
  clustering on subject identifier.
* **PWP total-time** conditions the risk set on event order and stratifies the
  baseline hazard by recurrence number. The user must provide an explicit
  positive-integer event-order variable. This is appropriate when the hazard of
  a second or third event is scientifically distinct from the hazard of the
  first event.

Intervals must satisfy `stop > start` and cannot overlap within a subject. Event
indicators are binary. Missingness is explicit: the default is to fail and the
alternative `na_action = "omit"` removes incomplete model rows before fitting.
Covariates may be numeric, logical, or factor variables.

The returned `coxph` object carries a `healthdatascience` attribute with the
model type, fitted rows, number of subjects, recurrent-event count, omitted-row
count, column mappings, event-order variable where relevant, and covariate list.
`tidy_recurrent_events()` returns Wald coefficient summaries, robust standard
errors, hazard ratios, and confidence intervals.

Andersen-Gill and PWP answer different questions. Andersen-Gill estimates a
common multiplicative effect across recurrent events under a common baseline
process. PWP total-time compares subjects within the same event-order stratum and
therefore conditions interpretation on previous recurrence history. The choice
between them should be made from the scientific risk-set definition rather than
by fit statistics alone.

## Multi-State Models

Multi-state analysis represents disease or care trajectories as transitions
between discrete states. `validate_transition_structure()` defines the directed
graph of allowed state changes and rejects self-transitions and duplicate edges.
Observed event rows are checked against that graph, and contiguous subject
intervals must preserve the previous state history.

`fit_multistate_cox()` fits one **cause-specific Cox model per allowed
transition**. For a transition such as `A -> B`, subjects contribute risk time
while they occupy state `A`; observed exits from `A` to another state are treated
as competing events and are censored at that exit time for the `A -> B` hazard.
Subject-level clustering is used for robust covariance estimation. The returned
`hds_multistate_cox` object records transition-specific event counts and the set
of transitions that were estimable from the observed data.

`tidy_multistate_cox()` reports transition identifiers, robust standard errors,
Wald tests, and **cause-specific hazard ratios**. These coefficients describe
instantaneous transition hazards conditional on occupying the origin state. They
should not be interpreted as direct effects on state probabilities.

`estimate_state_occupation()` estimates **Aalen-Johansen state-occupation
probabilities** nonparametrically from the observed counting-process data. It
constructs empirical transition-hazard increments at event times and accumulates
them through the product-integral transition matrix. The resulting probabilities
answer a different question from the Cox models: they describe the estimated
probability of occupying each state over time, not a covariate effect on a
transition hazard.

For the Aalen-Johansen estimator, every subject must have exactly one observed
state at the chosen origin time. The implementation returns point estimates only;
confidence bands and covariate-adjusted transition probabilities are separate
extensions rather than being silently inferred from the transition-specific Cox
models.

## Bootstrap Confidence Intervals

Functions such as `bootstrap_ci()` draw repeated samples with replacement to
approximate confidence intervals. Results depend on the number of resamples and
the representativeness of the original sample.
