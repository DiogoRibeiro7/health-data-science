# Bayesian Methods

The Bayesian helpers expose the sampling design and diagnostics instead of relying on hidden engine defaults.

`run_mcmc()` compiles Stan code and runs NUTS with explicit chain count, total iterations, warmup, thinning, seed, target acceptance probability, and maximum tree depth. The native `stanfit` return type is preserved and fitting metadata are attached.

`fit_hierarchical_bayes()` fits sampling-based `rstanarm::stan_glmer()` models and requires an explicit group-specific term in the formula. Iterations, warmup, chains, cores, seed, `adapt_delta`, maximum tree depth, QR decomposition, and missing-value handling are all visible in the API. Priors and other supported rstanarm arguments remain available through `...`.

`bayesian_sampling_diagnostics()` reports parameter-level R-hat and effective-sample-size flags together with divergent transitions and maximum-tree-depth hits. Diagnostic thresholds are screening rules for sampling quality, not evidence that the statistical model or prior specification is scientifically adequate.

`tidy_bayesian_posterior()` returns posterior means, medians, standard deviations, central credible intervals, effective sample sizes, and R-hat for `stanfit` or sampling-based `stanreg` models.

`posterior_predictive_check()` is restricted to `stanreg` posterior predictive checks against the observed fitted outcome. New predictor data belong in `posterior_predictive_draws()`, which calls `rstanarm::posterior_predict()`. Posterior predictive checks diagnose model-data discrepancies; they are not out-of-sample predictive validation.

`bayesian_model_averaging()` wraps `BMA::bic.glm()` with an explicit GLM family and model-search controls. `tidy_bayesian_model_average()` exposes posterior inclusion probabilities and model-averaged coefficient summaries. Inclusion probability is not a causal importance measure and depends on the candidate model space and prior/search assumptions.

These wrappers do not automatically choose priors, repair divergences, increase tree depth, remove weakly identified parameters, or declare convergence. Those decisions remain part of the modeling workflow.
