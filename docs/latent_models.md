# Advanced Latent Models

The latent-model wrappers now make data shape, class count, and engine arguments explicit instead of relying on fragile positional calls.

`fit_latent_transition()` uses the current `LMest::lmest()` interface. Long-format data require explicit unit and time columns. For backward compatibility, a wide matrix or data frame with subjects in rows and response occasions in columns can still be supplied; it is converted internally to long format before fitting.

`fit_bayesian_lca()` targets binary latent class analysis through `BayesLCA::blca()`. Indicators are validated as 0/1 and the number of classes is passed through the package's current `G` argument.

`fit_multilevel_lca()` uses named arguments when calling `randomLCA::randomLCA()`, keeping pattern frequencies and the number of latent classes distinct. This avoids the historical bug where the class count was accidentally passed positionally as the `freq` argument.

`fit_mixture_covariates()` retains the native FlexMix model class while making missing-value handling, component count, seed, optional model driver, concomitant model, and control object explicit.

All four wrappers attach a `healthdatascience` metadata attribute that can be retrieved with `latent_model_metadata()`.

Latent-class and mixture solutions remain sensitive to initialization, weak identification, local optima, sparse response patterns, and the scientific meaning of the chosen class count. A fitted solution should not be interpreted solely from an information criterion or from the fact that an optimizer converged.
