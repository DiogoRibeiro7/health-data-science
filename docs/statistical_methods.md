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

## Bootstrap Confidence Intervals

Functions such as `bootstrap_ci()` draw repeated samples with replacement to
approximate confidence intervals. Results depend on the number of resamples and
the representativeness of the original sample.

