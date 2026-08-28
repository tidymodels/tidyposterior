# Data-based starting values for Bayesian resampling models

`stan_glmer_inits()` computes starting values for the Bayesian
hierarchical models used by
[`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md)
from group-level sample statistics. The results can be passed to the
`init` argument of
[`rstanarm::stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html)
(directly or through `perf_mod(..., init = )`) and can substantially
shorten warmup for models that are slow to converge, especially those
with heterogeneous group variances.

## Usage

``` r
stan_glmer_inits(
  formula,
  data,
  seed = sample.int(10^5, 1),
  jitter = 0.1,
  blocks = c("fixed", "aux", "ranef")
)
```

## Arguments

- formula:

  A two-sided formula with a single numeric outcome, fixed effects, and
  at least one random effect term (in the
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) syntax used
  by
  [`rstanarm::stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html)).
  This should be the same formula that will be used to fit the model.
  See Details for the supported structures.

- data:

  A data frame with the columns used in `formula`; the same data that
  will be given to
  [`rstanarm::stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html).
  No missing values are allowed in the columns that are used.

- seed:

  A single integer controlling the per-chain random jittering.

- jitter:

  A single non-negative number for how much the starting values are
  perturbed for each chain. Location parameters are shifted by Gaussian
  noise with this standard deviation (on their natural, standardized
  scale) and scale parameters are multiplied by a log-normal factor with
  this standard deviation. A value of zero gives every chain identical
  starting values (not recommended for diagnostics).

- blocks:

  A character vector for which groups of parameters get data-based
  starting values: `"fixed"` (the intercept and fixed effect
  coefficients), `"aux"` (the residual standard deviation), and/or
  `"ranef"` (the random effect values and their covariance parameters).
  Parameters outside of `blocks` retain rstan's random initialization.

## Value

A function of `chain_id` (with class `"stan_glmer_inits"`) that returns
a named list of starting values for the model's underlying Stan
parameters. rstan invokes it once per chain. The unjittered values are
stored in the `"inits"` attribute.

## Details

The function is designed for the model structures created by
[`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md):


      statistic ~ model + (model + 0 | id)   # heterogeneous variances
      statistic ~ model + (1 | id)           # homogeneous variances
      statistic ~ model + (1 | id2/id)       # homogeneous, repeated CV

Other formulas with a fixed-effect intercept and factor-based random
effect terms may also work but are not tested. Note that the
heterogeneous-variance model with nested resamples
(`statistic ~ model + (model + 0 | id2/id)`) cannot be fit by
[`rstanarm::stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html)
for balanced resampling data because the number of random effects equals
the number of observations.

Starting values are estimated with ordinary least squares: the fixed
effects come from a linear model, the random effect values are per-level
summaries of its residuals, and their covariance matrix initializes the
correlation/variance decomposition used by rstanarm. The residual
standard deviation is estimated after removing additive grouping-factor
effects and is floored at 1\\ there is one observation per
model/resample combination, this residual scale is confounded with the
random effects, so the value is a heuristic.

The computations assume the model will be fit with `family = gaussian()`
(the
[`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md)
default), no case weights, `QR = FALSE`, and the default rstanarm priors
(which automatically rescale using the data). With non-default priors,
the starting values are still usable but are no longer on the intended
scale.

The **same** `formula` and `data` (after any outcome transformation)
must be given to this function and to
[`rstanarm::stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html);
otherwise the starting values may silently correspond to the wrong
random effect levels. The easiest way to use these starting values with
[`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md)
is its `initialize = TRUE` argument, which computes and applies them
automatically. To call this function manually (e.g., to control `blocks`
or `jitter`), recreate the internal data format of
[`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md):
the resampling results are stacked into a data frame with a `statistic`
column (with the `transform$func` applied), a `model` column, and the
resampling id column(s) (see the example below).

Data-based starting values do not uniformly reduce sampling time. For
models that already converge quickly they can add modest overhead, while
for slow-converging fits (e.g., `hetero_var = TRUE`) they can remove the
occasional very slow chain that started in a poor region of the
parameter space. They also tend to reduce the number of divergent
transitions, but divergences caused by the model geometry require
increasing `adapt_delta`, not better starting values.

## See also

[`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md),
[`rstanarm::stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html)

## Examples

``` r
data(precise_example)

# The data in the format used by perf_mod():
roc_data <-
  precise_example |>
  dplyr::select(id, dplyr::contains("ROC")) |>
  tidyr::pivot_longer(
    cols = c(-id),
    names_to = "model",
    values_to = "statistic"
  ) |>
  dplyr::mutate(model = gsub("_ROC", "", model))

inits <- stan_glmer_inits(
  statistic ~ model + (model + 0 | id),
  data = roc_data,
  seed = 1
)
inits
#> Starting values for `rstanarm::stan_glmer()`
#> formula: statistic ~ model + (model + 0 | id) 
#> initialized: fixed, aux, ranef 
#> random effect terms:
#>   id: 3 coefficient(s) x 10 levels
#> jitter: 0.1 with seed 1 

# The starting values for the second chain:
str(inits(2))
#> List of 8
#>  $ gamma       : num [1(1d)] 0.791
#>  $ z_beta      : num [1:2(1d)] -0.221 0.286
#>  $ aux_unscaled: num 0.151
#>  $ tau         : num [1(1d)] 1.22
#>  $ zeta        : num [1:3(1d)] 0.913 0.811 1.438
#>  $ rho         : num [1:2(1d)] 0.651 0.349
#>  $ z_T         : num [1:2(1d)] 0.326 -0.361
#>  $ z_b         : num [1:33(1d)] 0.984 -0.681 -0.353 -1.737 -0.452 ...

# \donttest{
fit <- rstanarm::stan_glmer(
  statistic ~ model + (model + 0 | id),
  data = roc_data,
  init = inits,
  refresh = 0,
  seed = 2
)
#> Warning: There were 14 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
# }
```
