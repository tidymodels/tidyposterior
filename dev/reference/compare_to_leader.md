# Compare Models to the Current Leader

The posterior distributions created by
[`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md)
can be used to ask how each candidate model compares to the best model
in the set (the "leader"). `compare_to_leader()` contrasts every model
against the leader and returns a data frame of probability statements
about those differences.

## Usage

``` r
compare_to_leader(x, ...)

# S3 method for class 'perf_mod'
compare_to_leader(
  x,
  leader = NULL,
  size = NULL,
  prob = 0.9,
  key = NULL,
  seed = sample.int(10000, 1),
  ...
)
```

## Arguments

- x:

  An object produced by
  [`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md).

- ...:

  Not currently used.

- leader:

  A single character string naming the model to use as the reference. If
  `NULL`, the model with the best posterior median is used.

- size:

  The size of an effective difference in the units of the metric (i.e.,
  the region of practical equivalence). When `NULL`, the ROPE statistics
  are not computed and `pr_worse` is the reported probability.

- prob:

  A number p (0 \< p \< 1) indicating the desired probability mass to
  include in the intervals.

- key:

  An optional data frame of display labels with a column of model names
  (called either `model` or `wflow_id`) and a character column called
  `label`. Every model in `x` must have a row in the key.
  [`initialize_keys()`](https://tidyposterior.tidymodels.org/dev/reference/initialize_keys.md)
  makes a template to edit.

- seed:

  A single integer for sampling from the posterior.

## Value

A tibble with one row per model and the extra class
`"compare_to_leader"`. The columns are:

- `model`: the model name, as a factor ordered from best to worst.

- `label`: the display label, ordered in the same way. This is the same
  as `model` unless `key` was given.

- `rank`: the integer rank of the model, where `1` is best.

- `leader`: a logical for whether the row is the reference model.

- `median`, `lower`, `upper`: the posterior median and credible interval
  for the metric.

- `mean_diff`, `lower_diff`, `upper_diff`: the posterior mean and
  credible interval for the difference `model - leader`.

- `pr_worse`: the probability that the model is worse than the leader.

- `size`: the value of the `size` argument (or `NA`).

- `pract_equiv`: the probability that the model is practically
  equivalent to the leader (or `NA` when `size` is `NULL`).

- `pract_worse`: the probability that the model is worse than the leader
  by more than `size` (or `NA` when `size` is `NULL`).

## Details

Ranking the models requires knowing whether the metric is better when
larger or smaller. This is taken from `x`, which records it when
[`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md)
is given a `tune_results` object or a workflow set. Fits made from a
data frame, an `rset` object, or a `caret` `resamples` object do not
carry a metric, so `compare_to_leader()` cannot be used on them.

Differences are parameterized as `model - leader` so that they describe
how much a candidate gives up relative to the current best. Since the
leader is, by definition, the best model in the set, `pr_worse` is the
informative one-sided probability. It accounts for the direction of the
metric: for metrics that are minimized (such as RMSE) it is the
posterior probability that the difference is positive, and for metrics
that are maximized (such as the area under the ROC curve) it is the
probability that the difference is negative.

The leader's own row describes a comparison against itself. That
difference is exactly zero, so `mean_diff`, `lower_diff`, `upper_diff`,
and `pr_worse` are `0`, `pract_equiv` is `1`, and `pract_worse` is `0`.

When `leader` names a model that is not the best in the set, the models
that beat it have posterior mass above `size` in the *better* direction.
That mass is not reported, so `pract_equiv` and `pract_worse` sum to
less than one for those rows. Their `pr_worse` values are still correct,
and `1 - pr_worse` is the probability that the model beats the leader.

If a transformation was used when `x` was created, the inverse is
applied before the summaries and differences are computed.

Which models compete is decided when `x` is fit, not here. For workflow
sets, `perf_mod(select_best = TRUE)` collapses workflows that fit the
same type of model with the same engine down to the best of each group,
which is useful when a workflow set contains several preprocessors for
the same model.

## See also

[`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md),
[`contrast_models()`](https://tidyposterior.tidymodels.org/dev/reference/contrast_models.md),
[`initialize_keys()`](https://tidyposterior.tidymodels.org/dev/reference/initialize_keys.md),
[`autoplot.compare_to_leader()`](https://tidyposterior.tidymodels.org/dev/reference/autoplot.compare_to_leader.md)

## Examples

``` r
library(parsnip)
library(rsample)
library(workflowsets)

set.seed(1)
folds <- vfold_cv(mtcars, v = 5)

# \donttest{
# A workflow set records the metric, so `compare_to_leader()` knows that
# RMSE is better when smaller:
mpg_models <-
  workflow_set(
    preproc = list(
      small = mpg ~ wt,
      medium = mpg ~ wt + hp,
      large = mpg ~ .
    ),
    models = list(lm = linear_reg())
  ) |>
  workflow_map("fit_resamples", resamples = folds, seed = 2)

set.seed(4321)
mpg_post <- perf_mod(mpg_models, metric = "rmse", refresh = 0, chains = 2)

compare_to_leader(mpg_post, seed = 2)
#> # Comparisons to the leader (medium_lm)
#> # A tibble: 3 × 14
#>   model     label   rank leader median lower upper mean_diff lower_diff
#>   <fct>     <fct>  <int> <lgl>   <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1 medium_lm mediu…     1 TRUE     2.70  2.00  3.42     0          0    
#> 2 small_lm  small…     2 FALSE    3.18  2.45  3.89     0.472     -0.452
#> 3 large_lm  large…     3 FALSE    4.43  3.72  5.12     1.72       0.761
#> # ℹ 5 more variables: upper_diff <dbl>, pr_worse <dbl>, size <dbl>,
#> #   pract_equiv <dbl>, pract_worse <dbl>

# A half mile per gallon is a meaningful difference here:
compare_to_leader(mpg_post, size = 0.5, seed = 2)
#> # Comparisons to the leader (medium_lm)
#> # A tibble: 3 × 14
#>   model     label   rank leader median lower upper mean_diff lower_diff
#>   <fct>     <fct>  <int> <lgl>   <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1 medium_lm mediu…     1 TRUE     2.70  2.00  3.42     0          0    
#> 2 small_lm  small…     2 FALSE    3.18  2.45  3.89     0.472     -0.452
#> 3 large_lm  large…     3 FALSE    4.43  3.72  5.12     1.72       0.761
#> # ℹ 5 more variables: upper_diff <dbl>, pr_worse <dbl>, size <dbl>,
#> #   pract_equiv <dbl>, pract_worse <dbl>
# }
```
