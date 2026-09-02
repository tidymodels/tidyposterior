# Estimate the Difference Between Models

The posterior distributions created by
[`perf_mod()`](https://tidyposterior.tidymodels.org/reference/perf_mod.md)
can be used to obtain the posterior distribution of the difference(s)
between models. One or more comparisons can be computed at the same
time.

## Usage

``` r
contrast_models(x, list_1 = NULL, list_2 = NULL, seed = sample.int(10000, 1))
```

## Arguments

- x:

  An object produced by
  [`perf_mod()`](https://tidyposterior.tidymodels.org/reference/perf_mod.md).

- list_1, list_2:

  Character vectors of equal length that specify the specific pairwise
  contrasts. The contrast is parameterized as `list_1[i] - list_2[i]`.
  If the defaults are left to `NULL`, all combinations are evaluated.

- seed:

  A single integer for sampling from the posterior.

## Value

A data frame of the posterior distribution(s) of the difference(s). The
object has an extra class of `"posterior_diff"`.

## Details

If a transformation was used when `x` was created, the inverse is
applied *before* the difference is computed.
