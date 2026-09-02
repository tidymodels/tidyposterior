# Summarize the Posterior Distributions of Model Statistics

Numerical summaries are created for each model including the posterior
mean and upper and lower credible intervals (aka uncertainty intervals).

## Usage

``` r
# S3 method for class 'posterior'
summary(object, prob = 0.9, seed = sample.int(10000, 1), ...)
```

## Arguments

- object:

  An object produced by
  [`tidy.perf_mod()`](https://tidyposterior.tidymodels.org/reference/tidy.perf_mod.md).

- prob:

  A number p (0 \< p \< 1) indicating the desired probability mass to
  include in the intervals.

- seed:

  A single integer for sampling from the posterior.

- ...:

  Not currently used

## Value

A data frame with summary statistics and a row for each model.

## Examples

``` r
data("ex_objects")

summary(posterior_samples)
#> # A tibble: 3 × 4
#>   model         mean lower upper
#>   <chr>        <dbl> <dbl> <dbl>
#> 1 cart         0.851 0.826 0.875
#> 2 logistic_reg 0.883 0.859 0.909
#> 3 mars         0.884 0.859 0.910
```
