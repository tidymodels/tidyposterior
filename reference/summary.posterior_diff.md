# Summarize Posterior Distributions of Model Differences

Credible intervals are created for the differences. Also, region of
practical equivalence (ROPE) statistics are computed when the effective
size of a difference is given.

## Usage

``` r
# S3 method for class 'posterior_diff'
summary(object, prob = 0.9, size = 0, ...)
```

## Arguments

- object:

  An object produced by
  [`contrast_models()`](https://tidyposterior.tidymodels.org/reference/contrast_models.md).

- prob:

  A number p (0 \< p \< 1) indicating the desired probability mass to
  include in the intervals.

- size:

  The size of an effective difference in the units of the chosen metric.
  For example, a 5 percent increase in accuracy (`size = 0.05`) between
  two models might be considered a "real" difference.

- ...:

  Not currently used

## Value

A data frame with interval and ROPE statistics for each comparison.

## Details

The ROPE estimates included in the results are the columns `pract_neg`,
`pract_equiv`, and `pract_pos`. `pract_neg` integrates the portion of
the posterior below `-size` (and `pract_pos` is the upper integral
starting at `size`). The interpretation depends on whether the metric
being analyzed is better when larger or smaller. `pract_equiv`
integrates between `[-size, size]`. If this is close to one, the two
models are unlikely to be practically different relative to `size`.

## Examples

``` r
data("ex_objects")

summary(contrast_samples)
#> # A tibble: 2 × 9
#>   contrast          probability     mean   lower  upper  size pract_neg
#>   <chr>                   <dbl>    <dbl>   <dbl>  <dbl> <dbl>     <dbl>
#> 1 logistic_reg vs …       1      3.29e-2  0.0196 0.0462     0        NA
#> 2 logistic_reg vs …       0.472 -5.40e-4 -0.0142 0.0130     0        NA
#> # ℹ 2 more variables: pract_equiv <dbl>, pract_pos <dbl>
summary(contrast_samples, size = 0.025)
#> # A tibble: 2 × 9
#>   contrast          probability     mean   lower  upper  size pract_neg
#>   <chr>                   <dbl>    <dbl>   <dbl>  <dbl> <dbl>     <dbl>
#> 1 logistic_reg vs …       1      3.29e-2  0.0196 0.0462 0.025    0     
#> 2 logistic_reg vs …       0.472 -5.40e-4 -0.0142 0.0130 0.025    0.0036
#> # ℹ 2 more variables: pract_equiv <dbl>, pract_pos <dbl>
```
