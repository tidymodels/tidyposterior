# Extract Posterior Distributions for Models

`tidy` can be used on an object produced by
[`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md)
to create a data frame with a column for the model name and the
posterior predictive distribution values.

## Usage

``` r
# S3 method for class 'perf_mod'
tidy(x, seed = sample.int(10000, 1), ...)
```

## Arguments

- x:

  An object from
  [`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md)

- seed:

  A single integer for sampling from the posterior.

- ...:

  Not currently used

## Value

A data frame with the additional class `"posterior"`

## Details

Note that this posterior only reflects the variability of the groups
(i.e. the fixed effects). This helps answer the question of which model
is best *for this data set*. If does not answer the question of which
model would be best on a new resample of the data (which would have
greater variability).
