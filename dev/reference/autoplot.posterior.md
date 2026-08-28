# Visualize the Posterior Distributions of Model Statistics

For objects of classes `posterior` and `perf_mod`,
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
produces a simple plot of posterior distributions. For workflow set
objects, there are several types of plots that can be produced.

## Usage

``` r
# S3 method for class 'posterior'
autoplot(object, ...)

# S3 method for class 'perf_mod'
autoplot(object, ...)

# S3 method for class 'perf_mod_workflow_set'
autoplot(object, type = "intervals", prob = 0.9, size = NULL, ...)
```

## Arguments

- object:

  An object produced by
  [`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md),
  [`tidy.perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/tidy.perf_mod.md),
  or a workflow set with computed results.

- ...:

  Options passed to `geom_line(stat = "density", ...)`.

- type:

  A value of one of: `"intervals"` (for model rank versus posterior
  probability using interval estimation), `"posteriors"` (density plots
  for each model), or `"ROPE"` (for practical equivalence probabilities
  versus workflow rank).

- prob:

  A number p (0 \< p \< 1) indicating the desired probability mass to
  include in the intervals.

- size:

  The size of an effective difference in the units of the chosen metric.
  For example, a 5 percent increase in accuracy (`size = 0.05`) between
  two models might be considered a "real" difference.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
data(ex_objects)
autoplot(posterior_samples)
```
