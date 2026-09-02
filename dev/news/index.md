# Changelog

## tidyposterior (development version)

## tidyposterior 1.1.0

- New
  [`stan_glmer_inits()`](https://tidyposterior.tidymodels.org/dev/reference/stan_glmer_inits.md)
  computes data-based starting values for the Bayesian models used by
  [`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md).
  The results can be passed to
  [`rstanarm::stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html)
  (directly or via `perf_mod(..., init = )`) and can shorten warmup for
  models that are slow to converge, such as those with heterogeneous
  variances.

- [`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md)
  gains an `initialize` argument (default `FALSE`) that computes and
  applies these starting values automatically, reusing the sampler
  `seed` (when one is passed) so that the entire fit is reproducible.

- Fixed a bug where
  [`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md)
  duplicated the resampling statistics for workflow sets: when more than
  one metric had been collected, each statistic was included in the
  Bayesian model once per metric.

- New
  [`compare_to_leader()`](https://tidyposterior.tidymodels.org/dev/reference/compare_to_leader.md)
  contrasts every model in a
  [`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md)
  object against the best model in the set and returns a data frame of
  probability statements about how much each candidate gives up relative
  to that leader.
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  on the result draws the posterior metric intervals alongside those
  probabilities.

- New
  [`initialize_keys()`](https://tidyposterior.tidymodels.org/dev/reference/initialize_keys.md)
  builds a template of model labels to edit and pass to the `key`
  argument of
  [`compare_to_leader()`](https://tidyposterior.tidymodels.org/dev/reference/compare_to_leader.md).

- [`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md)
  gains a `select_best` argument for workflow sets. Workflows are always
  reduced to their own best tuning parameter candidate;
  `select_best = TRUE` additionally collapses workflows that fit the
  same type of model with the same engine down to the best of each
  group, so that each model type/engine combination competes once. The
  default, `FALSE`, keeps the existing behavior of one competitor per
  `wflow_id`.

## tidyposterior 1.0.2

CRAN release: 2025-07-31

- Maintenance release for an upcoming ggplot2 release
  ([\#74](https://github.com/tidymodels/tidyposterior/issues/74)).

- Transition from the magrittr pipe to the base R pipe.

## tidyposterior 1.0.1

CRAN release: 2023-10-11

Maintenance release for a broken test in current R-devel.

Maintainer email change.

## tidyposterior 1.0.0

CRAN release: 2022-06-23

- Transition from `gather()` to `pivot_longer()`.

- Update to testthat 3e.

## tidyposterior 0.1.0

CRAN release: 2021-03-25

- The [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  methods are now removed in factor of
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  methods.

- [`perf_mod()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md)
  methods added for tuning parameter objects from `tune`, `finetune`,
  and `workflowsets`.

- `rstanarm` version 2.21.1 or higher is now required due to changes to
  their APIs.

- Re-licensed package from GPL-2 to MIT. All copyright holders are
  RStudio employees and give consent.

## `tidyposterior` 0.0.3

CRAN release: 2020-06-11

- [`contrast_models()`](https://tidyposterior.tidymodels.org/dev/reference/contrast_models.md)
  now returns a tibble and has an extra column called `contrast`.

- The plot methods are now deprecated and will be removed in the next
  version. They are not very good and can be replaced with simple
  `ggplot` code.

- An optional formula argument was added to
  [`perf_mod.rset()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md)
  and
  [`perf_mod.data.frame()`](https://tidyposterior.tidymodels.org/dev/reference/perf_mod.md).
  When the resampling method has multiple ID columns, a nested data
  structure is assumed (with a warning). The new `formula` argument can
  be used to over-ride the nesting.

### Breaking Changes

- Methods for compatibility with `dplyr` 1.0.0. For the newer `dplyr`
  version, if critical columns for `posterior` or `posterior_diff`
  objects are removed, the objects is down-graded to a tibble. For
  earlier versions of `dplyr`, the object is not down-cast.

## `tidyposterior` 0.0.2

CRAN release: 2018-11-15

A small, maintenance release.

### Minor bug fixes and improvements

- Moved from the `broom` package to the `generics` package to get the
  `tidy` generic.

- `ggplot2` was moved to Suggests

- The sole `tidy` method was more explicitly exported so that the
  `generics` man files show the method.

- The large RData objects containing the examples have been removed from
  the package and are accessible via a link to the GitHub repo.

## `tidyposterior` 0.0.1

CRAN release: 2017-11-14

- First CRAN submission
