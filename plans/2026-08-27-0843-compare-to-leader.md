# `compare_to_leader()` and its `autoplot()` method

## Overview

Refactor the prototype in `~/tmp/leader_plot.R` into package code. The prototype takes a workflow set, fits a `perf_mod()` internally, contrasts every model against the best-performing one, and draws a two-panel plot with `patchwork`. It is a single monolithic function with a dozen `TODO`s.

The refactored version splits this into three pieces:

1. `compare_to_leader()` — an S3 generic with a method for objects produced by `perf_mod()`. It does **not** fit the Bayesian model; the caller passes an already-fitted `perf_mod` (or `perf_mod_workflow_set`) object.
2. A classed tibble of results (class `"compare_to_leader"`) carrying the metric, direction, leader, ROPE size, and interval probability as attributes.
3. `autoplot.compare_to_leader()` — the two-panel plot, built with `patchwork` at 4:1 widths.

The "leader" is the model with the best posterior median for the metric (or a model named explicitly by the user). Every other model is contrasted against it as `candidate - leader`, so the results answer "how much worse than the current best is this model, and how sure are we?".

## Decisions

Confirmed with the maintainer before starting:

- **Plot layout**: add `patchwork` to `Imports` and keep the prototype's side-by-side panels at `widths = c(4, 1)`. `autoplot()` returns a patchwork object.
- **One-sided probability** when `size` is `NULL`: report `pr_worse`, the probability that a model is *worse* than the leader. A candidate cannot be better than the leader by construction, so "probability of being worse" is the informative direction. The value is direction-aware: for a `"minimize"` metric it is `Pr[candidate - leader > 0]`, for `"maximize"` it is `Pr[candidate - leader < 0]`.
- **Class machinery**: light. Class plus a `print()` method and attributes; no `vctrs`/`dplyr` compat suite like `posterior` and `posterior_diff` have. `dplyr` verbs will down-cast the result to a plain tibble, which is fine for a terminal summary table. `autoplot()` degrades gracefully when the attributes have been stripped.

## Prototype TODOs and how each is resolved

| Prototype `TODO` | Resolution |
| --- | --- |
| `set` should take a pre-fitted `perf_mod()` | The method dispatches on `perf_mod`; the internal `perf_mod()` call is deleted. |
| `equiv = 0.01` should default to `NULL` and pick a one-sided probability | Renamed `size` (matching `summary.posterior_diff()`); `NULL` means no ROPE, and `pr_worse` is the reported probability. |
| `current = "$."` should set the leader or determine it from data | Renamed `leader`; `NULL` picks the best posterior median, a string names the reference model. The regex-exclusion behaviour is dropped. |
| `key = NULL` — a data frame of `wflow_id`s and labels | Kept as `key`; a two-column data frame mapping model names to display labels. It replaces the prototype's `extract_workflow()`/`extract_spec_parsnip()` class/engine extraction, which is impossible here because the fitted `perf_mod` does not retain the original workflow set. `initialize_keys()` builds a template so the caller never has to retype the model names. |
| Parameterize the hard-coded `0.05`/`0.95` quantiles | `prob` argument, default `0.90`, matching `summary.posterior()` and `summary.posterior_diff()`. |
| `metric = "rmse"` should default to `NULL` | Gone entirely. Both the metric and its direction are read from the fitted object, so there is no `metric` or `direction` argument. |
| Add a class to the result data frame | Class `"compare_to_leader"` on a tibble. |
| Make `autoplot()` | `autoplot.compare_to_leader()`. |
| `slice(1, .by = c(class, engine))` "there is more nuance here" | Dropped entirely. De-duplication was only needed because labels were derived from the model class/engine; with `key` the caller controls labels and row identity is the model name. |

## Result columns

One row per model, ordered by `rank`.

| Column | Meaning |
| --- | --- |
| `model` | Model name as it appears in the `perf_mod` object; a factor ordered best to worst |
| `label` | Display label, a factor with the same ordering; equals `model` unless `key` is supplied |
| `rank` | Integer rank by posterior median, `1` is best |
| `leader` | Logical, `TRUE` for the reference model |
| `median` | Posterior median of the metric |
| `lower`, `upper` | Credible interval bounds for the metric at `prob` |
| `mean_diff` | Posterior mean of `model - leader` |
| `lower_diff`, `upper_diff` | Credible interval bounds for that difference at `prob` |
| `pr_worse` | Probability the model is worse than the leader (direction-aware) |
| `size` | The ROPE size, `NA` when `size` is `NULL` |
| `pract_equiv` | Probability of practical equivalence to the leader, `NA` when `size` is `NULL` |
| `pract_worse` | Probability of being worse than the leader by more than `size`, `NA` when `size` is `NULL` |

The leader's own row is filled with the exact degenerate values for a comparison against itself: `mean_diff`, `lower_diff`, `upper_diff` and `pr_worse` are `0`, `pract_equiv` is `1`, and `pract_worse` is `0`. These are correct rather than imputed, and they keep the leader visible in the right-hand plot panel.

## Design notes

- **Metric direction.** Taken from `x$metric$direction` only; there is no `direction` argument. `perf_mod.tune_results()` and `perf_mod.workflow_set()` record it, so those are the supported inputs. `perf_mod.rset()`, `perf_mod.data.frame()`, and `perf_mod.resamples()` store `NA_character_`, and `compare_to_leader()` errors on those with a message naming the two input types that do carry a metric. This matches the existing precedent in `plot_wset_intervals()`, which refuses to rank a metric it can't orient. A consequence worth remembering: the package's own example data sets (`precise_example` and friends) go through the data frame method, so the help page examples build a small workflow set instead.
- **No contrast-string parsing.** The prototype recovered model names with `gsub(paste(best_id, "vs "), "", contrast)`. Instead, join `summary.posterior_diff()` output back to the distinct `model_1`/`model_2` pairs taken from the `posterior_diff` object, so model names containing `" vs "` cannot break the mapping.
- **`pr_worse` from `probability`.** `summary.posterior_diff()` already returns `probability = Pr[difference > 0]`. `pr_worse` is that value for `"minimize"` metrics and its complement for `"maximize"`, so the raw posterior is not re-summarized.
- **Plot orientation.** Factor levels run best-first so the tibble reads naturally. A discrete ggplot2 axis places the first level at the bottom, so the plot runs from the worst model at the top down to the leader at the bottom, with no scale reversal needed. This matches the prototype and puts the leader closest to the x-axis labels.
- **`initialize_keys()`.** The awkward part of writing a `key` by hand is that the `model` column has to reproduce `x$names` exactly, and every model needs a row, or `compare_to_leader()` errors. `initialize_keys(x)` returns `tibble(model = x$names, label = x$names)`, so an unedited template is already valid and only the `label` column needs touching. The three `key` validation errors name the helper, so it is discoverable at the moment it is needed. It is a plain function rather than a generic since it takes one input type; converting it to a generic later would not be a breaking change.
- **Fill scale.** Dark means "good" in both modes: `scale_fill_viridis_c(direction = -1)` for `pract_equiv` (high is good) and `direction = 1` for `pr_worse` (low is good).
- **Axis labelling.** The left-hand panel is labelled with `attr(object, "metric")`, falling back to `"Posterior"` when the attribute is gone (a `dplyr` verb on the light class drops it). `autoplot()` takes a `metric_label` argument that overrides both, so a bare metric name like `rmse` can be written out with units, e.g. `"RMSE (miles per gallon)"`. Expressions are accepted as well as strings, since metrics such as R squared want `expression(R^2)`. The right-hand panel's label stays derived from which probability is being shown.
- **Zero-probability bars.** A probability of zero draws a bar with no length, which reads as a missing row. This is not an edge case: the leader's `pr_worse` is always exactly zero, so in that mode the most important row is the one that disappears. `autoplot()` gains a `zero_bar` argument (default `0.01`) that floors the plotted bar length, carrying over the prototype's `ifelse(pract_equiv < 0.01, 0.01, pract_equiv)`. Two refinements: the fill stays mapped to the *true* value so the colour never lies, and floored bars get a thin `grey30` outline. Without the outline a floored bar is only visible when its fill happens to be dark, so `pr_worse` zeros showed but `pract_equiv` zeros (pale yellow at zero) vanished into the panel background. The outline is applied only to the floored bars, via a `.outline` column of `"grey30"`/`NA` and `scale_colour_identity()`, since outlining every bar is visually noisy. `zero_bar = 0` draws the probabilities exactly and outlines nothing. The departure between bar length and value is documented on the help page.
- **Errors** use `rlang::abort()` with bullet vectors, matching the rest of the package, rather than adding a `cli` dependency.

## Work Items

### Implementation

- [x] Add `patchwork` to `Imports` in `DESCRIPTION`
- [x] Create `R/compare_to_leader.R` with the `compare_to_leader()` generic and a `default` method that errors informatively
- [x] Implement `compare_to_leader.perf_mod()` (leader selection, ranking, contrasts, ROPE/one-sided probabilities, `key` labels, attributes, class)
- [x] Add argument-validation helpers (`prob`, `size`, `leader`, `key`) and read the metric direction from the object
- [x] Implement `print.compare_to_leader()`
- [x] Implement `autoplot.compare_to_leader()` with the two patchwork panels
- [x] Register `autoplot` for the new class in `.onLoad()` (`R/zzz.R`) alongside the existing registrations

### Package plumbing

- [x] Add new column names to `utils::globalVariables()` in `R/tidyposterior-package.R`
- [x] Run `devtools::document()` to regenerate `NAMESPACE` and `man/`
- [x] Add `compare_to_leader` to the `_pkgdown.yml` reference index
- [x] Add a `NEWS.md` bullet
- [x] Add `plans` to `.Rbuildignore` so `R CMD check` stops flagging it as a non-standard top-level directory

### Tests

- [x] `tests/testthat/test_compare_to_leader.R`: results from an `rset`-based `perf_mod` with the metric recorded on the object
- [x] Test leader selection for both `"minimize"` and `"maximize"`, and an explicitly named `leader`
- [x] Test `size = NULL` (`pr_worse` populated, `pract_*` all `NA`) versus a supplied `size`
- [x] Test `key` labelling, including the error when a model is missing from the key
- [x] Snapshot tests for all validation errors
- [x] Test that `autoplot()` returns a patchwork/ggplot object
- [x] Exercise a real `workflow_set` so the stored metric name and direction are picked up
- [x] Run `devtools::check()` (or at least `R CMD check` on the new code paths)

## Outcome

`devtools::test()`: 285 passing, 0 failures. `devtools::check()`: 0 errors, and no new warnings or notes (the two vignette warnings come from checking with `--no-build-vignettes`, and the `plans` note is resolved by the `.Rbuildignore` entry). Examples pass with `--run-donttest` in 28 seconds.

The workflow set path was verified interactively rather than in the test suite: a four-workflow `workflow_set()` over `modeldata::two_class_dat` fit with `perf_mod(metric = "roc_auc")`. `compare_to_leader()` picked up `roc_auc`/`maximize` from the object, ranked `simple_glm` first, and gave `full_glm` a `pract_equiv` of 0.95 against it while both tree workflows came in at 0. The test file covers the same code path more cheaply by setting `$metric` on an `rset`-based fit, which avoids adding `modeldata` to `Suggests` and roughly a minute of `fit_resamples()` to every test run. The help page examples do build a real workflow set, under `\donttest{}`.

Verified against the prototype's semantics:

- `mean_diff` matches `contrast_models()` output computed directly for the same contrasts and seed.
- `pr_worse` matches `mean(difference < 0)` computed from the raw posterior for a maximized metric.
- Repeated calls with the same `seed` return identical results.

## Phase 2: choosing which workflows compete

The prototype's plot collapsed competitors with `slice(1, .by = c(class, engine))`, keeping only the best workflow for each model type/engine combination. That was dropped in the refactor because a fitted `perf_mod` object does not retain the workflow set, so the model class and engine are no longer reachable from `compare_to_leader()`.

The collapse therefore has to happen while the workflow set is still in hand, which means `perf_mod.workflow_set()`. A new `select_best` argument controls it.

There are three granularities a workflow set could compete at:

| Level | Competitor identity | Count for the prototype's set | Status |
| --- | --- | --- | --- |
| 1 | model type + engine | 3 | `select_best = TRUE` |
| 2 | + `wflow_id` | 4 | `select_best = FALSE` (default, existing behavior) |
| 3 | + `.config` | 52 | not offered; this is what `perf_mod.tune_results()` already does |

Level 3 was explicitly ruled out: entries stay keyed by `wflow_id`, and tuning candidates are always reduced to each workflow's best, exactly as before.

### Design notes

- `select_best` is a filter over workflows, not a renaming. The surviving workflow keeps its own `wflow_id` as the model name, which is what the prototype's plot displayed after its `slice()`. This also means the caller can still see which preprocessor won.
- No new dependency. `workflowsets::extract_workflow()` and `tune::extract_spec_parsnip()` are both in packages already in `Imports`, and both work on an unfitted workflow set. `rank_results()` reports the model type but not the engine, so the spec still has to be extracted.
- The single `rank_results(select_best = TRUE)` call the method already made is now filtered to the chosen metric and reused for both purposes, so no extra ranking pass is needed.
- **Naming caution**: the method internally calls `workflowsets::rank_results(select_best = TRUE)` to reduce each workflow to its best tuning candidate. That is a different axis from the new user-facing `select_best` argument. A comment in the source calls this out; renaming the argument (`best_per_engine`, say) would remove the ambiguity if it proves confusing. Filed as tidymodels/tidyposterior#83.

### Work Items

- [x] Add the `select_best` argument to `perf_mod.workflow_set()` with a `rlang::is_bool()` check
- [x] Add `best_by_engine()`, which extracts each workflow's spec class and engine and keeps the best `wflow_id` per group for the metric's direction
- [x] Filter the existing `ranked` table so the change flows through the join that was already there
- [x] Document `select_best` on `perf_mod()` and cross-reference it from `compare_to_leader()`
- [x] Add `model_type` and `engine` to `utils::globalVariables()`
- [x] Add a `NEWS.md` bullet
- [x] Unit tests for `best_by_engine()` using an unfitted workflow set and synthetic ranks, covering both metric directions and the case where no workflows share an engine
- [x] Integration test that `perf_mod(select_best = )` changes `$names` on a fitted workflow set
- [x] Snapshot tests for the `select_best` validation errors
- [x] Re-run `devtools::test()` and `devtools::check()`

### Phase 2 outcome

`devtools::test()`: 295 passing, 0 failures. `devtools::check()`: 0 errors, 0 notes (only the two vignette warnings caused by checking with `--no-build-vignettes`).

Verified on a workflow set of two preprocessors crossed with `linear_reg()` and `null_model()`, so each model type/engine combination appears twice. `select_best = FALSE` gives `a_lm`, `a_null`, `b_lm`, `b_null`; `select_best = TRUE` gives `b_lm` (the better linear regression for RMSE) and `a_null`. Reversing the direction to `"maximize"` flips the selection to `a_lm`, confirming the metric direction is honored rather than assumed.

## Out of scope

- `plot_rope_probs()` in `R/posteriors.R` calls `sample.int(1, 1000)` with its arguments transposed, so the contrast seed is always `1`. It is an existing bug in adjacent code, left unfixed here and filed as tidymodels/tidyposterior#82.
