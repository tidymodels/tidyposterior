library(rsample)
library(testthat)

## run fits outside of test functions
## https://github.com/stan-dev/rstanarm/issues/202

set.seed(4633)
test_bt <- bootstraps(mtcars, times = 10)
test_bt$one <- rnorm(nrow(test_bt), mean = 10)
test_bt$two <- rnorm(nrow(test_bt), mean = 12)
test_bt$three <- rnorm(nrow(test_bt), mean = 14)

fit_bt <- suppressWarnings(
  perf_mod(
    test_bt,
    seed = 781,
    chains = 2,
    iter = 1000,
    refresh = 0,
    verbose = FALSE
  )
)

## `compare_to_leader()` takes the metric direction from the object, and only
## the `tune_results` and workflow set methods of `perf_mod()` record one.
## Emulating that here keeps these tests off a slow model fit; a real workflow
## set is exercised in test_perf_mod.R.
fit_min <- fit_bt
fit_min$metric <- list(name = "rmse", direction = "minimize")

fit_max <- fit_bt
fit_max$metric <- list(name = "roc_auc", direction = "maximize")

res_min <- compare_to_leader(fit_min, seed = 2)
res_max <- compare_to_leader(fit_max, seed = 2)
res_size <- compare_to_leader(fit_max, size = 1, seed = 2)

col_names <- c(
  "model",
  "label",
  "rank",
  "leader",
  "median",
  "lower",
  "upper",
  "mean_diff",
  "lower_diff",
  "upper_diff",
  "pr_worse",
  "size",
  "pract_equiv",
  "pract_worse"
)

# ------------------------------------------------------------------------------

test_that("basic object", {
  expect_s3_class(res_max, "compare_to_leader")
  expect_s3_class(res_max, "tbl_df")
  expect_named(res_max, col_names)
  expect_equal(nrow(res_max), 3)
  expect_equal(res_max$rank, 1:3)
  expect_equal(attr(res_max, "direction"), "maximize")
  expect_equal(attr(res_max, "prob"), 0.90)
  expect_identical(attr(res_max, "size"), NA_real_)
})

test_that("reproducibility", {
  expect_equal(
    compare_to_leader(fit_max, seed = 2),
    res_max
  )
})

# ------------------------------------------------------------------------------

test_that("the leader is the best model for the metric direction", {
  # `three` has the largest mean and `one` the smallest
  expect_equal(as.character(res_max$model), c("three", "two", "one"))
  expect_equal(attr(res_max, "leader"), "three")
  expect_equal(res_max$leader, c(TRUE, FALSE, FALSE))

  expect_equal(as.character(res_min$model), c("one", "two", "three"))
  expect_equal(attr(res_min, "leader"), "one")
  expect_equal(res_min$leader, c(TRUE, FALSE, FALSE))
})

test_that("the metric and its direction come from the perf_mod object", {
  expect_equal(attr(res_min, "metric"), "rmse")
  expect_equal(attr(res_min, "direction"), "minimize")
  expect_equal(attr(res_max, "metric"), "roc_auc")
  expect_equal(attr(res_max, "direction"), "maximize")
})

test_that("the leader can be set explicitly", {
  res <- compare_to_leader(fit_max, leader = "two", seed = 2)
  expect_equal(attr(res, "leader"), "two")
  expect_equal(res$leader, c(FALSE, TRUE, FALSE))
  # the ranking is still by performance, not by the leader
  expect_equal(as.character(res$model), c("three", "two", "one"))
  # `three` beats the named leader. It is not the leader itself, so this is a
  # Monte Carlo estimate rather than an assigned value.
  expect_equal(res$pr_worse[1], 0, tolerance = 0.01)
})

# ------------------------------------------------------------------------------

test_that("the leader row holds the degenerate comparison against itself", {
  # these are assigned rather than estimated, so they are exact
  expect_equal(res_max$mean_diff[1], 0)
  expect_equal(res_max$lower_diff[1], 0)
  expect_equal(res_max$upper_diff[1], 0)
  expect_equal(res_max$pr_worse[1], 0)

  expect_equal(res_size$pract_equiv[1], 1)
  expect_equal(res_size$pract_worse[1], 0)
})

test_that("differences are parameterized as `model - leader`", {
  diffs <- contrast_models(
    fit_bt,
    list_1 = c("two", "one"),
    list_2 = c("three", "three"),
    seed = 2
  )
  expect_equal(
    res_max$mean_diff[-1],
    as.vector(tapply(diffs$difference, diffs$model_1, mean)[c("two", "one")])
  )
})

test_that("`pr_worse` accounts for the metric direction", {
  diffs <- contrast_models(
    fit_bt,
    list_1 = c("two", "one"),
    list_2 = c("three", "three"),
    seed = 2
  )
  # `three` is the leader when maximizing, so the others are worse when the
  # difference is negative
  expect_equal(
    res_max$pr_worse[-1],
    as.vector(
      tapply(diffs$difference, diffs$model_1, function(x) mean(x < 0))[c(
        "two",
        "one"
      )]
    )
  )
  # Every model is clearly separated in this example. The leader's 0 is
  # assigned, but the other two are proportions of the posterior draws, so a
  # single draw landing the other way makes them 0.999 rather than 1.
  expect_equal(res_max$pr_worse, c(0, 1, 1), tolerance = 0.01)
  expect_equal(res_min$pr_worse, c(0, 1, 1), tolerance = 0.01)
})

# ------------------------------------------------------------------------------

test_that("ROPE statistics are only computed when `size` is given", {
  expect_true(all(is.na(res_max$size)))
  expect_true(all(is.na(res_max$pract_equiv)))
  expect_true(all(is.na(res_max$pract_worse)))

  expect_equal(res_size$size, rep(1, 3))
  expect_false(any(is.na(res_size$pract_equiv)))
  expect_false(any(is.na(res_size$pract_worse)))
  expect_equal(attr(res_size, "size"), 1)
})

test_that("`prob` sets the width of the intervals", {
  narrow <- compare_to_leader(fit_max, prob = 0.5, seed = 2)
  expect_true(all(narrow$upper - narrow$lower < res_max$upper - res_max$lower))
  expect_equal(attr(narrow, "prob"), 0.5)
})

# ------------------------------------------------------------------------------

test_that("`key` supplies plot labels", {
  key <- tibble::tibble(
    model = c("one", "two", "three"),
    label = c("first", "second", "third")
  )
  res <- compare_to_leader(fit_max, key = key, seed = 2)
  expect_equal(as.character(res$label), c("third", "second", "first"))
  expect_equal(levels(res$label), c("third", "second", "first"))

  # `wflow_id` is accepted in place of `model`
  res_wflow <- compare_to_leader(
    fit_max,
    key = dplyr::rename(key, wflow_id = model),
    seed = 2
  )
  expect_equal(res_wflow$label, res$label)
})

test_that("`label` matches `model` when no key is given", {
  expect_equal(as.character(res_max$label), as.character(res_max$model))
})

test_that("`initialize_keys()` makes a usable template", {
  keys <- initialize_keys(fit_max)
  expect_s3_class(keys, "tbl_df")
  expect_named(keys, c("model", "label"))
  expect_equal(keys$model, fit_max$names)
  expect_equal(keys$label, fit_max$names)
  expect_type(keys$label, "character")

  # an unedited template is accepted and changes nothing
  expect_equal(
    compare_to_leader(fit_max, key = keys, seed = 2),
    res_max
  )

  # and it is a template, so editing the labels is what it is for
  keys$label <- c("first", "second", "third")
  edited <- compare_to_leader(fit_max, key = keys, seed = 2)
  expect_equal(
    as.character(edited$label),
    keys$label[match(as.character(edited$model), keys$model)]
  )

  # row order in the key does not matter
  shuffled <- compare_to_leader(fit_max, key = keys[c(3, 1, 2), ], seed = 2)
  expect_equal(shuffled$label, edited$label)
})

test_that("`initialize_keys()` needs a perf_mod object", {
  expect_snapshot(error = TRUE, initialize_keys(mtcars))
  expect_snapshot(error = TRUE, initialize_keys("nope"))
})

# ------------------------------------------------------------------------------

test_that("bad arguments", {
  expect_snapshot(error = TRUE, compare_to_leader(mtcars))
  expect_snapshot(error = TRUE, compare_to_leader(fit_bt))
  bad_dir <- fit_bt
  bad_dir$metric <- list(name = "rmse", direction = "up")
  expect_snapshot(error = TRUE, compare_to_leader(bad_dir))
  expect_snapshot(
    error = TRUE,
    compare_to_leader(fit_max, prob = 2)
  )
  expect_snapshot(
    error = TRUE,
    compare_to_leader(fit_max, prob = c(0.5, 0.9))
  )
  expect_snapshot(
    error = TRUE,
    compare_to_leader(fit_max, size = -1)
  )
  expect_snapshot(
    error = TRUE,
    compare_to_leader(fit_max, leader = "nope")
  )
  expect_snapshot(
    error = TRUE,
    compare_to_leader(fit_max, leader = c("one", "two"))
  )
  expect_snapshot(
    error = TRUE,
    compare_to_leader(fit_max, key = "nope")
  )
  expect_snapshot(
    error = TRUE,
    compare_to_leader(fit_max, key = mtcars)
  )
  expect_snapshot(
    error = TRUE,
    compare_to_leader(fit_max, key = tibble::tibble(model = "one"))
  )
  expect_snapshot(
    error = TRUE,
    compare_to_leader(
      fit_max,
      key = tibble::tibble(model = "one", label = "first")
    )
  )
})

test_that("at least two models are required", {
  one_model <- fit_bt
  one_model$names <- "one"
  expect_snapshot(error = TRUE, compare_to_leader(one_model))
})

# ------------------------------------------------------------------------------

test_that("printing", {
  # a fixed object so that the posterior draws don't make the snapshot flaky
  fake <- tidyposterior:::new_compare_to_leader(
    tibble::tibble(
      model = factor(c("a", "b"), levels = c("a", "b")),
      label = factor(c("a", "b"), levels = c("a", "b")),
      rank = 1:2,
      leader = c(TRUE, FALSE),
      median = c(1, 2),
      lower = c(0.5, 1.5),
      upper = c(1.5, 2.5),
      mean_diff = c(0, 1),
      lower_diff = c(0, 0.5),
      upper_diff = c(0, 1.5),
      pr_worse = c(0, 0.9),
      size = c(0.1, 0.1),
      pract_equiv = c(1, 0.05),
      pract_worse = c(0, 0.95)
    ),
    metric = "rmse",
    direction = "minimize",
    leader = "a",
    size = 0.1,
    prob = 0.9
  )
  expect_snapshot(print(fake))
})

# ------------------------------------------------------------------------------

test_that("autoplot", {
  p_worse <- autoplot(res_max)
  expect_s3_class(p_worse, "patchwork")
  expect_equal(p_worse[[2]]$labels$x, "Pr[Worse]")

  p_equiv <- autoplot(res_size)
  expect_s3_class(p_equiv, "patchwork")
  expect_equal(p_equiv[[2]]$labels$x, "Pr[Equiv]")

  # the metric name is used for the axis
  expect_equal(p_worse[[1]]$labels$x, "roc_auc")
  expect_equal(autoplot(res_min)[[1]]$labels$x, "rmse")
})

test_that("autoplot falls back when the attributes have been dropped", {
  # dplyr verbs down-cast the light class and drop its attributes
  stripped <- res_max
  attr(stripped, "metric") <- NULL
  expect_equal(autoplot(stripped)[[1]]$labels$x, "Posterior")

  # `metric_label` still works on a stripped object
  expect_equal(
    autoplot(stripped, metric_label = "RMSE (kg)")[[1]]$labels$x,
    "RMSE (kg)"
  )
})

test_that("`metric_label` overrides the left-hand x-axis", {
  p <- autoplot(res_max, metric_label = "Area under the ROC curve")
  expect_equal(p[[1]]$labels$x, "Area under the ROC curve")
  # the right-hand panel is untouched
  expect_equal(p[[2]]$labels$x, "Pr[Worse]")

  # expressions are allowed, for metrics such as R squared
  p_expr <- autoplot(res_max, metric_label = expression(R^2))
  expect_equal(p_expr[[1]]$labels$x, expression(R^2))

  # the recorded metric is used when nothing is given
  expect_equal(autoplot(res_max)[[1]]$labels$x, "roc_auc")
})

test_that("`metric_label` must be a single string or expression", {
  expect_snapshot(error = TRUE, autoplot(res_max, metric_label = 1))
  expect_snapshot(error = TRUE, autoplot(res_max, metric_label = c("a", "b")))
  expect_snapshot(
    error = TRUE,
    autoplot(res_max, metric_label = NA_character_)
  )
})

test_that("the y-axis runs from the worst at the top to the leader at the bottom", {
  p <- autoplot(res_max)
  # `panel_params$y$limits` is ordered from the bottom of the axis upwards
  bottom_up <- ggplot2::ggplot_build(p[[1]])$layout$panel_params[[1]]$y$limits
  expect_equal(bottom_up, as.character(res_max$model))
  expect_equal(bottom_up[1], attr(res_max, "leader"))
  expect_equal(bottom_up[length(bottom_up)], as.character(res_max$model[3]))

  # both panels share the ordering
  expect_equal(
    ggplot2::ggplot_build(p[[2]])$layout$panel_params[[1]]$y$limits,
    bottom_up
  )
})

test_that("`zero_bar` keeps zero-probability models visible", {
  # the leader always has a `pr_worse` of exactly zero; the rest are estimates
  expect_equal(res_max$pr_worse[1], 0)
  expect_equal(res_max$pr_worse, c(0, 1, 1), tolerance = 0.01)

  bars <- ggplot2::ggplot_build(autoplot(res_max)[[2]])$data[[1]]
  # the floor is applied to the leader's exact zero
  expect_equal(min(bars$xmax), 0.01)
  expect_equal(max(bars$xmax), 1, tolerance = 0.01)

  # only the floored bar is outlined; the others are left clean
  expect_equal(bars$colour, c("grey30", NA, NA))

  wider <- ggplot2::ggplot_build(autoplot(res_max, zero_bar = 0.1)[[2]])
  expect_equal(min(wider$data[[1]]$xmax), 0.1)

  # `zero_bar = 0` draws the probabilities exactly and outlines nothing
  exact <- ggplot2::ggplot_build(autoplot(res_max, zero_bar = 0)[[2]])
  expect_equal(exact$data[[1]]$xmax, c(0, 1, 1), tolerance = 0.01)
  expect_true(all(is.na(exact$data[[1]]$colour)))

  # flooring the bar never changes the returned data
  expect_equal(
    compare_to_leader(fit_max, seed = 2)$pr_worse,
    res_max$pr_worse
  )
})

test_that("`zero_bar` must be a single number in [0, 1]", {
  expect_snapshot(error = TRUE, autoplot(res_max, zero_bar = -1))
  expect_snapshot(error = TRUE, autoplot(res_max, zero_bar = 2))
  expect_snapshot(error = TRUE, autoplot(res_max, zero_bar = c(0.1, 0.2)))
})

test_that("autoplot needs a `compare_to_leader()` result", {
  bad <- res_max
  bad$pract_equiv <- NULL
  expect_snapshot(error = TRUE, autoplot(bad))
})
