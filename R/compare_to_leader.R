#' Compare Models to the Current Leader
#'
#' The posterior distributions created by [perf_mod()] can be used to ask how
#'  each candidate model compares to the best model in the set (the "leader").
#'  `compare_to_leader()` contrasts every model against the leader and returns
#'  a data frame of probability statements about those differences.
#'
#' @param x An object produced by [perf_mod()].
#' @param leader A single character string naming the model to use as the
#'  reference. If `NULL`, the model with the best posterior median is used.
#' @param size The size of an effective difference in the units of the metric
#'  (i.e., the region of practical equivalence). When `NULL`, the ROPE
#'  statistics are not computed and `pr_worse` is the reported probability.
#' @param prob A number p (0 < p < 1) indicating the desired probability mass
#'  to include in the intervals.
#' @param key An optional data frame of display labels with a column of model
#'  names (called either `model` or `wflow_id`) and a character column called
#'  `label`. Every model in `x` must have a row in the key.
#'  [initialize_keys()] makes a template to edit.
#' @param seed A single integer for sampling from the posterior.
#' @param ... Not currently used.
#' @return A tibble with one row per model and the extra class
#'  `"compare_to_leader"`. The columns are:
#'
#'  * `model`: the model name, as a factor ordered from best to worst.
#'  * `label`: the display label, ordered in the same way. This is the same as
#'    `model` unless `key` was given.
#'  * `rank`: the integer rank of the model, where `1` is best.
#'  * `leader`: a logical for whether the row is the reference model.
#'  * `median`, `lower`, `upper`: the posterior median and credible interval
#'    for the metric.
#'  * `mean_diff`, `lower_diff`, `upper_diff`: the posterior mean and credible
#'    interval for the difference `model - leader`.
#'  * `pr_worse`: the probability that the model is worse than the leader.
#'  * `size`: the value of the `size` argument (or `NA`).
#'  * `pract_equiv`: the probability that the model is practically equivalent
#'    to the leader (or `NA` when `size` is `NULL`).
#'  * `pract_worse`: the probability that the model is worse than the leader by
#'    more than `size` (or `NA` when `size` is `NULL`).
#'
#' @details Ranking the models requires knowing whether the metric is better
#'  when larger or smaller. This is taken from `x`, which records it when
#'  [perf_mod()] is given a `tune_results` object or a workflow set. Fits made
#'  from a data frame, an `rset` object, or a `caret` `resamples` object do not
#'  carry a metric, so `compare_to_leader()` cannot be used on them.
#'
#'  Differences are parameterized as `model - leader` so that they
#'  describe how much a candidate gives up relative to the current best. Since
#'  the leader is, by definition, the best model in the set, `pr_worse` is the
#'  informative one-sided probability. It accounts for the direction of the
#'  metric: for metrics that are minimized (such as RMSE) it is the posterior
#'  probability that the difference is positive, and for metrics that are
#'  maximized (such as the area under the ROC curve) it is the probability that
#'  the difference is negative.
#'
#'  The leader's own row describes a comparison against itself. That difference
#'  is exactly zero, so `mean_diff`, `lower_diff`, `upper_diff`, and `pr_worse`
#'  are `0`, `pract_equiv` is `1`, and `pract_worse` is `0`.
#'
#'  When `leader` names a model that is not the best in the set, the models
#'  that beat it have posterior mass above `size` in the _better_ direction.
#'  That mass is not reported, so `pract_equiv` and `pract_worse` sum to less
#'  than one for those rows. Their `pr_worse` values are still correct, and
#'  `1 - pr_worse` is the probability that the model beats the leader.
#'
#'  If a transformation was used when `x` was created, the inverse is applied
#'  before the summaries and differences are computed.
#'
#'  Which models compete is decided when `x` is fit, not here. For workflow
#'  sets, `perf_mod(select_best = TRUE)` collapses workflows that fit the same
#'  type of model with the same engine down to the best of each group, which is
#'  useful when a workflow set contains several preprocessors for the same
#'  model.
#' @seealso [perf_mod()], [contrast_models()], [initialize_keys()],
#'  [autoplot.compare_to_leader()]
#' @examplesIf rlang::is_installed(c("parsnip", "yardstick"))
#' library(parsnip)
#' library(rsample)
#' library(workflowsets)
#'
#' set.seed(1)
#' folds <- vfold_cv(mtcars, v = 5)
#'
#' \donttest{
#' # A workflow set records the metric, so `compare_to_leader()` knows that
#' # RMSE is better when smaller:
#' mpg_models <-
#'   workflow_set(
#'     preproc = list(
#'       small = mpg ~ wt,
#'       medium = mpg ~ wt + hp,
#'       large = mpg ~ .
#'     ),
#'     models = list(lm = linear_reg())
#'   ) |>
#'   workflow_map("fit_resamples", resamples = folds, seed = 2)
#'
#' set.seed(4321)
#' mpg_post <- perf_mod(mpg_models, metric = "rmse", refresh = 0, chains = 2)
#'
#' compare_to_leader(mpg_post, seed = 2)
#'
#' # A half mile per gallon is a meaningful difference here:
#' compare_to_leader(mpg_post, size = 0.5, seed = 2)
#' }
#' @export
compare_to_leader <- function(x, ...) {
  UseMethod("compare_to_leader")
}

#' @export
compare_to_leader.default <- function(x, ...) {
  rlang::abort(
    paste0(
      "`x` should be an object produced by `perf_mod()`, not <",
      class(x)[1],
      ">."
    )
  )
}

#' @export
#' @rdname compare_to_leader
compare_to_leader.perf_mod <- function(
  x,
  leader = NULL,
  size = NULL,
  prob = 0.90,
  key = NULL,
  seed = sample.int(10000, 1),
  ...
) {
  if (length(x$names) < 2) {
    rlang::abort(
      "`x` should contain at least two models to compare to the leader."
    )
  }
  direction <- check_direction(x)
  prob <- check_prob(prob)
  size <- check_size(size)
  key <- check_key(key, x$names)

  # ----------------------------------------------------------------------------
  # Posterior summaries of the metric itself, ranked from best to worst

  ranked <-
    tidy(x, seed = seed) |>
    dplyr::summarize(
      median = median(posterior),
      lower = unname(quantile(posterior, probs = (1 - prob) / 2)),
      upper = unname(quantile(posterior, probs = 1 - (1 - prob) / 2)),
      .by = c(model)
    )
  if (direction == "maximize") {
    ranked <- dplyr::arrange(ranked, dplyr::desc(median))
  } else {
    ranked <- dplyr::arrange(ranked, median)
  }
  ranked$rank <- seq_len(nrow(ranked))

  leader <- check_leader(leader, ranked$model)
  ranked$leader <- ranked$model == leader

  # ----------------------------------------------------------------------------
  # Contrast every other model against the leader as `model - leader`

  others <- setdiff(ranked$model, leader)
  diffs <- contrast_models(
    x,
    list_1 = others,
    list_2 = rep(leader, length(others)),
    seed = seed
  )
  diff_stats <- summarize_leader_diffs(diffs, prob, size, direction)

  res <-
    dplyr::full_join(ranked, diff_stats, by = "model") |>
    fill_leader_row(leader, size) |>
    dplyr::left_join(key, by = "model") |>
    dplyr::arrange(rank) |>
    dplyr::mutate(
      label = factor(label, levels = unique(label)),
      model = factor(model, levels = model)
    ) |>
    dplyr::relocate(model, label, rank, leader, median, lower, upper)

  new_compare_to_leader(
    res,
    metric = x$metric$name,
    direction = direction,
    leader = leader,
    size = size,
    prob = prob
  )
}

# `summary.posterior_diff()` labels its rows with a pasted contrast string. Join
# on that string using the model names taken from the `posterior_diff` object so
# that names containing " vs " can't be mis-parsed.
summarize_leader_diffs <- function(diffs, prob, size, direction) {
  contrasts <-
    diffs |>
    dplyr::distinct(model_1, model_2) |>
    dplyr::mutate(contrast = paste(model_1, model_2, sep = " vs "))

  res <-
    summary(diffs, prob = prob, size = size %||% 0) |>
    dplyr::inner_join(contrasts, by = "contrast")

  # `probability` is Pr[difference > 0] and the difference is `model - leader`
  if (direction == "maximize") {
    res$pr_worse <- 1 - res$probability
    res$pract_worse <- res$pract_neg
  } else {
    res$pr_worse <- res$probability
    res$pract_worse <- res$pract_pos
  }
  if (is.null(size)) {
    res$size <- na_dbl
  }

  res |>
    dplyr::select(
      model = model_1,
      mean_diff = mean,
      lower_diff = lower,
      upper_diff = upper,
      pr_worse,
      size,
      pract_equiv,
      pract_worse
    )
}

# The leader's comparison against itself is a degenerate posterior at zero
fill_leader_row <- function(x, leader, size) {
  is_leader <- x$model == leader
  x$mean_diff[is_leader] <- 0
  x$lower_diff[is_leader] <- 0
  x$upper_diff[is_leader] <- 0
  x$pr_worse[is_leader] <- 0
  x$size[is_leader] <- size %||% na_dbl
  x$pract_equiv[is_leader] <- if (is.null(size)) na_dbl else 1
  x$pract_worse[is_leader] <- if (is.null(size)) na_dbl else 0
  x
}

new_compare_to_leader <- function(x, metric, direction, leader, size, prob) {
  x <- tibble::as_tibble(x)
  attr(x, "metric") <- metric
  attr(x, "direction") <- direction
  attr(x, "leader") <- leader
  attr(x, "size") <- size %||% na_dbl
  attr(x, "prob") <- prob
  class(x) <- c("compare_to_leader", class(x))
  x
}

#' @export
print.compare_to_leader <- function(x, ...) {
  cat("# Comparisons to the leader")
  leader <- attr(x, "leader")
  if (!is.null(leader)) {
    cat(" (", leader, ")", sep = "")
  }
  cat("\n")
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

# ------------------------------------------------------------------------------

#' Create a Template of Model Labels
#'
#' [compare_to_leader()] can relabel models for plotting through its `key`
#'  argument. `initialize_keys()` builds a template for that argument with one
#'  row per model, ready to have its `label` column edited.
#'
#' @param x An object produced by [perf_mod()].
#' @return A tibble with one row per model and two columns: `model`, holding
#'  the model names recorded in `x`, and `label`, initialized to the same
#'  values.
#' @details Leave the `model` column alone. Those names are what
#'  [compare_to_leader()] joins on, and every model in `x` has to be
#'  represented, so editing or dropping them will produce an error. Edit the
#'  `label` column to whatever should appear on the plot.
#'
#'  Rows may be reordered and extra columns may be added; both are ignored.
#'  Labels do not have to be unique.
#' @seealso [compare_to_leader()]
#' @examplesIf rlang::is_installed(c("parsnip", "yardstick"))
#' library(parsnip)
#' library(rsample)
#' library(workflowsets)
#'
#' set.seed(1)
#' folds <- vfold_cv(mtcars, v = 5)
#'
#' \donttest{
#' mpg_models <-
#'   workflow_set(
#'     preproc = list(
#'       small = mpg ~ wt,
#'       medium = mpg ~ wt + hp,
#'       large = mpg ~ .
#'     ),
#'     models = list(lm = linear_reg())
#'   ) |>
#'   workflow_map("fit_resamples", resamples = folds, seed = 2)
#'
#' set.seed(4321)
#' mpg_post <- perf_mod(mpg_models, metric = "rmse", refresh = 0, chains = 2)
#'
#' mpg_keys <- initialize_keys(mpg_post)
#' mpg_keys
#'
#' # Edit the labels, then pass the result along:
#' mpg_keys$label <- c("1 predictor", "2 predictors", "all predictors")
#'
#' mpg_post |>
#'   compare_to_leader(size = 0.5, key = mpg_keys, seed = 2) |>
#'   autoplot()
#' }
#' @export
initialize_keys <- function(x) {
  if (!inherits(x, "perf_mod")) {
    rlang::abort(
      paste0(
        "`x` should be an object produced by `perf_mod()`, not <",
        class(x)[1],
        ">."
      )
    )
  }
  tibble::tibble(model = x$names, label = x$names)
}

# ------------------------------------------------------------------------------
# Argument checking

check_direction <- function(x) {
  direction <- x$metric$direction
  if (!is.character(direction) || length(direction) != 1 || is.na(direction)) {
    rlang::abort(
      c(
        "Can't tell whether the metric should be maximized or minimized.",
        paste0(
          "`x` records this only when `perf_mod()` is given a `tune_results` ",
          "object or a workflow set, since those carry the metric that was ",
          "used."
        )
      )
    )
  }
  if (!direction %in% c("maximize", "minimize")) {
    rlang::abort(
      paste0(
        "The metric direction recorded in `x` should be either 'maximize' or ",
        "'minimize', not '",
        direction,
        "'."
      )
    )
  }
  direction
}

check_prob <- function(prob) {
  bad_prob <-
    !is.numeric(prob) ||
    length(prob) != 1 ||
    is.na(prob) ||
    prob <= 0 ||
    prob >= 1
  if (bad_prob) {
    rlang::abort(
      "`prob` should be a single number greater than 0 and less than 1."
    )
  }
  prob
}

check_size <- function(size) {
  if (is.null(size)) {
    return(size)
  }
  if (!is.numeric(size) || length(size) != 1 || is.na(size) || size <= 0) {
    rlang::abort("`size` should be a single positive number or `NULL`.")
  }
  size
}

check_metric_label <- function(metric_label) {
  if (is.null(metric_label)) {
    return(metric_label)
  }
  ok <-
    length(metric_label) == 1 &&
    (is.character(metric_label) ||
      is.expression(metric_label) ||
      is.language(metric_label))
  if (!ok || (is.character(metric_label) && is.na(metric_label))) {
    rlang::abort(
      "`metric_label` should be a single character string, an expression, or `NULL`."
    )
  }
  metric_label
}

check_leader <- function(leader, models) {
  if (is.null(leader)) {
    return(models[1])
  }
  if (!is.character(leader) || length(leader) != 1 || is.na(leader)) {
    rlang::abort("`leader` should be a single character string or `NULL`.")
  }
  if (!leader %in% models) {
    rlang::abort(
      c(
        paste0("`leader` ('", leader, "') is not one of the models in `x`."),
        paste0(
          "Possible values are: ",
          paste0("'", models, "'", collapse = ", "),
          "."
        )
      )
    )
  }
  leader
}

check_key <- function(key, models) {
  if (is.null(key)) {
    return(tibble::tibble(model = models, label = models))
  }
  if (!is.data.frame(key)) {
    rlang::abort("`key` should be a data frame or `NULL`.")
  }
  id_col <- intersect(c("model", "wflow_id"), names(key))
  if (length(id_col) == 0) {
    rlang::abort(
      c(
        "`key` should have a column of model names called 'model' or 'wflow_id'.",
        "`initialize_keys()` makes a template with the right columns."
      )
    )
  }
  if (!any(names(key) == "label")) {
    rlang::abort(
      c(
        "`key` should have a column of labels called 'label'.",
        "`initialize_keys()` makes a template with the right columns."
      )
    )
  }
  key <- tibble::tibble(
    model = as.character(key[[id_col[1]]]),
    label = as.character(key$label)
  )
  missing_models <- setdiff(models, key$model)
  if (length(missing_models) > 0) {
    rlang::abort(
      c(
        "Every model in `x` should have a row in `key`.",
        paste0(
          "Missing: ",
          paste0("'", missing_models, "'", collapse = ", "),
          "."
        ),
        "`initialize_keys()` makes a template with a row for every model."
      )
    )
  }
  dplyr::filter(key, model %in% models)
}

# ------------------------------------------------------------------------------

#' Visualize How Models Compare to the Leader
#'
#' Two panels are drawn side-by-side: the posterior distribution of the metric
#'  for each model (as a median and credible interval) and the probability that
#'  each model differs from the leader.
#'
#' @param object An object produced by [compare_to_leader()].
#' @param zero_bar A single number giving the shortest bar to draw in the
#'  right-hand panel. Probabilities of zero would otherwise draw a bar with no
#'  length, which reads as a missing row rather than a zero. Set it to `0` to
#'  draw the probabilities exactly.
#' @param metric_label A single character string or expression used to label
#'  the x-axis of the left-hand panel. If `NULL`, the name of the metric
#'  recorded by [perf_mod()] is used. This is useful for spelling a metric out
#'  or adding units, such as `"RMSE (kg)"`.
#' @param ... Not currently used.
#' @return A [patchwork::patchwork] object made from two [ggplot2::ggplot()]
#'  objects.
#' @details The right-hand panel shows `pract_equiv` when [compare_to_leader()]
#'  was given a `size` and `pr_worse` otherwise. In both cases the fill scale is
#'  oriented so that darker bars are better.
#'
#'  Note that `zero_bar` makes a bar's length depart from the probability it
#'  represents: any value below `zero_bar` is drawn at `zero_bar`. The fill
#'  colour is always mapped to the true value, and the leader always has a
#'  `pr_worse` of exactly zero.
#'
#'  Models are ordered by rank along the y-axis, running from the worst at the
#'  top to the leader at the bottom.
#'
#'  The left-hand panel is labelled with the metric name when one is available.
#'  A `perf_mod` object always records one, but a `compare_to_leader()` result
#'  that has been through a `dplyr` verb will have lost the attribute, in which
#'  case the axis falls back to `"Posterior"`. Use `metric_label` to set it
#'  directly.
#' @seealso [compare_to_leader()]
#' @examplesIf rlang::is_installed(c("parsnip", "yardstick"))
#' library(parsnip)
#' library(rsample)
#' library(workflowsets)
#'
#' set.seed(1)
#' folds <- vfold_cv(mtcars, v = 5)
#'
#' \donttest{
#' mpg_models <-
#'   workflow_set(
#'     preproc = list(
#'       small = mpg ~ wt,
#'       medium = mpg ~ wt + hp,
#'       large = mpg ~ .
#'     ),
#'     models = list(lm = linear_reg())
#'   ) |>
#'   workflow_map("fit_resamples", resamples = folds, seed = 2)
#'
#' set.seed(4321)
#' mpg_post <- perf_mod(mpg_models, metric = "rmse", refresh = 0, chains = 2)
#'
#' mpg_res <- compare_to_leader(mpg_post, size = 0.5, seed = 2)
#' autoplot(mpg_res)
#'
#' # Spell the metric out and give it units:
#' autoplot(mpg_res, metric_label = "RMSE (miles per gallon)")
#' }
#' @export
autoplot.compare_to_leader <- function(
  object,
  zero_bar = 0.01,
  metric_label = NULL,
  ...
) {
  if (
    !is.numeric(zero_bar) ||
      length(zero_bar) != 1 ||
      is.na(zero_bar) ||
      zero_bar < 0 ||
      zero_bar > 1
  ) {
    rlang::abort("`zero_bar` should be a single number between 0 and 1.")
  }
  metric_label <- check_metric_label(metric_label)
  dat <- tibble::as_tibble(object)
  req_cols <- c("label", "median", "lower", "upper", "pr_worse", "pract_equiv")
  if (!all(req_cols %in% names(dat))) {
    rlang::abort(
      "`object` should be a data frame produced by `compare_to_leader()`."
    )
  }

  if (is.null(metric_label)) {
    metric_label <- attr(object, "metric")
    if (is.null(metric_label) || is.na(metric_label)) {
      metric_label <- "Posterior"
    }
  }

  if (any(!is.na(dat$pract_equiv))) {
    prob_col <- "pract_equiv"
    prob_lab <- "Pr[Equiv]"
    fill_dir <- -1
  } else {
    prob_col <- "pr_worse"
    prob_lab <- "Pr[Worse]"
    fill_dir <- 1
  }

  p_metric <-
    ggplot2::ggplot(dat, ggplot2::aes(x = median, y = label)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = lower, xmax = upper),
      width = 1 / 3
    ) +
    ggplot2::labs(y = NULL, x = metric_label) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 10, r = 8, b = 10, l = 10, unit = "pt")
    )

  # A probability of zero draws a bar with no length, which reads as a missing
  # row rather than a zero. Plot a small sliver instead so the model is still
  # visible. The fill is mapped to the true value so the colour stays honest.
  dat$.bar <- pmax(dat[[prob_col]], zero_bar)
  # Only the slivers are outlined. Without it a floored bar is invisible when
  # its fill is pale, but outlining every bar is needlessly busy.
  dat$.outline <- ifelse(dat[[prob_col]] < zero_bar, "grey30", NA)

  p_prob <-
    ggplot2::ggplot(
      dat,
      ggplot2::aes(
        x = .data$.bar,
        y = label,
        fill = .data[[prob_col]],
        colour = .data$.outline
      )
    ) +
    ggplot2::geom_col(show.legend = FALSE, linewidth = 0.3) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(breaks = c(0, 0.5, 1), limits = 0:1) +
    ggplot2::scale_fill_viridis_c(direction = fill_dir, limits = 0:1) +
    ggplot2::labs(y = NULL, x = prob_lab) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 8, unit = "pt")
    )

  patchwork::wrap_plots(p_metric, p_prob, widths = c(4, 1))
}
