#' Extract Posterior Distributions for Models
#'
#' `tidy` can be used on an object produced by [perf_mod()]
#'  to create a data frame with a column for the model name and
#'  the posterior predictive distribution values.
#'
#' @param x An object from [perf_mod()]
#' @param seed A single integer for sampling from the posterior.
#' @param ... Not currently used
#' @return A data frame with the additional class `"posterior"`
#' @details Note that this posterior only reflects the variability
#'  of the groups (i.e. the fixed effects). This helps answer the
#'  question of which model is best _for this data set_. If does not
#'  answer the question of which model would be best on a new
#'  resample of the data (which would have greater variability).
#' @export
#' @export tidy.perf_mod

tidy.perf_mod <- function(x, seed = sample.int(10000, 1), ...) {
  post_dat <- get_post(x, seed = seed)
  post_dat <-
    post_dat %>%
    tidyr::pivot_longer(c(dplyr::everything()),
                        names_to = "model",
                        values_to = "posterior") %>%
    dplyr::mutate(posterior = x$transform$inv(posterior))
  post_dat <- as_tibble(post_dat)
  class(post_dat) <- c("posterior", class(post_dat))
  post_dat
}

#' @export
print.posterior <- function(x, ...) {
  cat("# Posterior samples of performance\n")
  print(tibble::as_tibble(x), ...)
}

#' Summarize the Posterior Distributions of Model Statistics
#'
#' Numerical summaries are created for each model including the
#'  posterior mean and upper and lower credible intervals (aka
#'  uncertainty intervals).
#'
#' @param object An object produced by [tidy.perf_mod()].
#' @param prob A number p (0 < p < 1) indicating the desired
#'  probability mass to include in the intervals.
#' @param seed A single integer for sampling from the posterior.
#' @param ... Not currently used
#' @return A data frame with summary statistics and a row for
#'  each model.
#' @examples
#' data("ex_objects")
#'
#' summary(posterior_samples)
#' @export
summary.posterior <- function(object, prob = 0.90,
                              seed = sample.int(10000, 1), ...) {
  post_int <- object %>%
    dplyr::group_by(model) %>%
    dplyr::do(postint.data.frame(., prob = prob, seed = seed))
  post_stats <- object %>%
    dplyr::group_by(model) %>%
    dplyr::summarise(mean = mean(posterior)) %>%
    dplyr::full_join(post_int, by = "model")
  post_stats
}


get_post <- function(x, seed = sample.int(10000, 1)) {
  new_dat <- data.frame(model = unique(x$names))
  new_dat <-
    as.data.frame(lapply(x$ids, function(x) rep(x[1], nrow(new_dat)))) %>%
    bind_cols(new_dat)
  post_data <-
    rstanarm::posterior_epred(
      x$stan,
      newdata = new_dat,
      seed = seed,
      re.form = NA
    )
  post_data <- as.data.frame(post_data)
  names(post_data) <- x$names
  post_data
}

postint <- function(object, ...) UseMethod("postint")


postint.numeric <- function(object, prob = 0.90,
                            seed = sample.int(10000, 1), ...) {
  object <- matrix(object, ncol = 1)
  res <- rstanarm::posterior_interval(object, prob = prob, seed = seed)
  res <- as.data.frame(res)
  names(res) <- c("lower", "upper")
  res
}
postint.data.frame <- function(object, prob = 0.90,
                               seed = sample.int(10000, 1), ...) {
  postint(getElement(object, "posterior"), prob = prob, seed = seed)
}



#' Visualize the Posterior Distributions of Model Statistics
#'
#' For objects of classes `posterior` and `perf_mod`, `autoplot()` produces a
#' simple plot of posterior distributions. For workflow set objects, there are
#' several types of plots that can be produced.
#'
#' @inheritParams  summary.posterior_diff
#' @param object An object produced by [perf_mod()], [tidy.perf_mod()], or a
#' workflow set with computed results.
#' @return A [ggplot2::ggplot()] object.
#' @param ... Options passed to `geom_line(stat = "density", ...)`.
#' @param type A value of one of: `"intervals"` (for model rank versus posterior
#' probability using interval estimation), `"posteriors"` (density plots for
#' each model), or `"ROPE"` (for practical equivalence probabilities versus
#' workflow rank).
#' @examples
#' data(ex_objects)
#' autoplot(posterior_samples)
#' @export
autoplot.posterior <-
  function(object, ...) {
    ggplot2::ggplot(as.data.frame(object), ggplot2::aes(x = posterior, col = model)) +
      ggplot2::geom_line(stat = "density", ...)
  }


#' @rdname autoplot.posterior
#' @export
autoplot.perf_mod <- function(object, ...) {
  samples <- tidy(object)
  res <- autoplot(samples, ...)
  if (any(names(object) == "metric") && !is.na(object$metric$name)) {
    res <- res + ggplot2::xlab(object$metric$name)
  }
  res
}

#' @rdname autoplot.posterior
#' @export
autoplot.perf_mod_workflow_set <- function(object, type = "intervals", prob = 0.9, size = NULL, ...) {
  type <- match.arg(type, c("intervals", "posteriors", "ROPE"))
  if (type == "intervals") {
    res <- plot_wset_intervals(object, prob, ...)
  } else if (type == "posteriors") {
    res <- autoplot.perf_mod(object, ...)
  } else {
    res <- plot_rope_probs(object, size, ...)
  }
  res
}

plot_wset_intervals <- function(object, prob, ...) {
  plot_data <-
    tidy(object) %>%
    dplyr::group_by(model) %>%
    dplyr::summarize(
      .lower = quantile(posterior, prob = 1 - prob[1]),
      .estimate = median(posterior),
      .upper = quantile(posterior, prob = prob[1]),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(workflow = model)
  if (object$metric$direction == "maximize") {
    plot_data$rank <- rank(-plot_data$.estimate, ties.method = "random")
  } else if (object$metric$direction == "minimize") {
    plot_data$rank <- rank(plot_data$.estimate, ties.method = "random")
  } else {
    rlang::abort("Don't know how to rank metric")
  }
  ggplot2::ggplot(plot_data, ggplot2::aes(x = rank, y = .estimate, col = workflow)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .lower, ymax = .upper),
      width = diff(range(plot_data$rank)) / 75
    ) +
    ggplot2::labs(x = "Workflow Rank", y = object$metric$name)
}

plot_rope_probs <- function(object, size, ...) {
  if (is.null(size)) {
    rlang::abort("Please supply a practical effect size via the `size` argument. ")
  }
  posteriors <-
    tidy(object) %>%
    dplyr::group_by(model) %>%
    dplyr::summarize(.estimate = median(posterior), .groups = "drop") %>%
    dplyr::ungroup()

  if (object$metric$direction == "maximize") {
    posteriors <- dplyr::arrange(posteriors, dplyr::desc(.estimate))
    posteriors$rank <- rank(-posteriors$.estimate, ties.method = "random")
    worse_dir <- "pract_pos"
  } else if (object$metric$direction == "minimize") {
    posteriors <- dplyr::arrange(posteriors, .estimate)
    posteriors$rank <- rank(posteriors$.estimate, ties.method = "random")
    worse_dir <- "pract_neg"
  } else {
    rlang::abort("Don't know how to rank metric")
  }
  l1 <- rep(posteriors$model[1], nrow(posteriors))
  l2 <- posteriors$model

  model_diffs <- contrast_models(object, l1, l2, seed = sample.int(1, 1000))
  plot_data <- summary(model_diffs, size = size)
  rm_text <- paste0(posteriors$model[1], " vs ")
  plot_data$model <- gsub(rm_text, "", plot_data$contrast, fixed = TRUE)

  plot_data <- dplyr::full_join(
    plot_data[, c("model", "pract_equiv")],
    posteriors[, c("model", "rank")],
    by = "model"
  )
  plot_data$workflow <- plot_data$model
  ggplot2::ggplot(plot_data, ggplot2::aes(x = rank, y = pract_equiv)) +
    ggplot2::geom_line(alpha = .2) +
    ggplot2::geom_point(ggplot2::aes(col = workflow)) +
    ggplot2::labs(x = "Workflow Rank", y = "Probability of Practical Equivalence") +
    ggplot2::ylim(0:1)
}
