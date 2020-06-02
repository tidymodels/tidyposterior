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
#' @examples
#' # Example objects from the "Getting Started" vignette at
#' #  https://topepo.github.io/tidyposterior/articles/Getting_Started.html
#'
#' # File for pre-run model is at
#' ex_dat <- "https://bit.ly/2OJdvl1"
#'
#' # load(load(url(ex_dat))
#'
#' # roc_model
#' # posterior_values <- tidy(roc_model)
#' # head(posterior_values)
#' # class(posterior_values)
#'
#' @export
#' @export tidy.perf_mod

tidy.perf_mod <- function(x, seed = sample.int(10000, 1), ...) {
  post_dat <- get_post(x, seed = seed)
  post_dat <-
    tidyr::gather(
      post_dat,
      key = model,
      value = posterior
    ) %>%
    dplyr::mutate(posterior = x$transform$inv(posterior))
  post_dat <- as_tibble(post_dat)
  class(post_dat) <- c("posterior", class(post_dat))
  post_dat
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
#' # Example objects from the "Getting Started" vignette at
#' #  https://topepo.github.io/tidyposterior/articles/Getting_Started.html
#'
#' # File for pre-run model is at
#' ex_dat <- "https://bit.ly/2OJdvl1"
#'
#' # load(load(url(ex_dat))
#'
#' # posterior_values <- tidy(roc_model)
#' # summary(posterior_values)
#'
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


#' Visualize the Posterior Distributions of Model Statistics
#'
#' A simple violin plot is created by the function.
#'
#' @param data An object produced by [tidy.perf_mod()].
#' @param mapping,...,environment Not currently used.
#' @param reorder A logical; should the `model` column be reordered
#'  by the average of the posterior distribution?
#' @return A [ggplot2::ggplot()] object using
#'  [ggplot2::geom_violin()] for the posteriors.
#' @examples
#' # Example objects from the "Getting Started" vignette at
#' #  https://topepo.github.io/tidyposterior/articles/Getting_Started.html
#'
#' # File for pre-run model is at
#' ex_dat <- "https://bit.ly/2OJdvl1"
#'
#' # load(load(url(ex_dat))
#'
#' # posterior_values <- tidy(roc_model)
#'
#' # library(ggplot2)
#' # ggplot(posterior_values) + theme_bw()
#' @export
ggplot.posterior <-
  function (data, mapping = NULL, ..., environment = NULL, reorder = TRUE) {
    if(reorder)
      data$model <- stats::reorder(data$model, data$posterior)
    ggplot2::ggplot(as.data.frame(data), aes(x = model, y = posterior)) +
      ggplot2::geom_violin() +
      ggplot2::xlab("") + ggplot2::ylab("Posterior Probability")
  }

get_post <- function(x, seed = sample.int(10000, 1)) {
  new_dat <- data.frame(model = unique(x$names))
  new_dat <-
    as.data.frame(lapply(x$ids, function(x) rep(x[1], nrow(new_dat)))) %>%
    bind_cols(new_dat)
  post_data <-
    rstanarm::posterior_linpred(
      x$stan,
      newdata = new_dat,
      seed = seed,
      re.form = NA,
      transform = TRUE
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
                               seed = sample.int(10000, 1), ...)
  postint(getElement(object, "posterior"), prob = prob, seed = seed)
