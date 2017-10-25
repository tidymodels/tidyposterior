#' Extract Posterior Distributions for Models
#' 
#' `tidy` can be used on an object produced by [Bayes_resample()]
#'  to create a data frame with a column for the model name and
#'  the posterior predictive distribution values. 
#'  
#' @param x An object from [Bayes_resample()]
#' @param seed A single integer for sampling from the posterior. 
#' @param ... Not currently used
#' @return A data frame with the additional class `"posterior"`
#' @export
#' @importFrom tidyr gather
#' @importFrom dplyr mutate %>%
#' @importFrom broom tidy
tidy.Bayes_resample <- function(x, seed = sample.int(10000, 1), ...) {
  post_dat <- get_post(x, seed = seed)
  post_dat <-
    tidyr::gather(
      post_dat,
      key = model,
      value = posterior
    ) %>%
    dplyr::mutate(posterior = x$transform$inv(posterior))
  class(post_dat) <- c("posterior", class(post_dat))
  post_dat
}

#' Summarize the Posterior Distributions of Model Statistics
#' 
#' Numerical summaries are created for each model including the
#'  posterior mean and upper and lower credible intervals (aka
#'  uncertainty intervals). 
#'
#' @param object An object produced by [tidy.Bayes_resample()]. 
#' @param prob A number p (0 < p < 1) indicating the desired
#'  probability mass to include in the intervals. 
#' @param seed A single integer for sampling from the posterior. 
#' @param ... Not currently used
#' @return A data frame with summary statistics and a row for
#'  each model. 
#' @export
#' @importFrom dplyr group_by do summarise full_join
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
#' @param data An object produced by [tidy.Bayes_resample()]. 
#' @param mapping,...,environment Not currently used. 
#' @param reorder A logical; should the `model` column be reordered
#'  by the average of the posterior distribution? 
#' @return A [ggplot2::ggplot()] object. 
#' @export
#' @importFrom ggplot2 ggplot geom_violin xlab ylab
#' @importFrom stats reorder
ggplot.posterior <- 
  function (data, mapping = NULL, ..., environment = NULL, reorder = TRUE) {
    if(reorder) 
      data$model <- stats::reorder(data$model, data$posterior)
    ggplot(as.data.frame(data), aes(x = model, y = posterior)) + 
      geom_violin() +
      xlab("") + 
      ylab("Posterior Probability")
  }


#' @importFrom rstanarm posterior_predict
get_post <- function(x, seed = sample.int(10000, 1)) {
  new_dat <- data.frame(model = unique(x$names))
  new_dat <- as.data.frame(lapply(x$ids, 
                                  function(x) rep(x[1], nrow(new_dat)))) %>%
    bind_cols(new_dat)
  post_data <- 
    rstanarm::posterior_predict(x$Bayes_mod, newdata = new_dat, seed = seed)
  post_data <- as.data.frame(post_data)
  names(post_data) <- x$names
  post_data
}

postint <- function(object, ...) UseMethod("postint")

#' @importFrom rstanarm posterior_interval
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

#' @importFrom utils globalVariables
utils::globalVariables(c(".", "aes", "posterior"))
