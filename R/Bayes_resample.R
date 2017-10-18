#' Bayesian Analysis of Resampling Statistics
#'
#' These functions can be used to process and analyze matched
#'  resampling statistics from different models using a Bayesian
#'  generalized linear model with effects for the model and the
#'  resamples.
#'
#' @param object A data frame or and `rset` object (such as
#'  [rsample::vfold_cv()]) containing the `id` column(s) and at least
#'  two numeric columns of model performance statistics (e.g.
#'  accuracy).
#' @param ... Additonal arguments to pass to [rstanarm::stan_glmer()]
#'  such as `verbose`, `prior`, `seed`, etc.
#' @return An object of class `Bayes_resample`.
#' @export
Bayes_resample <- function(object, ...)
  UseMethod("Bayes_resample")

# Make a general data.frame method, maybe `gather` methods for
# `rset` and `rsample` objects instead of having the `gather`
# code inside of `Bayes_resample.rset`. If we do that, there could
# be more specific methods (e.g. "rolling_origin" instead of `rset`)

#' @rdname Bayes_resample
#' @param transform An named list of transformation and inverse
#'  transformation fuctions. See [logit_trans()] as an example.
#' @param hetero_var A logical; if `TRUE`, then different
#'  variances are estimated for each model group. Otherwise, the
#'  same variance is used for each group. Estimating heterogeneous
#'  variances may slow or prevent convergence.
#' @export
#' @importFrom dplyr filter select mutate %>%
#' @importFrom rsample gather.rset pretty.group_vfold_cv
#' @importFrom rsample pretty.bootstraps pretty.nested_cv
#' @importFrom rsample pretty.mc_cv pretty.rolling_origin
#' @importFrom rsample pretty.loo_cv pretty.vfold_cv
#' @importFrom rstanarm stan_glmer
#' @importFrom rlang !!
Bayes_resample.rset <-
  function(object, transform = no_trans, hetero_var = FALSE, ...) {
    rset_type <- pretty(object)

    if(inherits(object, "bootstraps"))
      object <- object %>% dplyr::filter(id != "Apparent")

    resamples <- gather(object) %>%
      dplyr::mutate(statistic = transform$func(statistic))

    ## Make a formula based on resampling type (repeatedcv, rof),
    ## This could be done with more specific classes

    model_names <- unique(as.character(resamples$model))

    if (hetero_var) {
      mod <- stan_glmer(statistic ~  model + (model + 0 | id),
                        data = resamples, ...)
    } else {
      mod <- stan_glmer(statistic ~  model + (1 | id),
                        data = resamples, ...)
    }

    res <- list(Bayes_mod = mod,
                hetero_var = hetero_var,
                names = model_names,
                rset_type = rset_type,
                ids = unique(resamples$id),
                transform = transform)
    class(res) <- "Bayes_resample"
    res
  }

#' @importFrom utils globalVariables
utils::globalVariables(c("id", "model", "splits", "statistic"))

#' @export
print.Bayes_resample <- function(x, ...) {
  cat("Bayesian Analysis of Resampling Results\n")
  cat("Original data: ")
  cat(x$rset_type, sep = "\n")
  cat("\n")
  invisible(x)
}

#' @export
summary.Bayes_resample <- function(object, ...) {
  summary(object$Bayes_mod)
}
