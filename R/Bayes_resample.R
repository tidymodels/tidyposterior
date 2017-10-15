#' Bayesian Analysis of Resampling Statistics
#'
#' These functions can be used to process and analyze matched
#'  resampling statistics from different models using a Bayesian
#'  generalized linear model with effects for the model and the
#'  resamples.
#' 
#' @param object A data frame or and `rset` object (such as
#'  [rsample::vfold_cv()]) containing the `id` column and at least
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
#' @export
#' @importFrom dplyr filter select mutate %>%
#' @importFrom tidyr gather
#' @importFrom rstanarm stan_glmer
#' @importFrom rlang !!
Bayes_resample.rset <- 
  function(object, transform = no_trans, ...) {
    
    if(inherits(object, "bootstraps"))
      object <- object %>% dplyr::filter(id != "Apparent")
    
    rset_type <- class(object)[1]
    if(any(names(object) == "splits"))
      object <- object %>% dplyr::select(-splits)
    
    object <- as.data.frame(object)
    
    id_vars <- grep("^id", names(object), value = TRUE)
    
    other_vars <- names(object)[!(names(object) %in% id_vars)]
    if(length(other_vars) < 2) 
      stop("There should be at least two other columns ",
           "(besides `id` and `splits` columns) in the data set.")
    
    # check types?
    
    resamples <- 
      tidyr::gather(
        object,
        key = model,
        value = statistic,
        - !!id_vars,
        na.rm = TRUE,
        convert = TRUE,
        factor_key = TRUE
      ) %>%
      dplyr::mutate(statistic = transform$func(statistic))
    
    ## Make a formula based on resampling type (repeatedcv, rof), 
    ## This could be done with more specific classes
    
    model_names <- unique(as.character(resamples$model))
    
    mod <- stan_glmer(statistic ~  model + (1 | id), data = resamples, ...)
    
    res <- list(Bayes_mod = mod,
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
  cat("Original data from `", x$rset_type, "` object\n\n")
  invisible(x)
}

#' @export
summary.Bayes_resample <- function(object, ...) {
  summary(object$Bayes_mod)
}
