#' Bayesian Analysis of Resampling Statistics
#'
#' Bayesian analysis used here to answer the question: "when
#'  looking at resampling results, are the differences between
#'  models 'real?'" To answer this, a model can be created were the
#'  _outcome_ is the resampling statistics (e.g. accuracy or RMSE).
#'  These values are explained by the model types. In doing this, we
#'  can get parameter estimates for each model's affect on
#'  performance and make statistical (and practical) comparisons
#'  between models.
#'
#' @param object A data frame or an `rset` object (such as
#'  [rsample::vfold_cv()]) containing the `id` column(s) and at least
#'  two numeric columns of model performance statistics (e.g.
#'  accuracy). Additionally, an object from `caret::resamples`
#'  can be used. 
#' @param ... Additonal arguments to pass to [rstanarm::stan_glmer()]
#'  such as `verbose`, `prior`, `seed`, `family`, etc.
#' @return An object of class `perf_mod`.
#' @details These functions can be used to process and analyze
#'  matched resampling statistics from different models using a
#'  Bayesian generalized linear model with effects for the model and
#'  the resamples.
#' 
#' By default, a generalized linear model with Gaussian error and
#'  an identity link is fit to the data and has terms for the
#'  predictive model grouping variable. In this way, the performance
#'  metrics can be compared between models.
#'
#' Additionally, random effect terms are also used. For most
#'  resampling methods (except repeated _V_-fold cross-validation),
#'  a simple random intercept model its used with an exchangeable
#'  (i.e. compound-symmetric) variance structure. In the case of
#'  repeated cross-validation, two random intercept terms are used;
#'  one for the repeat and another for the fold within repeat. These
#'  also have exchangeable correlation structures.
#'
#' The above model specification assumes that the variance in the
#'  performance metrics is the same across models. However, this is
#'  unlikely to be true in some cases. For example, for simple
#'  binomial accuracy, it well know that the variance is highest
#'  when the accuracy is near 50 percent. When the argument
#'  `hetero_var = TRUE`, the variance structure uses random
#'  intercepts for each model term. This may produce more realistic
#'  posterior distributions but may take more time to converge.
#'
#' Also, as shown in the package vignettes, the Gaussian assumption
#'  make be unrealistic. In this case, there are at least two
#'  approaches that can be used. First, the outcome statistics can
#'  be transformed prior to fitting the model. For example, for
#'  accuracy, the logit transformation can be used to convert the
#'  outcome values to be on the real line and a model is fit to
#'  these data. Once the posterior distributions are computed, the
#'  inverse transformation can be used to put them back into the
#'  original units. The `transform` argument can be used to do this.
#'
#' The second approach would be to use a different error
#'  distribution from the exponential family. For RMSE values, the
#'  Gamma distribution may produce better results at the expense of
#'  model computational complexity. This can be achieved by passing
#'  the `family` argument to `perf_mod` as one might with the
#'  `glm` function.
#' @examples 
#' # Example objects from the "Getting Started" vignette at
#' #  https://topepo.github.io/tidyposterior/articles/Getting_Started.html
#' 
#' file <- system.file("examples", "roc_model.RData", package = "tidyposterior")
#' load(file)
#' 
#' roc_model
#' 
#' # Summary method shows the underlying `stan` model
#' summary(roc_model)
#' @export
perf_mod <- function(object, ...)
  UseMethod("perf_mod")


#' @export
perf_mod.default <- function(object, ...)
  stop("`object` should have at least one of these classes: ",
       "'rset', 'data.frame', 'resamples', or 'vfold_cv'. ",
       "See ?perf_mod")

# Make a general data.frame method, maybe `gather` methods for
# `rset` and `rsample` objects instead of having the `gather`
# code inside of `perf_mod.rset`. If we do that, there could
# be more specific methods (e.g. "rolling_origin" instead of `rset`)

#' @rdname perf_mod
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
perf_mod.rset <-
  function(object, transform = no_trans, hetero_var = FALSE, ...) {
    check_trans(transform)
    rset_type <- try(pretty(object), silent = TRUE)
    if(inherits(rset_type, "try-error"))
      rset_type <- NA

    ## dplyr::filter (and `[` !) drops the other classes =[
    if(inherits(object, "bootstraps")) {
      oc <- class(object)
      object <- object %>% dplyr::filter(id != "Apparent")
      class(object) <- oc
    }

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

    res <- list(stan = mod,
                hetero_var = hetero_var,
                names = model_names,
                rset_type = rset_type,
                ids = get_id_vals(resamples),
                transform = transform)
    class(res) <- "perf_mod"
    res
  }

#' @export
#' @exportMethod perf_mod vfold_cv
#' @importFrom rsample vfold_cv
#' @rdname perf_mod
perf_mod.vfold_cv <-
  function(object, transform = no_trans, hetero_var = FALSE, ...) {
    check_trans(transform)
    rset_type <- try(pretty(object), silent = TRUE)
    if(inherits(rset_type, "try-error"))
      rset_type <- NA

    resamples <- gather(object) %>%
      dplyr::mutate(statistic = transform$func(statistic))
    
    model_names <- unique(as.character(resamples$model))
    
    if(attributes(object)$repeats > 1) {
      if (hetero_var) {
        mod <- stan_glmer(statistic ~  model + 
                            (model + 0 | id2/id),
                          data = resamples, ...)
      } else {
        mod <- stan_glmer(statistic ~  model + 
                            (1 | id2/id),
                          data = resamples, ...)
      }
    } else {
      if (hetero_var) {
        mod <- stan_glmer(statistic ~  model + (model + 0 | id),
                          data = resamples, ...)
      } else {
        mod <- stan_glmer(statistic ~  model + (1 | id),
                          data = resamples, ...)
      }
    }

    
    res <- list(stan = mod,
                hetero_var = hetero_var,
                names = model_names,
                rset_type = rset_type,
                ids = get_id_vals(resamples),
                transform = transform)
    class(res) <- "perf_mod"
    res
  }



#' @importFrom utils globalVariables
utils::globalVariables(c("id", "model", "splits", "statistic", "Resample"))

#' @export
print.perf_mod <- function(x, ...) {
  cat("Bayesian Analysis of Resampling Results\n")
  if(!is.na(x$rset_type)) {
    cat("Original data: ")
    cat(x$rset_type, sep = "\n")
  }
  cat("\n")
  invisible(x)
}

#' @export
summary.perf_mod <- function(object, ...) {
  summary(object$stan)
}


#' @export
#' @importFrom stats setNames
#' @importFrom purrr map_chr
#' @rdname perf_mod
#' @param metric A single character value for the statstic from
#'  the `resamples` object that should be analyzed. 
perf_mod.resamples <-
  function(object,
           transform = no_trans,
           hetero_var = FALSE,
           metric = object$metrics[1],
           ...) {
    suffix <- paste0("~", metric, "$")
    metric_cols <- grep(suffix,
                        names(object$values),
                        value = TRUE)
    object$values <- object$values %>%
      dplyr::select(Resample,!!metric_cols) %>%
      setNames(gsub(suffix, "", names(.)))
    
    if(is_repeated_cv(object)) {
      split_up <- strsplit(as.character(object$values$Resample), "\\.")
      object$values <- object$values %>%
        dplyr::mutate(id = map_chr(split_up, function(x) x[2]),
                      id2 = map_chr(split_up, function(x) x[1])) %>%
        dplyr::select(-Resample)
      class(object$values) <- c("vfold_cv", "rset", class(object$values))
      cv_att <- list(v = length(unique(object$values$id2)), 
                     repeats = length(unique(object$values$id)), 
                     strata = FALSE)
      for (i in names(cv_att)) attr(object$values, i) <- cv_att[[i]]
    } else {
      object$values <- object$values %>%
        dplyr::rename(id = Resample)
      class(object$values) <- c("rset", class(object$values))
    }
    
    
    perf_mod(object$values, transform = transform, 
             hetero_var = hetero_var, ...)
  }

#' @export
#' @rdname perf_mod
perf_mod.data.frame <-
  function(object,
           transform = no_trans,
           hetero_var = FALSE,
           ...) {
    id_cols <- grep("(^id)|(^id[1-9]$)",
                    names(object),
                    value = TRUE)
    if (length(id_cols) == 0)
      stop("One or more `id` columns are required.", call. = FALSE)

    if (length(id_cols) > 1) {
      warning(
        "Since no specific resampling method is known,",
        "the ID variables are collapsed into one column.",
        call. = FALSE
      )
      tmp <- apply(object[, id_cols], 1, paste0, collapse = "-")
      for (i in id_cols)
        object[, i] <- NULL
      object$id <- tmp
    }
    
    class(object) <- c("rset", class(object))
    
    perf_mod(object, transform = transform, 
             hetero_var = hetero_var, ...)
  }
