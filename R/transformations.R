#' Simple Transformation Functions
#'
#' A set of objects are contained here to easily facilitate the
#'  use of outcome transformations for modeling. For example, if
#'  there is a large amount of variability in the resampling results
#'  for the Kappa statistics, which lies between -1 and 1, assuming
#'  normality may produce posterior estimates outside of the natural
#'  bound. One way to solve this is to use a link function or assume
#'  a prior that is appropriately bounded. Another approach is to
#'  transform the outcome values prior to modeling using a Gaussian
#'  prior and reverse-transforming the posterior estimates prior to
#'  visualization and summarization. These object can help
#'  facilitate this last approach.
#'
#' @details The `logit_trans` object is useful for model
#'  performance statistics bounds in zero and one, such as accuracy
#'  or the area under the ROC curve.
#'
#' `ln_trans` and `inv_trans` can be useful when the statistics
#'  are right-skewed and strictly positive.
#'
#' `Fisher_trans` was originally used for correlation statistics
#'  but can be used here for an metrics falling between -1 and 1,
#'  such as Kappa.
#'
#' @examples
#' logit_trans$func(.5)
#' logit_trans$inv(0)
#' @rdname transformations
#' @export
no_trans <- list(
  func = function(x) x,
  inv = function(x) x
)
#' @rdname transformations
#' @export
logit_trans <- list(
  func = binomial()$linkfun,
  inv = binomial()$linkinv
)
#' @rdname transformations
#' @export
Fisher_trans <- list(func = atanh, inv = tanh)
#' @rdname transformations
#' @export
ln_trans <- list(func = log, inv = exp)
#' @rdname transformations
#' @export
inv_trans <- list(
  func = function(x) 1 / x,
  inv = function(x) 1 / x
)
