#' Example Data Sets
#'
#' @details Three difference data sets are contained in the package
#'  as examples. Each _simulates_ an `rset` object but the `splits`
#'  columns are not included to save space.
#'
#' `precise_example` contains the results of the classification analysis of a real data set using 10-fold CV. The holdout data sets contained thousands of examples and have precise performance estimates. Three models were fit to the original data and several performance metrics are included.
#'
#' `noisy_example` was also generated from a regresison data simulation. The original data set was small (50 samples) and 10-repeated of 10-fold CV were used with four models. There is an excessive of variability in the results (probably more than the resample-to-resample variability). The RMSE distributions show fairly right-skewed distributions.
#'
#' `concrete_example` contains the results of the regression case study from the book _Applied Predictive Modeling_. The original data set contained 745 samples in the training set. 10-repeats of 10-fold CV was also used and 13 models were fit to the data.
#'
#' `ts_example` is from a data set where rolling-origin forecast resampling was used. Each assessment set is the summary of 14 observations (i.e. 2 weeks). The analysis set consisted of a base of about 5,500 samples plus the previous assessment sets. Four regression models were applied to these data.
#'
#' @name precise_example
#' @aliases precise_example
#' @docType data
#' @return Tibbles with the additonal class `rset`
#' @keywords datasets
#' @examples
#' data(precise_example)
#' precise_example
NULL

#' @name noisy_example
#' @rdname precise_example
#' @aliases noisy_example
#' @docType data
#' @keywords datasets
NULL

#' @name concrete_example
#' @rdname precise_example
#' @aliases concrete_example
#' @docType data
#' @keywords datasets
NULL

#' @name ts_example
#' @rdname precise_example
#' @aliases ts_example
#' @docType data
#' @keywords datasets
NULL
