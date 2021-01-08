# tidyposterior (development version)

* The `ggplot()` methods are now removed in factor of `autoplot()` methods. 

# `tidyposterior` 0.0.3

* `contrast_models()` now returns a tibble and has an extra column called `contrast`. 

* The plot methods are now deprecated and will be removed in the next version. They are not very good and can be replaced with simple `ggplot` code.  

* An optional formula argument was added to `perf_mod.rset()` and `perf_mod.data.frame()`. When the resampling method has multiple ID columns, a nested data structure is assumed (with a warning). The new `formula` argument can be used to over-ride the nesting.  

## Breaking Changes

* Methods for compatibility with `dplyr` 1.0.0. For the newer `dplyr` version, if critical columns for `posterior` or `posterior_diff` objects are removed, the objects is down-graded to a tibble. For earlier versions of `dplyr`, the object is not down-cast. 


# `tidyposterior` 0.0.2

A small, maintenance release. 

## Minor bug fixes and improvements

* Moved from the `broom` package to the `generics` package to get the `tidy` generic. 

* `ggplot2` was moved to Suggests

* The sole `tidy` method was more explicitly exported so that the `generics` man files show the method. 

* The large RData objects containing the examples have been removed from the package and are accessible via a link to the GitHub repo. 

# `tidyposterior` 0.0.1

* First CRAN submission
