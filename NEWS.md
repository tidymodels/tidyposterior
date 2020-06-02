# `tidyposterior` 0.0.3

* Methods for compatibility with `dplyr` 1.0.0

* `contrast_models()` now returns a tibble and has an extra column called `contrast`. 

* The plot methods are now deprecated and will be removed in the next version. They are not very good and can be replaced with simple `ggplot` code.  

# `tidyposterior` 0.0.2

A small, maintenance release. 

## Minor bug fixes and improvements

* Moved from the `broom` package to the `generics` package to get the `tidy` generic. 

* `ggplot2` was moved to Suggests

* The sole `tidy` method was more explicitly exported so that the `generics` man files show the method. 

* The large RData objects containing the examples have been removed from the package and are accessible via a link to the GitHub repo. 

# `tidyposterior` 0.0.1

* First CRAN submission
