# NA

If you are filing a bug, make sure these boxes are checked before
submitting your issue— thank you!

Start a new R session

Install the latest version of of the package:
`update.packages(oldPkgs="tidyposterior", ask=FALSE)`

[Write a minimal reproducible
example](http://stackoverflow.com/a/5963610)

run [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) and add
the results to the issue. Even better would be to use the
[`sessioninfo`](https://github.com/r-lib/sessioninfo) package’s
`session_info()`.

### Minimal, reproducible example:

**Please read this page**: [reprex = {repr}oducible
{ex}ample](https://github.com/jennybc/reprex#what-is-a-reprex)

Text and example code modified from [the R FAQ on
stackoverflow](http://stackoverflow.com/a/5963610)

#### *Minimal* Reproducible Dataset:

If you are not using a data set in a package then use
e.g. [`dput()`](https://rdrr.io/r/base/dput.html) to give us something
that can be put in R immediately, e.g. 

``` r

dput(head(iris,4))
```

Without a dataset, there usually isn’t much that we can do to help.

If your data frame has a factor with many levels, the `dput` output can
be unwieldy because it will still list all the possible factor levels
even if they aren’t present in the the subset of your data. To solve
this issue, you can use the
[`droplevels()`](https://rdrr.io/r/base/droplevels.html) function.
Notice below how species is a factor with only one level:
`dput(droplevels(head(iris, 4)))`.

#### Minimal, runnable code:

``` r

library(tidyposterior)
library(dplyr)

data(precise_example)

accuracy <- precise_example |> select(id, contains("Accuracy"))
accuracy <- setNames(accuracy, tolower(gsub("_Accuracy$", "", names(accuracy)))) 
accuracy

acc_model <- perf_mod(accuracy, seed = 13311, verbose = FALSE)   
```

### Session Info:

``` r

sessionInfo()

# or sessioninfo::session_info()
```

Be sure to test your chunks of code in an empty R session before
submitting your issue!
