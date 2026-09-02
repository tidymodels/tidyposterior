# Extra methods for the posterior class to work with dplyr verbs

Objects with class `posterior` are defined to be tibbles with required
columns `model` (character) and `posterior` (numeric). If operations on
these objects break those rules, they are down-cast to basic tibbles.

## Usage

``` r
vec_restore.posterior(x, to, ...)

vec_proxy.posterior(x, ...)

vec_ptype2.posterior.posterior(x, y, ..., x_arg = "", y_arg = "")

vec_ptype2.posterior.tbl_df(x, y, ..., x_arg = "", y_arg = "")

vec_ptype2.tbl_df.posterior(x, y, ..., x_arg = "", y_arg = "")

vec_ptype2.posterior.data.frame(x, y, ..., x_arg = "", y_arg = "")

vec_ptype2.data.frame.posterior(x, y, ..., x_arg = "", y_arg = "")

vec_cast.posterior.posterior(x, to, ..., x_arg = "", to_arg = "")

vec_cast.posterior.tbl_df(x, to, ..., x_arg = "", to_arg = "")

vec_cast.tbl_df.posterior(x, to, ..., x_arg = "", to_arg = "")

vec_cast.posterior.data.frame(x, to, ..., x_arg = "", to_arg = "")

vec_cast.data.frame.posterior(x, to, ..., x_arg = "", to_arg = "")
```
