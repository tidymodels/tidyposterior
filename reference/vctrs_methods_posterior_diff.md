# Extra methods for the `posterior_diff` class to work with dplyr verbs

Objects with class `posterior_diff` are defined to be tibbles with
required columns `difference` (numeric) and character columns `model_1`,
`model_2`, and `contrast`. If operations on these objects break those
rules, they are down-cast to basic tibbles.

## Usage

``` r
vec_restore.posterior_diff(x, to, ...)

vec_proxy.posterior_diff(x, ...)

vec_ptype2.posterior_diff.posterior_diff(x, y, ..., x_arg = "", y_arg = "")

vec_ptype2.posterior_diff.tbl_df(x, y, ..., x_arg = "", y_arg = "")

vec_ptype2.tbl_df.posterior_diff(x, y, ..., x_arg = "", y_arg = "")

vec_ptype2.posterior_diff.data.frame(x, y, ..., x_arg = "", y_arg = "")

vec_ptype2.data.frame.posterior_diff(x, y, ..., x_arg = "", y_arg = "")

vec_cast.posterior_diff.posterior_diff(x, to, ..., x_arg = "", to_arg = "")

vec_cast.posterior_diff.tbl_df(x, to, ..., x_arg = "", to_arg = "")

vec_cast.tbl_df.posterior_diff(x, to, ..., x_arg = "", to_arg = "")

vec_cast.posterior_diff.data.frame(x, to, ..., x_arg = "", to_arg = "")

vec_cast.data.frame.posterior_diff(x, to, ..., x_arg = "", to_arg = "")
```
