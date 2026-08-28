#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom rlang !!
#' @importFrom rlang %||%
#' @importFrom rlang .data
#' @importFrom rlang is_string
#' @importFrom rlang na_dbl
#' @importFrom rsample vfold_cv
#' @importFrom rstanarm posterior_epred
#' @importFrom rstanarm stan_glmer
#' @importFrom stats as.formula
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#' @importFrom utils combn
## usethis namespace: end
NULL

# nocov start

# Global vars ------------------------------------------------------------------

utils::globalVariables(
  c(
    ".",
    ".config",
    ".estimate",
    ".lower",
    ".metric",
    ".upper",
    "aes",
    "contrast",
    "difference",
    "engine",
    "id",
    "label",
    "leader",
    "lower",
    "lower_diff",
    "mean_diff",
    "model",
    "model_1",
    "model_2",
    "model_type",
    "posterior",
    "pr_worse",
    "pract_equiv",
    "pract_neg",
    "pract_pos",
    "pract_worse",
    "probability",
    "Resample",
    "size",
    "splits",
    "statistic",
    "sub_model",
    "upper",
    "upper_diff",
    "wflow_id",
    "workflow"
  )
)

# nocov end
