#' @importFrom dplyr filter select mutate %>% rename group_by summarise full_join
#' @importFrom dplyr do %>% bind_cols
#' @importFrom rsample gather.rset pretty.group_vfold_cv
#' @importFrom rsample pretty.bootstraps pretty.nested_cv
#' @importFrom rsample pretty.mc_cv pretty.rolling_origin
#' @importFrom rsample pretty.loo_cv pretty.vfold_cv vfold_cv
#' @importFrom rstanarm stan_glmer posterior_epred posterior_interval
#' @importFrom rlang !! is_string na_dbl
#' @importFrom stats setNames reorder as.formula median quantile
#' @importFrom purrr map_chr map map2 map_df
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot geom_violin xlab ylab geom_line geom_vline
#' @importFrom ggplot2 facet_grid
#' @importFrom utils combn globalVariables
#' @importFrom tibble is_tibble as_tibble

# ------------------------------------------------------------------------------

utils::globalVariables(c("contrast", "difference", "model_1", "model_2"))
utils::globalVariables(c(".", "aes", "posterior", ".metric"))
utils::globalVariables(c("id", "model", "splits", "statistic", "Resample"))
utils::globalVariables(c(".config", ".estimate", ".lower", ".upper",
                         "pract_equiv", "sub_model", "wflow_id", "workflow"))
