#' Estimate the Difference Between Models
#' 
#' The posterior distributions created by [Bayes_resample()] can
#'  be used to obtain the posterior distribution of the difference(s)
#'  between models. One or more comparisons can be computed at
#'  the same time. 
#'  
#' @param x An object produced by [Bayes_resample()].
#' @param list_1,list_2 Character vectors of equal length that
#'  specify the specific pairwise contrasts. The contrast is
#'  parameterized as `list_1[i] - list_2[i]`. If the defaults
#'  are left to `NULL`, all combinations are evaluated. 
#' @param seed A single integer for sampling from the posterior. 
#' @return A data frame of the posterior distribution(s) of the
#'  difference(s). The object has an extra class of
#'  `"posterior_diff"`.
#'  
#' @details If a transformation was used when `x` was created, the
#'  inverse is applied _before_ the difference is computed. 
#' @export
#' @importFrom purrr map2 map_df
#' @importFrom utils combn
contrast_models <- function(x, list_1 = NULL, list_2 = NULL,
                            seed = sample.int(10000, 1)) {
  if (is.null(list_1) & is.null(list_2)) {
    combos <- combn(x$names, 2)
    list_1 <- combos[1, ]
    list_2 <- combos[2, ]
  } else {
    if (length(list_1) != length(list_2))
      stop("`list_1` and `list_2` should be the same length.",
           call. = FALSE)
  }
  
  models <- purrr::map2(list_1, list_2, make_df, id_vals = x$ids)
  diffs <- 
    purrr::map_df(
      models, 
      make_diffs, 
      obj = x$Bayes_mod, 
      trans = x$transform,
      seed = seed
    )
  class(diffs) <- c("posterior_diff", class(diffs))
  diffs
}

#' Summarize Posterior Distributions of Model Differences
#' 
#' Credible intervals are created for the differences. Also,
#'  region of practical equivalence (ROPE) statistics are computed
#'  when the effective size of a difference is given.
#'
#' @param object An object produced by [contrast_models()].
#' @param prob A number p (0 < p < 1) indicating the desired
#'  probability mass to include in the intervals. 
#' @param size The size of an effective difference. For example, a
#'  5% increase in accuracy between two models might be considered a
#'  "real" difference.
#' @param ... Not currently used
#' @return A data frame with interval and ROPE statistics for each
#'  comparison. 
#' @details The ROPE estimates included in the results are the
#'  columns `pract_neg`, `pract_equiv`, and `pract_pos`. `pract_neg`
#'  integrates the portion of the posterior below `-size` (and
#'  `pract_pos` is the upper integral starting at `size`). The
#'  interpretation depends on whether the metric being analyzed is
#'  better when larger or smaller. `pract_equiv` integrates between
#'  `[-size, size]`. If this is close to one, the two models are
#'  unlikely to be practically different relative to `size`.
#' @export
#' @importFrom dplyr mutate rename group_by summarise full_join %>%
summary.posterior_diff <- function(object, prob = 0.90, size = 0, ...) {
  object <- object %>% 
    dplyr::mutate(contrast = paste(model_1, model_2, sep = " vs ")) %>%
    dplyr::rename(posterior = difference)
  post_int <- object %>%
    dplyr::group_by(contrast) %>% 
    dplyr::do(postint.data.frame(., prob = prob))
  post_stats <- object %>% 
    dplyr::group_by(contrast) %>% 
    dplyr::summarise(probability = mean(posterior > 0),
                     mean = mean(posterior)) %>%
    dplyr::full_join(post_int, by = c("contrast")) 
  if(size != 0) {
    rope_stats <- object %>% 
      dplyr::group_by(contrast) %>% 
      dplyr::summarise(size = size, 
                       pract_neg = mean(posterior < -size),
                       pract_equiv = mean(posterior >= -size & posterior <= size),
                       pract_pos = mean(posterior > size))
  } else 
    rope_stats <- object %>% 
    dplyr::group_by(contrast) %>% 
    dplyr::summarise(size = size, 
                     pract_neg = NA,
                     pract_equiv = NA,
                     pract_pos = NA)
  dplyr::full_join(post_stats, rope_stats, by = c("contrast")) 
}


#' Visualize the Posterior Distributions of Model Differences
#' 
#' A density is created for each contrast in a facetted grid.
#'
#' @param data An object produced by [contrast_models()]. 
#' @param mapping,...,environment Not currently used. 
#' @param size The size of an effective difference. For example, a
#'  5% increase in accuracy between two models might be considered a
#'  "real" difference. 
#' @return A [ggplot2::ggplot()] object. 
#' @export
#' @importFrom ggplot2 ggplot geom_line xlab ylab facet_grid geom_vline
#' @importFrom stats reorder
ggplot.posterior_diff <- 
  function (data, mapping = NULL, ..., environment = NULL, size = 0) {
    out <- ggplot(as.data.frame(data), 
                  aes(x = difference)) + 
      geom_line(stat = "density", trim = TRUE) +
      ylab("Posterior Probability")
    if(length(unique(paste0(data$model_1, data$model_2))) > 1)
      out <- out  + facet_grid(model_2 ~ model_1) 
    if(size != 0) 
      out <- out + 
        geom_vline(xintercept = c(-size, size), lty = 2, alpha = .5)
    out
  }

#' @importFrom dplyr bind_cols
make_df <- function(a, b, id_vals = NULL) {
  new_dat <- data.frame(model = c(a, b))
  as.data.frame(lapply(id_vals, function(x) rep(x[1], nrow(new_dat)))) %>%
    dplyr::bind_cols(new_dat)
}

make_diffs <- function(spec, obj, trans, seed) {
  res_1 <- posterior_linpred(
    obj, 
    newdata = spec[1,],
    seed = seed,
    re.form = NA,
    transform = TRUE
  )
  res_1 <- trans$inv(res_1[,1])
  res_2 <- posterior_linpred(
    obj, 
    newdata = spec[2,],
    seed = seed,
    re.form = NA,
    transform = TRUE
  )
  res_2 <- trans$inv(res_2[,1])
  res <- data.frame(difference = res_1 - res_2,
                    model_1 = as.character(spec$model[1]),
                    model_2 = as.character(spec$model[2]),
                    stringsAsFactors = FALSE)
  res
}


#' @importFrom utils globalVariables
utils::globalVariables(c("contrast", "difference", "model_1", "model_2"))

