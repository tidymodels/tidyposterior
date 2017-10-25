library(tidyposterior)
library(rsample)
library(testthat)

set.seed(4633)
test_bt <- bootstraps(mtcars, times = 10)
test_bt$one <- rnorm(nrow(test_bt), mean = 10)
test_bt$two <- rnorm(nrow(test_bt), mean = 12)
test_bt$three <- rnorm(nrow(test_bt), mean = 14)

fit_bt <- Bayes_resample(test_bt, seed = 781, 
                         chains = 2, iter = 100, 
                         verbose = FALSE)


## Regression test expected values from initial run
## on 01/24/17 using rstanarm_2.15.3, Rcpp_0.12.13,
## rstan_2.16.2, rstantools_1.3.0, StanHeaders_2.16.0-1,
## seed = 3666

no_rope_90 <- structure(
  list(
    contrast = c("one vs three", "one vs two", "two vs three"), 
    probability = c(0, 0.1, 0.08), 
    mean = c(-4.12051862225756, -1.6788094239894, -1.7876188931085), 
    lower = c(-5.72821597426092, -3.88577310109503, -3.92561746525354), 
    upper = c(-2.17987663652163, 0.418565665138206, 0.580691049056751), 
    size = c(0, 0, 0), 
    pract_neg = c(NA, NA, NA), 
    pract_equiv = c(NA, NA, NA), 
    pract_pos = c(NA, NA, NA )
  ), 
  class = c("tbl_df", "tbl", "data.frame"), 
  row.names = c(NA, -3L), 
  .Names = c("contrast", "probability", "mean", "lower", 
             "upper", "size", "pract_neg", "pract_equiv", 
             "pract_pos")
)

no_rope_99 <- structure(
  list(
    contrast = c("one vs three", "one vs two", "two vs three"), 
    probability = c(0, 0.1, 0.08), 
    mean = c(-4.12051862225756, -1.6788094239894, -1.7876188931085), 
    lower = c(-6.71677384510621, -4.99016146379666, -5.03614719046164), 
    upper = c(-1.58542110599456, 2.21176294776361, 0.87660952438492), 
    size = c(0, 0, 0), 
    pract_neg = c(NA, NA, NA), 
    pract_equiv = c(NA, NA, NA), 
    pract_pos = c(NA, NA, NA )
  ), 
  class = c("tbl_df", "tbl", "data.frame"), 
  row.names = c(NA, -3L), 
  .Names = c("contrast", "probability", "mean", "lower", 
             "upper", "size", "pract_neg", "pract_equiv", 
             "pract_pos")
)

rope_1_90 <- structure(
  list(
    contrast = c("one vs three", "one vs two", "two vs three"), 
    probability = c(0, 0.1, 0.08), 
    mean = c(-4.12051862225756, -1.6788094239894, -1.7876188931085), 
    lower = c(-5.72821597426092, -3.88577310109503, -3.92561746525354), 
    upper = c(-2.17987663652163, 0.418565665138206, 0.580691049056751), 
    size = c(1, 1, 1), 
    pract_neg = c(1, 0.72, 0.76), 
    pract_equiv = c(0, 0.25, 0.24), 
    pract_pos = c(0, 0.03, 0)
  ), 
  class = c("tbl_df", "tbl", "data.frame"), 
  row.names = c(NA, -3L), 
  .Names = c("contrast", "probability", "mean", "lower", 
             "upper", "size", "pract_neg", "pract_equiv", 
             "pract_pos")
)

###################################################################

test_that('bad args', {
  expect_error(contrast_models(fit_bt, "one", c("two", "three")))
})

test_that('basics', {
  comp <- contrast_models(fit_bt)
  expect_equal(nrow(comp), 3* 100)
  expect_equal(names(comp), c("difference", "model_1", "model_2"))
})

# see https://github.com/tidyverse/dplyr/issues/2751
test_that('summary object', {
  set.seed(3666)
  expect_equal(as.data.frame(summary(contrast_models(fit_bt))), 
               as.data.frame(no_rope_90))
  set.seed(3666)
  expect_equal(as.data.frame(summary(contrast_models(fit_bt), prob = .99)), 
               as.data.frame(no_rope_99))
  set.seed(3666)
  expect_equal(as.data.frame(summary(contrast_models(fit_bt), size = 1)), 
               as.data.frame(rope_1_90))
})


