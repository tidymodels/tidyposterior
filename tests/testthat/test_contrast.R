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

contr_obj <- contrast_models(fit_bt, seed = 3666)

## Regression test expected values from initial run created with
## seed 647. See `test_contrast_session` in file below for versions
## seed = 3666

load("../test_contrast_obj.RData")

###################################################################

test_that('bad args', {
  expect_error(contrast_models(fit_bt, "one", c("two", "three")))
})

test_that('basics', {
  expect_equal(nrow(contr_obj), 3* 100)
  expect_equal(names(contr_obj), c("difference", "model_1", "model_2"))
})

# see https://github.com/tidyverse/dplyr/issues/2751
test_that('reproducibility and summary calcs', {
  post_90_obs <- summary(contr_obj)
  post_90_obs <- as.data.frame(post_90_obs)
  expect_equal(post_90_obs, fit_bt_post_int_90)
  
  post_99_obs <- summary(contr_obj, prob = .99)
  post_99_obs <- as.data.frame(post_99_obs)
  expect_equal(post_99_obs, fit_bt_post_int_99)

  post_int_rope_1_obs <- summary(contr_obj, size = 1)
  post_int_rope_1_obs <- as.data.frame(post_int_rope_1_obs)
  expect_equal(post_int_rope_1_obs, fit_bt_post_int_rope_1)
})


