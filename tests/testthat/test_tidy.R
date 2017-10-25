library(tidyposterior)
library(rsample)
library(testthat)

set.seed(4633)
test_bt <- bootstraps(mtcars, times = 10)
test_bt$one <- rnorm(nrow(test_bt), mean = 10)
test_bt$two <- rnorm(nrow(test_bt), mean = 12)

set.seed(4633)
test_rcv <- vfold_cv(mtcars, v = 5, repeats = 2)
test_rcv$one <- rnorm(nrow(test_rcv), mean = 10)
test_rcv$two <- rnorm(nrow(test_rcv), mean = 12)

fit_bt <- Bayes_resample(test_bt, seed = 781, 
                         chains = 2, iter = 50, 
                         verbose = FALSE)
set.seed(647)
tidy_bt <- tidy(fit_bt)

## Regression test expected values from initial run
## on 01/24/17 using rstanarm_2.15.3, Rcpp_0.12.13,
## rstan_2.16.2, rstantools_1.3.0, StanHeaders_2.16.0-1
## and seed 647

post_int_90 <- structure(
  list(model = c("one", "two"), 
       mean = c(9.10805518150591, 10.8262925767279), 
       lower = c(7.28272101749654, 9.13029524769803), 
       upper = c(10.8742598880652, 12.6513539228596)), 
  class = c("tbl_df", "tbl", "data.frame"), 
  row.names = c(NA, -2L), 
  .Names = c("model", "mean", "lower", "upper"))
post_int_99 <- structure(
  list(model = c("one", "two"), 
       mean = c(9.10805518150591, 10.8262925767279), 
       lower = c(6.47521602125946, 8.22174687320194), 
       upper = c(11.1440035413268, 13.6692043619728)), 
  class = c("tbl_df","tbl", "data.frame"), 
  row.names = c(NA, -2L), 
  .Names = c("model", "mean", "lower", "upper"))
  

###################################################################

test_that('basic object', {
  expect_equal(sort(unique(tidy_bt$model)), c("one", "two"))
  expect_equal(nrow(tidy_bt), 100)
})

###################################################################

# see https://github.com/tidyverse/dplyr/issues/2751
test_that('summary object', {
  expect_equal(as.data.frame(summary(tidy_bt)), as.data.frame(post_int_90))
  expect_equal(as.data.frame(summary(tidy_bt, prob = .99)), as.data.frame(post_int_99))
})

###################################################################

test_that('ggplot object', {
  p <- ggplot(tidy_bt)
  expect_true(inherits(p, "ggplot"))
  expect_equal(nrow(p$data), 100)
})

