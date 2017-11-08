library(tidyposterior)
library(rsample)
library(testthat)

###################################################################

set.seed(4633)
test_bt <- bootstraps(mtcars, times = 10)
test_bt$one <- rnorm(nrow(test_bt), mean = 10)
test_bt$two <- rnorm(nrow(test_bt), mean = 12)

set.seed(4633)
test_rcv <- vfold_cv(mtcars, v = 5, repeats = 2)
test_rcv$one <- rnorm(nrow(test_rcv), mean = 10)
test_rcv$two <- rnorm(nrow(test_rcv), mean = 12)

fit_bt <- perf_mod(test_bt, seed = 781, 
                   chains = 2, iter = 50, 
                   verbose = FALSE)

tidy_bt <- tidy(fit_bt, seed = 647)

###################################################################

test_that('reproducibility', {
  expect_equal(tidy(fit_bt, seed = 647), tidy_bt)
})

###################################################################

test_that('basic object', {
  expect_equal(sort(unique(tidy_bt$model)), c("one", "two"))
  expect_equal(nrow(tidy_bt), 100)
})

###################################################################

test_that('ggplot object', {
  p <- ggplot(tidy_bt)
  expect_true(inherits(p, "ggplot"))
  expect_equal(nrow(p$data), 100)
})

