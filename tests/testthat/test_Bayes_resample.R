library(tidyposterior)
library(rsample)
library(testthat)

set.seed(4633)
test_cv <- vfold_cv(mtcars)
test_cv$one <- rnorm(nrow(test_cv), mean = 10)
test_cv$two <- rnorm(nrow(test_cv), mean = 12)

###################################################################

test_that('bad arguments', {
  expect_error(Bayes_resample(test_cv, transform = NULL))
  expect_error(Bayes_resample(test_cv, transform = no_trans[1]))  
  expect_error(Bayes_resample(test_cv, transform = list(not = 1, right = 2))) 
  expect_error(Bayes_resample(test_cv, transform = list(func = 1, inc = 2))) 
  expect_error(Bayes_resample(1:10))
})

###################################################################

test_that('basic usage', {
  obj_1 <- Bayes_resample(test_cv, seed = 781, 
                          chains = 2, iter = 50, 
                          verbose = FALSE)
  expect_equal(obj_1$names, c("one", "two"))
  expect_equal(obj_1$ids, list(id = c(paste0("Fold0", 1:9), "Fold10")))
  expect_equal(obj_1$rset_type, "10-fold cross-validation")  
  expect_equal(class(obj_1$Bayes_mod), c('stanreg', 'glm', 'lm', 'lmerMod'))
  expect_equal(formula(obj_1$Bayes_mod), as.formula(statistic ~ model + (1 | id)))
  expect_output(print(obj_1))
  expect_equal(summary(obj_1), summary(obj_1$Bayes_mod))
})

