library(tidyposterior)
library(testthat)

###################################################################

test_that('files exist', {
  roc_file <- system.file("examples", "roc_model.RData", package = "tidyposterior")
  diff_file <- system.file("examples", "glm_v_nnet.RData", package = "tidyposterior")
  cat(roc_file, "\n", diff_file, "\n", sep = "")
  expect_true(file.exists(roc_file))
  expect_true(file.exists(roc_file))  
})

###################################################################

test_that('load files and check type', {
  roc_file <- system.file("examples", "roc_model.RData", package = "tidyposterior")
  diff_file <- system.file("examples", "glm_v_nnet.RData", package = "tidyposterior")
  expect_equal(load(roc_file), "roc_model")
  expect_equal(load(diff_file), "glm_v_nnet")
  
  expect_equal(class(roc_model), "perf_mod")
  expect_equal(class(glm_v_nnet), c("posterior_diff", "data.frame"))
})

