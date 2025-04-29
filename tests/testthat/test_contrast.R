library(tidyposterior)
library(rsample)
library(testthat)

# ------------------------------------------------------------------------------

set.seed(4633)
test_bt <- bootstraps(mtcars, times = 10)
test_bt$one <- rnorm(nrow(test_bt), mean = 10)
test_bt$two <- rnorm(nrow(test_bt), mean = 12)
test_bt$three <- rnorm(nrow(test_bt), mean = 14)

fit_bt <- perf_mod(test_bt, seed = 781, chains = 2, iter = 100, verbose = FALSE)

contr_obj <- contrast_models(fit_bt, seed = 3666)

# ------------------------------------------------------------------------------

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    contrast_models(fit_bt, "one", c("two", "three"))
  )
})

test_that("basics", {
  expect_equal(nrow(contr_obj), 3 * 100)
  expect_true(inherits(contr_obj, "tbl_df"))
  expect_true(inherits(contr_obj, "posterior_diff"))
  expect_equal(
    names(contr_obj),
    c("difference", "model_1", "model_2", "contrast")
  )
  expect_true(is.character(contr_obj$model_1))
  expect_true(is.character(contr_obj$model_2))
  expect_true(is.character(contr_obj$contrast))
  expect_true(is.numeric(contr_obj$difference))
})

# ------------------------------------------------------------------------------

test_that("reproducibility", {
  expect_equal(contrast_models(fit_bt, seed = 3666), contr_obj)
})

# ------------------------------------------------------------------------------

# TODO test for dplyr compatability

# ------------------------------------------------------------------------------

test_that("autoplot for contrasts", {
  p_1 <- autoplot(contrast_models(fit_bt, seed = 3666))
  expect_s3_class(p_1, "ggplot")
  expect_equal(
    names(p_1$data),
    c("difference", "model_1", "model_2", "contrast")
  )
  expect_equal(rlang::get_expr(p_1$mapping$x), rlang::expr(difference))
  expect_equal(rlang::get_expr(p_1$mapping$y), NULL)
  expect_true("model_1" %in% names(as.list(p_1$facet)$params$cols))
  expect_true("model_2" %in% names(as.list(p_1$facet)$params$rows))
  expect_equal(as.character(p_1$labels$y), "Posterior Probability")
  expect_equal(as.character(p_1$labels$x), "difference")
})
