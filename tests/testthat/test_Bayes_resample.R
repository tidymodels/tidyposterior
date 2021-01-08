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

## emulate caret::resamples object from 10-fold

rs_obj <- list(
  methods = c(one = "lm", two = "rpart"),
  values =  as.data.frame(test_bt[, -1]),
  metrics = "blah"
)
colnames(rs_obj$values) <- c("Resample", "one~blah", "two~blah")
rs_obj$values$Resample <- vfold_cv(mtcars)$id
class(rs_obj) <- "resamples"

rs_rcv <- rs_obj
rs_rcv$values$Resample <-
  paste0("Fold", rep(1:5, 2), ".", "Rep", rep(1:2, each = 5))

## run fits outside of test functions
## https://github.com/stan-dev/rstanarm/issues/202

obj_1 <- perf_mod(test_bt, seed = 781,
                  chains = 2, iter = 50,
                  verbose = FALSE)

test_df <- as.data.frame(test_bt[, -1])
obj_2 <- perf_mod(test_df, seed = 781,
                  chains = 2, iter = 50,
                  verbose = FALSE)

obj_3 <- perf_mod(test_bt, seed = 781,
                  chains = 2, iter = 50,
                  verbose = FALSE,
                  hetero_var = TRUE)

obj_4 <- perf_mod(rs_obj, seed = 781,
                  chains = 2, iter = 50,
                  verbose = FALSE)

obj_5 <- perf_mod(rs_rcv, seed = 781,
                  chains = 2, iter = 50,
                  verbose = FALSE)

obj_6 <- perf_mod(test_rcv, seed = 781,
                  chains = 2, iter = 50,
                  verbose = FALSE)

# ------------------------------------------------------------------------------

test_that('bad arguments', {
  expect_error(perf_mod(test_bt, transform = NULL))
  expect_error(perf_mod(test_bt, transform = no_trans[1]))
  expect_error(perf_mod(test_bt, transform = list(not = 1, right = 2)))
  expect_error(perf_mod(test_bt, transform = list(func = 1, inc = 2)))
  expect_error(perf_mod(1:10))
})

# ------------------------------------------------------------------------------

test_that('basic usage', {
  expect_equal(obj_1$names, c("one", "two"))
  expect_equal(obj_1$ids, list(id = c(paste0("Bootstrap0", 1:9), "Bootstrap10")))
  expect_equal(obj_1$rset_type, "Bootstrap sampling")
  expect_equal(class(obj_1$stan), c('stanreg', 'glm', 'lm', 'lmerMod'))
  expect_equal(formula(obj_1$stan), as.formula(statistic ~ model + (1 | id)))
  expect_output(print(obj_1))
  expect_equal(summary(obj_1), summary(obj_1$stan))
})

# ------------------------------------------------------------------------------

test_that('data frame method', {
  expect_equal(obj_2$names, c("one", "two"))
  expect_equal(obj_2$ids, list(id = c(paste0("Bootstrap0", 1:9), "Bootstrap10")))
  expect_equal(obj_2$rset_type, NA)
  expect_equal(class(obj_2$stan), c('stanreg', 'glm', 'lm', 'lmerMod'))
  expect_equal(formula(obj_2$stan), as.formula(statistic ~ model + (1 | id)))
  expect_output(print(obj_2))
  expect_equal(summary(obj_2), summary(obj_2$stan))
})

# ------------------------------------------------------------------------------

test_that('model-specifc variance', {
  expect_equal(formula(obj_3$stan), as.formula(statistic ~model + (model + 0 | id)))
})

# ------------------------------------------------------------------------------

test_that('rsample method', {
  expect_equal(obj_4$names, c("one", "two"))
  expect_equal(obj_4$ids, list(id = c(paste0("Fold0", 1:9), "Fold10")))
  expect_equal(obj_4$rset_type, NA)
  expect_equal(class(obj_4$stan), c('stanreg', 'glm', 'lm', 'lmerMod'))
  expect_equal(formula(obj_4$stan), as.formula(statistic ~ model + (1 | id)))
  expect_output(print(obj_4))
  expect_equal(summary(obj_4), summary(obj_4$stan))
})


test_that('rsample method with repeated cv', {
  expect_true(tidyposterior:::is_repeated_cv(rs_rcv$values))
  expect_equal(obj_5$names, c("one", "two"))
  expect_equal(obj_5$rset_type, "5-fold cross-validation repeated 2 times")
  expect_equal(class(obj_5$stan), c('stanreg', 'glm', 'lm', 'lmerMod'))
  expect_equal(formula(obj_5$stan),
               as.formula(statistic ~ model + (1 | id2/id)))
  expect_output(print(obj_5))
  expect_equal(summary(obj_5), summary(obj_5$stan))
})

# ------------------------------------------------------------------------------

test_that('repeated v_fold method', {
  expect_equal(obj_6$names, c("one", "two"))
  expect_equal(obj_6$ids,
               list(
                 id = paste0("Repeat", 1:2),
                 id2 = paste0("Fold", 1:5)
               )
  )
  expect_equal(obj_6$rset_type, "5-fold cross-validation repeated 2 times")
  expect_equal(class(obj_6$stan), c('stanreg', 'glm', 'lm', 'lmerMod'))
  expect_equal(formula(obj_6$stan),
               as.formula(statistic ~ model + (1 | id2/id)))
  expect_output(print(obj_6))
  expect_equal(summary(obj_6), summary(obj_6$stan))
})

# ------------------------------------------------------------------------------

test_that('printing', {
  expect_output(print(obj_1), "Bayesian Analysis of Resampling Results")
})


test_that('summary', {
  expect_true(inherits(summary(obj_1), "summary.stanreg"))
})


test_that('postint', {
  expect_equal(
    tidyposterior:::postint.numeric(2),
    data.frame(lower = 2, upper = 2)
  )
  expect_equal(
    tidyposterior:::postint.data.frame(tidy(obj_1)),
    data.frame(lower = 9.52393870753461, upper = 12.3900327798712),
    tol = 0.01
  )
})



test_that('autoplots', {
  p_1 <- autoplot(obj_1)
  expect_s3_class(p_1, "ggplot")
  expect_equal(
    names(p_1$data),
    c("model", "posterior")
  )
  expect_equal(rlang::get_expr(p_1$mapping$x), rlang::expr(posterior))
  expect_equal(rlang::get_expr(p_1$mapping$y), NULL)
  expect_equal(rlang::get_expr(p_1$mapping$colour), rlang::expr(model))
  expect_equal(as.list(p_1$facet)$params, list())
  expect_equal(as.character(p_1$labels$y), "density")
  expect_equal(as.character(p_1$labels$x), "posterior")


  p_2 <- autoplot(tidy(obj_1))
  expect_s3_class(p_2, "ggplot")
  expect_equal(
    names(p_2$data),
    c("model", "posterior")
  )
  expect_equal(rlang::get_expr(p_2$mapping$x), rlang::expr(posterior))
  expect_equal(rlang::get_expr(p_2$mapping$y), NULL)
  expect_equal(rlang::get_expr(p_2$mapping$colour), rlang::expr(model))
  expect_equal(as.list(p_2$facet)$params, list())
  expect_equal(as.character(p_2$labels$y), "density")
  expect_equal(as.character(p_2$labels$x), "posterior")
})

