## run fits outside of test functions
## https://github.com/stan-dev/rstanarm/issues/202
if (rlang::is_installed(c("parsnip", "yardstick"))) {
  library(rsample)
  library(parsnip)
  library(workflowsets)
  library(testthat)
  library(yardstick)

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
    values = as.data.frame(test_bt[, -1]),
    metrics = "blah"
  )
  colnames(rs_obj$values) <- c("Resample", "one~blah", "two~blah")
  rs_obj$values$Resample <- vfold_cv(mtcars)$id
  class(rs_obj) <- "resamples"

  rs_rcv <- rs_obj
  rs_rcv$values$Resample <-
    paste0("Fold", rep(1:5, 2), ".", "Rep", rep(1:2, each = 5))

  obj_1 <- perf_mod(
    test_bt,
    seed = 781,
    chains = 2,
    iter = 1000,
    refresh = 0,
    verbose = FALSE
  )

  test_df <- as.data.frame(test_bt[, -1])
  obj_2 <- perf_mod(
    test_df,
    seed = 781,
    refresh = 0,
    chains = 2,
    iter = 1000,
    verbose = FALSE
  )

  obj_3 <- perf_mod(
    test_bt,
    seed = 781,
    chains = 2,
    iter = 1000,
    refresh = 0,
    verbose = FALSE,
    hetero_var = TRUE
  )

  obj_4 <- perf_mod(
    rs_obj,
    seed = 781,
    chains = 2,
    iter = 1000,
    refresh = 0,
    verbose = FALSE
  )

  obj_5 <- perf_mod(
    rs_rcv,
    seed = 781,
    chains = 2,
    iter = 1000,
    verbose = FALSE
  )

  obj_6 <- perf_mod(
    test_rcv,
    seed = 781,
    chains = 2,
    iter = 1000,
    refresh = 0,
    verbose = FALSE
  )
}
# ------------------------------------------------------------------------------

test_that("bad arguments", {
  expect_snapshot(error = TRUE, perf_mod(test_bt, transform = NULL))
  expect_snapshot(error = TRUE, perf_mod(test_bt, transform = no_trans[1]))
  expect_snapshot(
    error = TRUE,
    perf_mod(test_bt, transform = list(not = 1, right = 2))
  )
  expect_snapshot(
    error = TRUE,
    perf_mod(test_bt, transform = list(func = 1, inc = 2))
  )
  expect_snapshot(error = TRUE, perf_mod(1:10))
})

# ------------------------------------------------------------------------------

test_that("basic usage", {
  skip_if_not_installed(c("parsnip"))
  skip_if_not_installed(c("yardstick"))

  expect_equal(obj_1$names, c("one", "two"))
  expect_equal(
    obj_1$ids,
    list(id = c(paste0("Bootstrap0", 1:9), "Bootstrap10"))
  )
  expect_equal(obj_1$rset_type, "Bootstrap sampling")
  expect_equal(class(obj_1$stan), c("stanreg", "glm", "lm", "lmerMod"))
  expect_equal(
    formula(obj_1$stan),
    as.formula(statistic ~ model + (1 | id)),
    ignore_formula_env = TRUE
  )
  expect_snapshot(print(obj_1))
  expect_equal(summary(obj_1), summary(obj_1$stan))
})

# ------------------------------------------------------------------------------

test_that("data frame method", {
  skip_if_not_installed(c("parsnip"))
  skip_if_not_installed(c("yardstick"))

  expect_equal(obj_2$names, c("one", "two"))
  expect_equal(
    obj_2$ids,
    list(id = c(paste0("Bootstrap0", 1:9), "Bootstrap10"))
  )
  expect_equal(obj_2$rset_type, NA)
  expect_equal(class(obj_2$stan), c("stanreg", "glm", "lm", "lmerMod"))
  expect_equal(
    formula(obj_2$stan),
    as.formula(statistic ~ model + (1 | id)),
    ignore_formula_env = TRUE
  )
  expect_snapshot(print(obj_2))
  expect_equal(summary(obj_2), summary(obj_2$stan))
})

# ------------------------------------------------------------------------------

test_that("model-specifc variance", {
  skip_if_not_installed(c("parsnip"))
  skip_if_not_installed(c("yardstick"))

  expect_equal(
    formula(obj_3$stan),
    as.formula(statistic ~ model + (model + 0 | id)),
    ignore_formula_env = TRUE
  )
})

# ------------------------------------------------------------------------------

test_that("rsample method", {
  skip_if_not_installed(c("parsnip"))
  skip_if_not_installed(c("yardstick"))

  expect_equal(obj_4$names, c("one", "two"))
  expect_equal(obj_4$ids, list(id = c(paste0("Fold0", 1:9), "Fold10")))
  expect_equal(obj_4$rset_type, NA)
  expect_equal(class(obj_4$stan), c("stanreg", "glm", "lm", "lmerMod"))
  expect_equal(
    formula(obj_4$stan),
    as.formula(statistic ~ model + (1 | id)),
    ignore_formula_env = TRUE
  )
  expect_snapshot(print(obj_4))
  expect_equal(summary(obj_4), summary(obj_4$stan))
})


test_that("rsample method with repeated cv", {
  skip_if_not_installed(c("parsnip"))
  skip_if_not_installed(c("yardstick"))

  expect_true(tidyposterior:::is_repeated_cv(rs_rcv$values))
  expect_equal(obj_5$names, c("one", "two"))
  expect_equal(obj_5$rset_type, "5-fold cross-validation repeated 2 times")
  expect_equal(class(obj_5$stan), c("stanreg", "glm", "lm", "lmerMod"))
  expect_equal(
    formula(obj_5$stan),
    as.formula(statistic ~ model + (1 | id2 / id)),
    ignore_formula_env = TRUE
  )
  expect_snapshot(print(obj_5))
  expect_equal(summary(obj_5), summary(obj_5$stan))
})

# ------------------------------------------------------------------------------

test_that("repeated v_fold method", {
  skip_if_not_installed(c("parsnip"))
  skip_if_not_installed(c("yardstick"))

  expect_equal(obj_6$names, c("one", "two"))
  expect_equal(
    obj_6$ids,
    list(
      id = paste0("Repeat", 1:2),
      id2 = paste0("Fold", 1:5)
    )
  )
  expect_equal(obj_6$rset_type, "5-fold cross-validation repeated 2 times")
  expect_equal(class(obj_6$stan), c("stanreg", "glm", "lm", "lmerMod"))
  expect_equal(
    formula(obj_6$stan),
    as.formula(statistic ~ model + (1 | id2 / id)),
    ignore_formula_env = TRUE
  )
  expect_snapshot(print(obj_6))
  expect_equal(summary(obj_6), summary(obj_6$stan))
})

# ------------------------------------------------------------------------------

test_that("summary", {
  skip_if_not_installed(c("parsnip"))
  skip_if_not_installed(c("yardstick"))

  expect_true(inherits(summary(obj_1), "summary.stanreg"))
})


test_that("postint", {
  skip_if_not_installed(c("parsnip"))
  skip_if_not_installed(c("yardstick"))

  expect_equal(
    tidyposterior:::postint.numeric(2),
    data.frame(lower = 2, upper = 2)
  )
  expect_equal(
    tidyposterior:::postint.data.frame(tidy(obj_1)),
    data.frame(lower = 9.52393870753461, upper = 12.3900327798712),
    tolerance = 0.01
  )
})

test_that("autoplots", {
  skip_if_not_installed(c("parsnip"))
  skip_if_not_installed(c("yardstick"))

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

# ------------------------------------------------------------------------------

test_that("workflow sets", {
  skip_if_not_installed(c("parsnip"))
  skip_if_not_installed(c("yardstick"))

  lm_spec <- linear_reg() |> set_engine("lm")
  set.seed(10)
  bt <- bootstraps(mtcars, times = 10)
  wset <-
    workflow_set(
      list(
        one = mpg ~ I(1 / sqrt(disp)),
        half = mpg ~ cyl + I(1 / sqrt(disp)) + hp + drat + wt,
        all = mpg ~ .
      ),
      list(lm = lm_spec)
    ) |>
    workflow_map("fit_resamples", resamples = bt, seed = 1)

  expect_no_error(
    rsq_mod <- perf_mod(wset, seed = 3, refresh = 0, metric = "rsq")
  )
  expect_equal(
    colnames(coef(rsq_mod$stan)$id),
    c("(Intercept)", "modelhalf_lm", "modelone_lm")
  )
  expect_equal(
    unique(tidy(rsq_mod)$model),
    c("one_lm", "half_lm", "all_lm")
  )

  p_tidy <- autoplot(rsq_mod, type = "posteriors")
  expect_s3_class(p_tidy, "ggplot")
  expect_equal(
    names(p_tidy$data),
    c("model", "posterior")
  )
  expect_equal(rlang::get_expr(p_tidy$mapping$x), rlang::expr(posterior))
  expect_equal(rlang::get_expr(p_tidy$mapping$colour), rlang::expr(model))
  expect_equal(as.list(p_tidy$facet$params), list())
  expect_equal(as.character(p_tidy$labels$x), "rsq")
  expect_equal(as.character(p_tidy$labels$colour), "model")
  expect_equal(as.character(p_tidy$labels$y), "density")
  expect_equal(as.character(p_tidy$labels$fill), "fill")

  p_int <- autoplot(rsq_mod, type = "intervals")
  expect_s3_class(p_int, "ggplot")
  expect_equal(
    names(p_int$data),
    c("workflow", ".lower", ".estimate", ".upper", "rank")
  )
  expect_equal(rlang::get_expr(p_int$mapping$x), rlang::expr(rank))
  expect_equal(rlang::get_expr(p_int$mapping$y), rlang::expr(.estimate))
  expect_equal(rlang::get_expr(p_int$mapping$colour), rlang::expr(workflow))
  expect_equal(as.list(p_tidy$facet$params), list())
  expect_equal(as.character(p_int$labels$x), "Workflow Rank")
  expect_equal(as.character(p_int$labels$y), "rsq")
  expect_equal(as.character(p_int$labels$colour), "workflow")
  expect_equal(as.character(p_int$labels$ymin), ".lower")
  expect_equal(as.character(p_int$labels$ymax), ".upper")

  p_rope <- autoplot(rsq_mod, type = "ROPE", size = .1)
  expect_s3_class(p_rope, "ggplot")
  expect_equal(
    names(p_rope$data),
    c("model", "pract_equiv", "rank", "workflow")
  )
  expect_equal(rlang::get_expr(p_rope$mapping$x), rlang::expr(rank))
  expect_equal(rlang::get_expr(p_rope$mapping$y), rlang::expr(pract_equiv))
  expect_equal(as.list(p_tidy$facet$params), list())
  expect_equal(as.character(p_rope$labels$x), "Workflow Rank")
  expect_equal(
    as.character(p_rope$labels$y),
    "Probability of Practical Equivalence"
  )
  expect_equal(as.character(p_rope$labels$colour), "workflow")
})
