library(dplyr)

data("ex_objects", package = "tidyposterior")

keep_post <- function(x) inherits(x, "posterior")
keep_diff <- function(x) inherits(x, "posterior_diff")

# ------------------------------------------------------------------------------

test_that("keep posterior class", {
  expect_true(keep_post(posterior_samples |> dplyr::select(posterior, model)))
  expect_true(keep_post(posterior_samples |> dplyr::filter(posterior > median(posterior_samples$posterior))))
  expect_true(keep_post(posterior_samples |> mutate(hey = "you")))
  expect_true(keep_post(posterior_samples |> mutate(hey = "you") |> select(-hey)))
  expect_true(keep_post(posterior_samples |> arrange(model)))
  expect_true(keep_post(posterior_samples |> inner_join(tibble(model = "cart"), by = "model")))
  expect_true(keep_post(posterior_samples |> full_join(tibble(model = "cart"), by = "model")))
  expect_true(keep_post(posterior_samples |> left_join(tibble(model = "cart"), by = "model")))
  expect_true(keep_post(posterior_samples |> right_join(tibble(model = "cart"), by = "model")))
  expect_true(keep_post(posterior_samples |> anti_join(tibble(model = "cart"), by = "model")))
})

test_that("drop posterior class", {
  skip_if(tidyposterior:::dplyr_pre_1.0.0())
  expect_false(keep_post(posterior_samples |> select(-model)))
  expect_false(keep_post(posterior_samples[, -2]))
})

# ------------------------------------------------------------------------------

test_that("keep posterior_diff class", {
  expect_true(keep_diff(contrast_samples |> dplyr::select(difference, model_1, model_2, contrast)))
  expect_true(keep_diff(contrast_samples |> dplyr::filter(difference > median(contrast_samples$difference))))
  expect_true(keep_diff(contrast_samples |> mutate(hey = "you")))
  expect_true(keep_diff(contrast_samples |> mutate(hey = "you") |> select(-hey)))
  expect_true(keep_diff(contrast_samples |> arrange(model_2)))
  expect_true(keep_diff(contrast_samples |> inner_join(tibble(model_2 = "cart"), by = "model_2")))
  expect_true(keep_diff(contrast_samples |> full_join(tibble(model_2 = "cart"), by = "model_2")))
  expect_true(keep_diff(contrast_samples |> left_join(tibble(model_2 = "cart"), by = "model_2")))
  expect_true(keep_diff(contrast_samples |> right_join(tibble(model_2 = "cart"), by = "model_2")))
  expect_true(keep_diff(contrast_samples |> anti_join(tibble(model_2 = "cart"), by = "model_2")))
})

test_that("drop posterior_diff class", {
  skip_if(tidyposterior:::dplyr_pre_1.0.0())
  expect_false(keep_diff(contrast_samples |> select(-model_2)))
  expect_false(keep_diff(contrast_samples[, -2]))
})
