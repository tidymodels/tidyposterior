## run fits outside of test functions
## https://github.com/stan-dev/rstanarm/issues/202

if (rlang::is_installed("rstan")) {
  cv_data <- initialize_cv_data()
  rcv_data <- initialize_rcv_data()
  p4_data <- initialize_p4_data()

  init_het <- stan_glmer_inits(
    statistic ~ model + (model + 0 | id),
    data = cv_data,
    seed = 31
  )
  fit_het <- initialize_test_fit(
    statistic ~ model + (model + 0 | id),
    cv_data,
    init_het
  )

  init_hom <- stan_glmer_inits(
    statistic ~ model + (1 | id),
    data = cv_data,
    seed = 31
  )
  fit_hom <- initialize_test_fit(
    statistic ~ model + (1 | id),
    cv_data,
    init_hom
  )

  init_nst <- stan_glmer_inits(
    statistic ~ model + (1 | id2 / id),
    data = rcv_data,
    seed = 31
  )
  fit_nst <- initialize_test_fit(
    statistic ~ model + (1 | id2 / id),
    rcv_data,
    init_nst
  )

  init_p4 <- stan_glmer_inits(
    statistic ~ model + (model + 0 | id),
    data = p4_data,
    seed = 31
  )
  fit_p4 <- initialize_test_fit(
    statistic ~ model + (model + 0 | id),
    p4_data,
    init_p4
  )

  pm_fit <- suppressWarnings(
    perf_mod(
      initialize_cv_wide(),
      hetero_var = TRUE,
      initialize = TRUE,
      seed = 31,
      chains = 2,
      iter = 200,
      refresh = 0
    )
  )
}

test_that("heterogeneous variance starting values have the right structure", {
  inits <- stan_glmer_inits(
    statistic ~ model + (model + 0 | id),
    data = initialize_cv_data(),
    seed = 1
  )
  expect_s3_class(inits, "stan_glmer_inits")
  base <- attr(inits, "inits")
  expect_named(
    base,
    c("gamma", "z_beta", "aux_unscaled", "tau", "zeta", "rho", "z_T", "z_b")
  )
  expect_true(is.array(base$gamma))
  expect_length(base$gamma, 1)
  expect_length(base$z_beta, 2)
  expect_length(base$aux_unscaled, 1)
  expect_true(is.array(base$tau))
  expect_length(base$tau, 1)
  expect_length(base$zeta, 3)
  expect_length(base$rho, 2)
  expect_true(all(base$rho >= 0.005 & base$rho <= 0.995))
  expect_length(base$z_T, 2)
  # one value per coefficient and level, plus rstanarm's "_NEW_" level
  expect_length(base$z_b, 3 * (10 + 1))
  expect_true(all(base$tau > 0))
  expect_true(all(base$zeta > 0))
  expect_true(base$aux_unscaled > 0)
})

test_that("homogeneous starting values have the right structure", {
  inits <- stan_glmer_inits(
    statistic ~ model + (1 | id),
    data = initialize_cv_data(),
    seed = 1
  )
  base <- attr(inits, "inits")
  expect_named(base, c("gamma", "z_beta", "aux_unscaled", "tau", "z_b"))
  expect_length(base$tau, 1)
  expect_length(base$z_b, 10 + 1)
})

test_that("nested homogeneous starting values have the right structure", {
  inits <- stan_glmer_inits(
    statistic ~ model + (1 | id2 / id),
    data = initialize_rcv_data(),
    seed = 1
  )
  base <- attr(inits, "inits")
  expect_named(base, c("gamma", "z_beta", "aux_unscaled", "tau", "z_b"))
  expect_length(base$tau, 2)
  expect_length(base$z_b, (10 + 1) + (2 + 1))
  # terms are ordered with the most levels first, as in the Stan model
  term_info <- attr(inits, "term_info")
  expect_equal(term_info[[1]]$name, "id:id2")
  expect_equal(term_info[[1]]$nlev, 10)
  expect_equal(term_info[[2]]$name, "id2")
  expect_equal(term_info[[2]]$nlev, 2)
})

test_that("nested heterogeneous starting values can be computed", {
  # rstanarm cannot fit this model for balanced resamples (the number of
  # random effects equals the number of observations) but the values still
  # have the layout that the Stan model declares
  inits <- stan_glmer_inits(
    statistic ~ model + (model + 0 | id2 / id),
    data = initialize_rcv_data(),
    seed = 1
  )
  base <- attr(inits, "inits")
  expect_length(base$tau, 2)
  expect_length(base$zeta, 6)
  expect_length(base$rho, 4)
  expect_length(base$z_T, 4)
  expect_length(base$z_b, 3 * (10 + 1) + 3 * (2 + 1))
})

test_that("four model starting values pad z_T to the declared length", {
  inits <- stan_glmer_inits(
    statistic ~ model + (model + 0 | id),
    data = initialize_p4_data(),
    seed = 1
  )
  base <- attr(inits, "inits")
  expect_length(base$z_beta, 3)
  expect_length(base$zeta, 4)
  expect_length(base$rho, 3)
  # the Stan model declares (p - 1) * (p - 2) values but consumes 5
  expect_length(base$z_T, 6)
  expect_length(base$z_b, 4 * (10 + 1))
})

test_that("formula parsing matches the lme4 reference layouts", {
  # the fixture was generated from lme4::lFormula() by the script in
  # inst/generate_lme4_layouts.R; see that file to recreate or extend it
  reference <- readRDS(test_path("fixtures", "lme4-layouts.rds"))

  as_reference <- function(layout) {
    list(
      X = unname(layout$X),
      terms = lapply(layout$terms, function(x) {
        list(
          name = x$name,
          coef_names = x$coef_names,
          levels = levels(x$fct),
          codes = as.integer(x$fct),
          design = unname(x$mm)
        )
      })
    )
  }

  cv_data <- initialize_cv_data()
  rcv_data <- initialize_rcv_data()
  layouts <- list(
    hetero = glmer_init_layout(statistic ~ model + (model + 0 | id), cv_data),
    homogeneous = glmer_init_layout(statistic ~ model + (1 | id), cv_data),
    nested = glmer_init_layout(statistic ~ model + (1 | id2 / id), rcv_data),
    hetero_nested = glmer_init_layout(
      statistic ~ model + (model + 0 | id2 / id),
      rcv_data
    ),
    four_models = glmer_init_layout(
      statistic ~ model + (model + 0 | id),
      initialize_p4_data()
    )
  )

  expect_named(layouts, names(reference))
  for (structure in names(reference)) {
    expect_equal(
      as_reference(layouts[[structure]]),
      reference[[structure]],
      ignore_attr = TRUE,
      label = paste0("parsed layout for '", structure, "'"),
      expected.label = "the lme4 reference"
    )
  }
})

test_that("starting values reproduce the empirical estimates", {
  cv_data <- initialize_cv_data()
  inits <- stan_glmer_inits(
    statistic ~ model + (model + 0 | id),
    data = cv_data,
    seed = 1
  )
  base <- attr(inits, "inits")
  sd_y <- sd(cv_data$statistic)

  lm_fit <- lm(statistic ~ model, data = cv_data)
  expect_equal(unname(base$gamma[1]), mean(cv_data$statistic))

  x_sd <- apply(model.matrix(lm_fit)[, -1, drop = FALSE], 2, sd)
  adj_scale <- 2.5 * sd_y / x_sd
  expect_equal(
    unname(as.vector(base$z_beta) * adj_scale),
    unname(coef(lm_fit)[-1]),
    tolerance = 1e-8
  )

  sig_hat <- max(
    sigma(lm(statistic ~ model + id, data = cv_data)),
    0.01 * sd_y
  )
  expect_equal(unname(base$aux_unscaled), sig_hat / sd_y, tolerance = 1e-8)

  # mapping the standardized random effect values through the covariance
  # Cholesky factor recovers the per-cell deviations from the group means
  devs <- cv_data$statistic - fitted(lm_fit)
  b_hat <- tapply(devs, list(cv_data$id, cv_data$model), mean)
  T_mat <- theta_L_forward(
    p = 3,
    tau = base$tau[1],
    zeta = base$zeta,
    rho = base$rho,
    z_T = base$z_T,
    sigma_hat = sig_hat
  )
  recon <- t(T_mat %*% matrix(base$z_b[1:30], nrow = 3))
  expect_equal(unname(recon), unname(as.matrix(b_hat)), tolerance = 1e-8)
})

test_that("chains are jittered, reproducible, and do not disturb the RNG", {
  args <- list(
    formula = statistic ~ model + (model + 0 | id),
    data = initialize_cv_data(),
    seed = 5
  )
  init_1 <- do.call(stan_glmer_inits, args)
  init_2 <- do.call(stan_glmer_inits, args)

  # same seed, same chain: identical
  expect_equal(init_1(1), init_2(1))
  # different chains: every jittered element differs
  chain_1 <- init_1(1)
  chain_2 <- init_1(2)
  for (param in names(chain_1)) {
    expect_true(all(chain_1[[param]] != chain_2[[param]]))
  }

  # no jitter: identical to the base values for every chain
  init_0 <- do.call(stan_glmer_inits, c(args, list(jitter = 0)))
  expect_equal(init_0(1), attr(init_0, "inits"))
  expect_equal(init_0(1), init_0(2))

  # large jitter respects the parameter constraints
  init_big <- do.call(stan_glmer_inits, c(args, list(jitter = 10)))
  vals <- init_big(1)
  expect_true(all(vals$rho >= 0.005 & vals$rho <= 0.995))
  expect_true(all(vals$tau > 0))
  expect_true(all(vals$zeta > 0))
  expect_true(vals$aux_unscaled > 0)

  # calling the function restores the RNG state
  set.seed(42)
  before <- .Random.seed
  invisible(init_1(1))
  expect_identical(before, .Random.seed)
})

test_that("blocks limit which parameters are initialized", {
  cv_data <- initialize_cv_data()
  init_fixed_aux <- stan_glmer_inits(
    statistic ~ model + (model + 0 | id),
    data = cv_data,
    seed = 1,
    blocks = c("fixed", "aux")
  )
  expect_named(
    attr(init_fixed_aux, "inits"),
    c("gamma", "z_beta", "aux_unscaled")
  )

  init_ranef <- stan_glmer_inits(
    statistic ~ model + (model + 0 | id),
    data = cv_data,
    seed = 1,
    blocks = "ranef"
  )
  expect_named(
    attr(init_ranef, "inits"),
    c("tau", "zeta", "rho", "z_T", "z_b")
  )
})

test_that("stan_glmer uses the starting values", {
  skip_if_not_installed("rstan")

  fits <- list(
    hetero = list(fit = fit_het, inits = init_het),
    homogeneous = list(fit = fit_hom, inits = init_hom),
    nested = list(fit = fit_nst, inits = init_nst),
    four_models = list(fit = fit_p4, inits = init_p4)
  )
  for (structure in names(fits)) {
    used <- rstan::get_inits(fits[[structure]]$fit$stanfit)
    for (chain in 1:2) {
      supplied <- fits[[structure]]$inits(chain)
      expect_equal(
        used[[chain]][names(supplied)],
        supplied,
        tolerance = 1e-6,
        ignore_attr = TRUE,
        label = paste0(structure, ", chain ", chain, ", rstan"),
        expected.label = "supplied values"
      )
    }
  }
})

test_that("starting value inputs are checked", {
  cv_data <- initialize_cv_data()
  expect_snapshot(
    error = TRUE,
    stan_glmer_inits(~ model + (1 | id), data = cv_data)
  )
  expect_snapshot(
    error = TRUE,
    stan_glmer_inits(statistic ~ model, data = cv_data)
  )
  expect_snapshot(
    error = TRUE,
    stan_glmer_inits(statistic ~ model + 0 + (1 | id), data = cv_data)
  )
  expect_snapshot(
    error = TRUE,
    stan_glmer_inits(statistic ~ model + (1 | id), data = as.matrix(cv_data))
  )
  expect_snapshot(
    error = TRUE,
    stan_glmer_inits(statistic ~ potato + (1 | id), data = cv_data)
  )
  missing_y <- cv_data
  missing_y$statistic[3] <- NA
  expect_snapshot(
    error = TRUE,
    stan_glmer_inits(statistic ~ model + (1 | id), data = missing_y)
  )
  missing_id <- cv_data
  missing_id$id[5] <- NA
  expect_snapshot(
    error = TRUE,
    stan_glmer_inits(statistic ~ model + (1 | id), data = missing_id)
  )
  constant_y <- cv_data
  constant_y$statistic <- 1
  expect_snapshot(
    error = TRUE,
    stan_glmer_inits(statistic ~ model + (1 | id), data = constant_y)
  )
  expect_snapshot(
    error = TRUE,
    stan_glmer_inits(statistic ~ model + (1 | id), data = cv_data, jitter = -1)
  )
  expect_snapshot(
    error = TRUE,
    stan_glmer_inits(statistic ~ model + (1 | id), data = cv_data, seed = 1:2)
  )
  expect_snapshot(
    error = TRUE,
    stan_glmer_inits(
      statistic ~ model + (1 | id),
      data = cv_data,
      blocks = "potato"
    )
  )
})

test_that("starting value prints", {
  inits <- stan_glmer_inits(
    statistic ~ model + (model + 0 | id),
    data = initialize_cv_data(),
    seed = 1
  )
  expect_snapshot(print(inits))
})

test_that("perf_mod can create data-based starting values", {
  skip_if_not_installed("rstan")

  expect_s3_class(pm_fit, "perf_mod")
  # perf_mod() reuses the sampler seed for the starting values, so they can
  # be reproduced with a manual call on the same long-format data
  long_data <- tidyr::pivot_longer(
    initialize_cv_wide(),
    cols = c(-id),
    names_to = "model",
    values_to = "statistic"
  )
  manual <- stan_glmer_inits(
    statistic ~ model + (model + 0 | id),
    data = long_data,
    seed = 31
  )
  used <- rstan::get_inits(pm_fit$stan$stanfit)
  for (chain in 1:2) {
    supplied <- manual(chain)
    expect_equal(
      used[[chain]][names(supplied)],
      supplied,
      tolerance = 1e-6,
      ignore_attr = TRUE
    )
  }
})

test_that("perf_mod initialization arguments are checked", {
  cv_wide <- initialize_cv_wide()
  expect_snapshot(
    error = TRUE,
    perf_mod(cv_wide, initialize = "yes")
  )
  expect_snapshot(
    error = TRUE,
    perf_mod(cv_wide, initialize = TRUE, init = "random")
  )
  expect_snapshot(
    error = TRUE,
    perf_mod(cv_wide, initialize = TRUE, family = poisson)
  )
  expect_snapshot(
    error = TRUE,
    perf_mod(cv_wide, initialize = TRUE, family = "poisson")
  )
})
