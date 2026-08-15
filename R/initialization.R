#' Data-based starting values for Bayesian resampling models
#'
#' [stan_glmer_inits()] computes starting values for the Bayesian hierarchical
#' models used by [perf_mod()] from group-level sample statistics. The results
#' can be passed to the `init` argument of [rstanarm::stan_glmer()] (directly
#' or through `perf_mod(..., init = )`) and can substantially shorten warmup
#' for models that are slow to converge, especially those with heterogeneous
#' group variances.
#' @keywords internal
#' @param formula A two-sided formula with a single numeric outcome, fixed
#'  effects, and at least one random effect term (in the `lme4::lmer()`
#'  syntax used by [rstanarm::stan_glmer()]). This should be the same formula
#'  that will be used to fit the model. See Details for the supported
#'  structures.
#' @param data A data frame with the columns used in `formula`; the same data
#'  that will be given to [rstanarm::stan_glmer()]. No missing values are
#'  allowed in the columns that are used.
#' @param seed A single integer controlling the per-chain random jittering.
#' @param jitter A single non-negative number for how much the starting
#'  values are perturbed for each chain. Location parameters are shifted by
#'  Gaussian noise with this standard deviation (on their natural,
#'  standardized scale) and scale parameters are multiplied by a log-normal
#'  factor with this standard deviation. A value of zero gives every chain
#'  identical starting values (not recommended for diagnostics).
#' @param blocks A character vector for which groups of parameters get
#'  data-based starting values: `"fixed"` (the intercept and fixed effect
#'  coefficients), `"aux"` (the residual standard deviation), and/or
#'  `"ranef"` (the random effect values and their covariance parameters).
#'  Parameters outside of `blocks` retain \pkg{rstan}'s random initialization.
#' @return A function of `chain_id` (with class `"stan_glmer_inits"`) that
#'  returns a named list of starting values for the model's underlying Stan
#'  parameters. \pkg{rstan} invokes it once per chain. The unjittered values
#'  are stored in the `"inits"` attribute.
#' @details
#' The function is designed for the model structures created by [perf_mod()]:
#'
#' \preformatted{
#'   statistic ~ model + (model + 0 | id)   # heterogeneous variances
#'   statistic ~ model + (1 | id)           # homogeneous variances
#'   statistic ~ model + (1 | id2/id)       # homogeneous, repeated CV
#' }
#'
#' Other formulas with a fixed-effect intercept and factor-based random
#' effect terms may also work but are not tested. Note that the
#' heterogeneous-variance model with nested resamples
#' (`statistic ~ model + (model + 0 | id2/id)`) cannot be fit by
#' [rstanarm::stan_glmer()] for balanced resampling data because the number
#' of random effects equals the number of observations.
#'
#' Starting values are estimated with ordinary least squares: the fixed
#' effects come from a linear model, the random effect values are per-level
#' summaries of its residuals, and their covariance matrix initializes the
#' correlation/variance decomposition used by \pkg{rstanarm}. The residual
#' standard deviation is estimated after removing additive grouping-factor
#' effects and is floored at 1\% of the outcome's standard deviation; when
#' there is one observation per model/resample combination, this residual
#' scale is confounded with the random effects, so the value is a heuristic.
#'
#' The computations assume the model will be fit with `family = gaussian()`
#' (the [perf_mod()] default), no case weights, `QR = FALSE`, and the default
#' \pkg{rstanarm} priors (which automatically rescale using the data). With
#' non-default priors, the starting values are still usable but are no longer
#' on the intended scale.
#'
#' The **same** `formula` and `data` (after any outcome transformation) must
#' be given to this function and to [rstanarm::stan_glmer()]; otherwise the
#' starting values may silently correspond to the wrong random effect levels.
#' The easiest way to use these starting values with [perf_mod()] is its
#' `initialize = TRUE` argument, which computes and applies them
#' automatically. To call this function manually (e.g., to control `blocks`
#' or `jitter`), recreate the internal data format of [perf_mod()]: the
#' resampling results are stacked into a data frame with a `statistic` column
#' (with the `transform$func` applied), a `model` column, and the resampling
#' id column(s) (see the example below).
#'
#' Data-based starting values do not uniformly reduce sampling time. For
#' models that already converge quickly they can add modest overhead, while
#' for slow-converging fits (e.g., `hetero_var = TRUE`) they can remove the
#' occasional very slow chain that started in a poor region of the parameter
#' space. They also tend to reduce the number of divergent transitions, but
#' divergences caused by the model geometry require increasing
#' `adapt_delta`, not better starting values.
#' @seealso [perf_mod()], [rstanarm::stan_glmer()]
#' @examples
#' data(precise_example)
#'
#' # The data in the format used by perf_mod():
#' roc_data <-
#'   precise_example |>
#'   dplyr::select(id, dplyr::contains("ROC")) |>
#'   tidyr::pivot_longer(
#'     cols = c(-id),
#'     names_to = "model",
#'     values_to = "statistic"
#'   ) |>
#'   dplyr::mutate(model = gsub("_ROC", "", model))
#'
#' inits <- stan_glmer_inits(
#'   statistic ~ model + (model + 0 | id),
#'   data = roc_data,
#'   seed = 1
#' )
#' inits
#'
#' # The starting values for the second chain:
#' str(inits(2))
#'
#' \donttest{
#' fit <- rstanarm::stan_glmer(
#'   statistic ~ model + (model + 0 | id),
#'   data = roc_data,
#'   init = inits,
#'   refresh = 0,
#'   seed = 2
#' )
#' }
#' @export
stan_glmer_inits <- function(
  formula,
  data,
  seed = sample.int(10^5, 1),
  jitter = 0.1,
  blocks = c("fixed", "aux", "ranef")
) {
  blocks <- rlang::arg_match(blocks, multiple = TRUE)
  check_inits_inputs(formula, data, seed, jitter)

  layout <- glmer_init_layout(formula, data)

  fixed <- fixed_inits(layout$y, layout$X, layout$sd_y)
  sig_hat <- sigma_init(layout$y, layout$X, layout$terms, layout$sd_y)
  b_hats <- ranef_estimates(fixed$resid, layout$terms)

  decov <- vector("list", length(layout$terms))
  z_b <- vector("list", length(layout$terms))
  for (i in seq_along(layout$terms)) {
    decov[[i]] <- decov_inits(b_hats[[i]], sig_hat)
    T_mat <- theta_L_forward(
      p = layout$terms[[i]]$p,
      tau = decov[[i]]$tau,
      zeta = decov[[i]]$zeta,
      rho = decov[[i]]$rho,
      z_T = decov[[i]]$z_T,
      sigma_hat = sig_hat
    )
    z_b[[i]] <- z_b_inits(b_hats[[i]], T_mat)
  }

  base <- assemble_inits(fixed, sig_hat, decov, z_b, layout$sd_y, blocks)

  term_info <- lapply(
    layout$terms,
    function(x) list(name = x$name, p = x$p, nlev = nlevels(x$fct))
  )
  new_stan_glmer_inits(
    base = base,
    seed = seed,
    jitter = jitter,
    sd_y = layout$sd_y,
    blocks = blocks,
    formula = formula,
    term_info = term_info
  )
}

check_inits_inputs <- function(formula, data, seed, jitter) {
  if (!inherits(formula, "formula") || length(formula) != 3) {
    rlang::abort("`formula` should be a two-sided formula.")
  }
  if (!is.data.frame(data)) {
    rlang::abort("`data` should be a data frame.")
  }
  missing_vars <- setdiff(all.vars(formula), names(data))
  if (length(missing_vars) > 0) {
    rlang::abort(
      paste0(
        "Some variables in `formula` are not in `data`: ",
        paste0("'", missing_vars, "'", collapse = ", "),
        "."
      )
    )
  }
  has_na <- vapply(data[all.vars(formula)], anyNA, logical(1))
  if (any(has_na)) {
    rlang::abort(
      paste0(
        "There are missing values in column(s) ",
        paste0("'", names(has_na)[has_na], "'", collapse = ", "),
        ". Starting values require complete data; otherwise the random ",
        "effect values may not align with the levels used in the model fit."
      )
    )
  }
  if (!rlang::is_scalar_integerish(seed) || is.na(seed)) {
    rlang::abort("`seed` should be a single integer.")
  }
  bad_jitter <-
    !is.numeric(jitter) ||
    length(jitter) != 1 ||
    !is.finite(jitter) ||
    jitter < 0
  if (bad_jitter) {
    rlang::abort("`jitter` should be a single non-negative number.")
  }
  invisible(NULL)
}

# Split the right-hand side of the formula into fixed effect terms and
# random effect "bar" terms such as `(model + 0 | id)`.
split_bar_terms <- function(formula) {
  bars <- list()
  fixed <- list()
  walk <- function(expr) {
    if (is.call(expr)) {
      if (identical(expr[[1]], as.name("+")) && length(expr) == 3) {
        walk(expr[[2]])
        walk(expr[[3]])
        return(invisible(NULL))
      }
      is_paren_bar <-
        identical(expr[[1]], as.name("(")) &&
        is.call(expr[[2]]) &&
        identical(expr[[2]][[1]], as.name("|"))
      if (is_paren_bar) {
        bars[[length(bars) + 1]] <<- expr[[2]]
        return(invisible(NULL))
      }
      if (identical(expr[[1]], as.name("|"))) {
        bars[[length(bars) + 1]] <<- expr
        return(invisible(NULL))
      }
    }
    fixed[[length(fixed) + 1]] <<- expr
    invisible(NULL)
  }
  walk(formula[[3]])
  list(bars = bars, fixed = fixed)
}

# Expand nested grouping such as `id2/id` the way that lme4 does: an effect
# for the outer factor plus one for the inner factor crossed with it (which
# lme4 labels `id:id2`).
expand_nesting <- function(group) {
  if (is.call(group) && identical(group[[1]], as.name("/"))) {
    outer_groups <- expand_nesting(group[[2]])
    deepest <- outer_groups[[length(outer_groups)]]
    c(outer_groups, list(call(":", group[[3]], deepest)))
  } else {
    list(group)
  }
}

# Evaluate a grouping expression as a factor, dropping unused levels the way
# that lme4 does. Crossed factors (e.g. `id:id2`) use the standard `:`
# operator, which orders the levels with the rightmost factor varying
# fastest.
eval_group_factor <- function(group, data) {
  vars <- all.vars(group)
  cols <- lapply(data[vars], function(x) if (is.factor(x)) x else as.factor(x))
  factor(eval(group, cols))
}

# Parse the formula the same way that lme4 (and therefore rstanarm) does so
# that the random effect terms have the ordering, coefficient names, and
# level order used by the Stan model: terms are sorted by decreasing number
# of levels, coefficients follow `model.matrix()` column order, and nested
# grouping factors are expanded into crossed factors. This purpose-built
# parser avoids a dependency on lme4 itself; the test suite verifies its
# output against saved reference layouts that were generated with
# `lme4::lFormula()` (see inst/generate_lme4_layouts.R).
glmer_init_layout <- function(formula, data) {
  parts <- split_bar_terms(formula)
  if (length(parts$bars) == 0) {
    rlang::abort(
      paste0(
        "`formula` should contain at least one random effect term (e.g. ",
        "`(model + 0 | id)`) since the starting values are for models ",
        "fit with `rstanarm::stan_glmer()`."
      )
    )
  }

  fixed_rhs <- if (length(parts$fixed) > 0) {
    fixed_chr <- vapply(
      parts$fixed,
      function(x) paste(deparse(x), collapse = " "),
      character(1)
    )
    paste(fixed_chr, collapse = " + ")
  } else {
    "1"
  }
  f_fixed <- as.formula(
    paste(paste(deparse(formula[[2]]), collapse = " "), "~", fixed_rhs)
  )
  mf <- model.frame(f_fixed, data = data)
  X <- model.matrix(attr(mf, "terms"), mf)
  if (!"(Intercept)" %in% colnames(X)) {
    rlang::abort(
      paste0(
        "`formula` should include a fixed-effect intercept; models without ",
        "one use a different parameterization that is not supported."
      )
    )
  }

  y <- model.response(mf)
  if (!is.numeric(y)) {
    rlang::abort("The outcome should be numeric.")
  }
  if (any(!is.finite(y))) {
    rlang::abort("The outcome contains non-finite values.")
  }
  sd_y <- sd(y)
  if (!is.finite(sd_y) || sd_y <= 0) {
    rlang::abort(
      "The outcome has zero variance so no starting values can be computed."
    )
  }

  trms <- list()
  for (bar in parts$bars) {
    lhs_chr <- paste(deparse(bar[[2]]), collapse = " ")
    mm <- model.matrix(as.formula(paste("~", lhs_chr)), data = data)
    for (group in expand_nesting(bar[[3]])) {
      fct <- eval_group_factor(group, data)
      grp_nm <- paste(deparse(group), collapse = "")
      if (nlevels(fct) < 2) {
        rlang::abort(
          paste0("Grouping factor '", grp_nm, "' should have multiple levels.")
        )
      }
      trms[[length(trms) + 1]] <- list(
        name = grp_nm,
        fct = fct,
        mm = mm,
        p = ncol(mm),
        coef_names = colnames(mm)
      )
    }
  }
  # match the Stan model's term ordering: decreasing number of levels
  n_levels <- vapply(trms, function(x) nlevels(x$fct), integer(1))
  trms <- trms[order(-n_levels)]

  list(y = y, X = X, sd_y = sd_y, terms = trms)
}

# Least squares estimates for the fixed effects and their conversion to the
# standardized scale used by the Stan model: with the default priors,
# beta = z_beta * (2.5 * sd(y) / sd(x)) and `gamma` is the intercept after
# mean-centering the predictors (i.e., the outcome grand mean).
fixed_inits <- function(y, X, sd_y) {
  fit <- lm.fit(X, y)
  beta <- fit$coefficients
  beta[is.na(beta)] <- 0

  non_int <- setdiff(colnames(X), "(Intercept)")
  z_beta <- NULL
  if (length(non_int) > 0) {
    x_sd <- apply(X[, non_int, drop = FALSE], 2, sd)
    zero_var <- x_sd <= 0
    if (any(zero_var)) {
      rlang::warn(
        paste0(
          "Fixed effect column(s) ",
          paste0("'", non_int[zero_var], "'", collapse = ", "),
          " have zero variance; their starting values were set to zero."
        )
      )
      x_sd[zero_var] <- 1
      beta[non_int][zero_var] <- 0
    }
    z_beta <- unname(beta[non_int] / (2.5 * sd_y / x_sd))
  }
  list(
    gamma = mean(y),
    z_beta = z_beta,
    resid = unname(y - X %*% matrix(beta, ncol = 1))[, 1]
  )
}

# The residual standard deviation from a fixed effects model that also
# contains additive terms for each grouping factor. When there is one value
# per cell (e.g., one statistic per model/resample), the true residual scale
# is confounded with the random effects and this is a heuristic; the value is
# floored at 1% of the outcome standard deviation.
sigma_init <- function(y, X, trms, sd_y) {
  X_add <- X
  for (tm in trms) {
    dummies <- model.matrix(~f, data.frame(f = tm$fct))[, -1, drop = FALSE]
    X_add <- cbind(X_add, dummies)
  }
  fit <- lm.fit(X_add, y)
  df <- length(y) - fit$rank
  resid_sd <- if (df > 0) sqrt(sum(fit$residuals^2) / df) else 0
  max(resid_sd, 0.01 * sd_y)
}

# Per-level least squares estimates of the random effects, computed from the
# fixed effects residuals. Terms are processed from the fewest levels to the
# most (subtracting each term's estimates from the working residuals) so
# that, with nested factors, the outer factor absorbs the variation that is
# shared by its inner levels. Results are returned in the original term order.
ranef_estimates <- function(resid_fe, trms) {
  est <- vector("list", length(trms))
  r <- resid_fe
  n_levels <- vapply(trms, function(x) nlevels(x$fct), integer(1))
  for (i in order(n_levels)) {
    tm <- trms[[i]]
    lvls <- levels(tm$fct)
    B <- matrix(
      0,
      nrow = length(lvls),
      ncol = tm$p,
      dimnames = list(lvls, tm$coef_names)
    )
    n_empty <- 0
    for (l in seq_along(lvls)) {
      in_lvl <- which(tm$fct == lvls[l])
      if (length(in_lvl) == 0) {
        n_empty <- n_empty + 1
        next
      }
      cf <- lm.fit(tm$mm[in_lvl, , drop = FALSE], r[in_lvl])$coefficients
      cf[is.na(cf)] <- 0
      B[l, ] <- cf
    }
    if (n_empty > 0) {
      rlang::warn(
        paste0(
          n_empty,
          " level(s) of grouping factor '",
          tm$name,
          "' have no data; their starting values were set to zero."
        )
      )
    }
    est[[i]] <- B
    r <- r - rowSums(tm$mm * B[as.integer(tm$fct), , drop = FALSE])
  }
  est
}

# Convert the sample covariance matrix of the random effect estimates to the
# parameters of rstanarm's `decov()` reparameterization: an overall scale
# (`tau`, defined relative to the residual standard deviation), variance
# proportions (`zeta`, a la a simplex), and the correlation structure via an
# onion-method Cholesky factor (`rho` and `z_T`). Note that the Stan model
# declares `z_T` with (p - 1) * (p - 2) values per term but only consumes the
# first 2 + 3 + ... + (p - 1) of them; the remainder is padding.
decov_inits <- function(B, sigma_hat) {
  p <- ncol(B)
  if (p == 1) {
    tau <- max(sd(B[, 1]) / sigma_hat, 0.01)
    return(list(tau = tau, zeta = NULL, rho = NULL, z_T = NULL))
  }

  S <- cov(B)
  trace_S <- sum(diag(S))
  tau <- max(sqrt(trace_S / p) / sigma_hat, 0.01)
  if (trace_S <= 0) {
    S <- diag(p) * (tau * sigma_hat)^2
    trace_S <- sum(diag(S))
  }
  zeta <- pmax(p * diag(S) / trace_S, 0.01)

  S_pd <- S
  diag(S_pd) <- pmax(diag(S_pd), 1e-8 * sigma_hat^2)
  R <- cov2cor(S_pd)
  L <- NULL
  for (ridge in c(0, 1e-3, 1e-2, 1e-1)) {
    R_r <- (1 - ridge) * R + ridge * diag(p)
    L <- tryCatch(t(chol(R_r)), error = function(e) NULL)
    if (!is.null(L)) {
      break
    }
  }
  if (is.null(L)) {
    L <- diag(p)
  }

  rho <- (L[2, 1] + 1) / 2
  z_T <- numeric(0)
  if (p > 2) {
    for (r in 2:(p - 1)) {
      rho <- c(rho, 1 - L[r + 1, r + 1]^2)
      seg <- L[r + 1, 1:r]
      if (all(abs(seg) < 1e-8)) {
        seg <- rep(1e-3, r)
      }
      z_T <- c(z_T, seg)
    }
  }
  rho <- pmin(pmax(rho, 0.005), 0.995)
  n_pad <- (p - 1) * (p - 2) - length(z_T)
  if (n_pad > 0) {
    z_T <- c(z_T, rep(0.01, n_pad))
  }
  list(tau = tau, zeta = unname(zeta), rho = unname(rho), z_T = unname(z_T))
}

# An R replication of the `make_theta_L()` function in rstanarm's Stan code
# that maps the decov parameters to the Cholesky factor of a term's
# covariance matrix. The off-diagonal values in rows >= 3 are scaled by the
# *previous* row's standard deviation, matching the Stan code as written;
# this must not be "corrected" or the z_b starting values will be wrong.
theta_L_forward <- function(p, tau, zeta, rho, z_T, sigma_hat, scale = 1) {
  if (p == 1) {
    return(matrix(tau * scale * sigma_hat, 1, 1))
  }
  pi_ <- zeta / sum(zeta)
  trace_T <- (tau * scale * sigma_hat)^2 * p
  T_i <- matrix(0, p, p)
  T_i[1, 1] <- sqrt(pi_[1] * trace_T)
  std_dev <- sqrt(pi_[2] * trace_T)
  T_21 <- 2 * rho[1] - 1
  T_i[2, 1] <- std_dev * T_21
  T_i[2, 2] <- std_dev * sqrt(1 - T_21^2)
  mark <- 1
  if (p > 2) {
    for (r in 2:(p - 1)) {
      seg <- z_T[mark:(mark + r - 1)]
      mark <- mark + r
      scale_factor <- sqrt(rho[r] / sum(seg^2)) * std_dev
      std_dev <- sqrt(pi_[r + 1] * trace_T)
      T_i[r + 1, 1:r] <- seg * scale_factor
      T_i[r + 1, r + 1] <- sqrt(1 - rho[r]) * std_dev
    }
  }
  T_i
}

# Solve for the standardized random effect values so that, after Stan maps
# them through the term's Cholesky factor, the resulting values equal the
# empirical estimates. rstanarm appends one fictitious "_NEW_" level to each
# grouping factor (used for predictions on new groups); its values are set
# near zero. The layout is level-major: all coefficients for level 1, then
# level 2, and so on.
z_b_inits <- function(B, T_mat) {
  z <- solve(T_mat, t(B))
  c(unname(z), rep(1e-4, nrow(T_mat)))
}

assemble_inits <- function(fixed, sig_hat, decov, z_b, sd_y, blocks) {
  base <- list()
  if ("fixed" %in% blocks) {
    base$gamma <- as.array(fixed$gamma)
    if (length(fixed$z_beta) > 0) {
      base$z_beta <- as.array(fixed$z_beta)
    }
  }
  if ("aux" %in% blocks) {
    base$aux_unscaled <- sig_hat / sd_y
  }
  if ("ranef" %in% blocks) {
    base$tau <- as.array(vapply(decov, function(x) x$tau, numeric(1)))
    zeta <- unlist(lapply(decov, function(x) x$zeta))
    if (length(zeta) > 0) {
      base$zeta <- as.array(zeta)
    }
    rho <- unlist(lapply(decov, function(x) x$rho))
    if (length(rho) > 0) {
      base$rho <- as.array(rho)
    }
    z_T <- unlist(lapply(decov, function(x) x$z_T))
    if (length(z_T) > 0) {
      base$z_T <- as.array(z_T)
    }
    base$z_b <- as.array(unlist(z_b))
  }
  base
}

# Perturb the starting values for a given chain, restoring the RNG state
# afterwards so that calling the function does not affect the caller's
# random number stream.
jitter_inits <- function(base, chain_id, seed, jitter, sd_y) {
  if (jitter <= 0) {
    return(base)
  }
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(
      suppressWarnings(rm(".Random.seed", envir = globalenv())),
      add = TRUE
    )
  }
  set.seed(seed + chain_id)

  shift <- function(x, amount = jitter) {
    x + rnorm(length(x), mean = 0, sd = amount)
  }
  rescale <- function(x) {
    x * exp(rnorm(length(x), mean = 0, sd = jitter))
  }

  out <- base
  if (!is.null(out$gamma)) {
    out$gamma <- shift(out$gamma, jitter * sd_y)
  }
  if (!is.null(out$z_beta)) {
    out$z_beta <- shift(out$z_beta)
  }
  if (!is.null(out$aux_unscaled)) {
    out$aux_unscaled <- rescale(out$aux_unscaled)
  }
  if (!is.null(out$tau)) {
    out$tau <- rescale(out$tau)
  }
  if (!is.null(out$zeta)) {
    out$zeta <- rescale(out$zeta)
  }
  if (!is.null(out$rho)) {
    out$rho <- as.array(pmin(pmax(shift(out$rho), 0.005), 0.995))
  }
  if (!is.null(out$z_T)) {
    out$z_T <- shift(out$z_T)
  }
  if (!is.null(out$z_b)) {
    out$z_b <- shift(out$z_b)
  }
  out
}

# The returned function deliberately closes over only small objects (the
# list of starting values and a few scalars), not the original data, so that
# it is cheap to send to parallel workers when `cores > 1`.
new_stan_glmer_inits <- function(
  base,
  seed,
  jitter,
  sd_y,
  blocks,
  formula,
  term_info
) {
  env <- rlang::new_environment(
    data = list(base = base, seed = seed, jitter = jitter, sd_y = sd_y),
    parent = rlang::ns_env("tidyposterior")
  )
  fn <- rlang::new_function(
    args = rlang::pairlist2(chain_id = ),
    body = quote(jitter_inits(base, chain_id, seed, jitter, sd_y)),
    env = env
  )
  structure(
    fn,
    class = c("stan_glmer_inits", "function"),
    inits = base,
    seed = seed,
    jitter = jitter,
    blocks = blocks,
    formula = paste(deparse(formula), collapse = " "),
    term_info = term_info
  )
}

# Used by perf_mod() when `initialize = TRUE`: create data-based starting
# values for the model fit (or return NULL when `initialize = FALSE`). If a
# `seed` was passed for the sampler, it is reused for the starting values so
# that the entire fit is reproducible from a single seed.
perf_mod_inits <- function(initialize, formula, data, dots) {
  if (!rlang::is_bool(initialize)) {
    rlang::abort("`initialize` should be a single logical value.")
  }
  if (!initialize) {
    return(NULL)
  }
  if (any(names(dots) == "init")) {
    rlang::abort(
      paste0(
        "When `initialize = TRUE`, an `init` value cannot also be passed ",
        "to `rstanarm::stan_glmer()`. Use one or the other."
      )
    )
  }
  family <- dots$family
  if (!is.null(family)) {
    if (is.character(family)) {
      family <- get(family, mode = "function", envir = asNamespace("stats"))
    }
    if (is.function(family)) {
      family <- family()
    }
    is_default <-
      identical(family$family, "gaussian") &&
      identical(family$link, "identity")
    if (!is_default) {
      rlang::abort(
        paste0(
          "`initialize = TRUE` computes starting values for the default ",
          "Gaussian model with an identity link and cannot be used with ",
          "other families or links."
        )
      )
    }
  }
  seed <- if (is.null(dots$seed)) sample.int(10^5, 1) else dots$seed
  stan_glmer_inits(formula, data = data, seed = seed)
}

#' @export
print.stan_glmer_inits <- function(x, ...) {
  cat("Starting values for `rstanarm::stan_glmer()`\n")
  cat("formula:", attr(x, "formula"), "\n")
  cat("initialized:", paste(attr(x, "blocks"), collapse = ", "), "\n")
  cat("random effect terms:\n")
  for (tm in attr(x, "term_info")) {
    cat(
      "  ",
      tm$name,
      ": ",
      tm$p,
      " coefficient(s) x ",
      tm$nlev,
      " levels\n",
      sep = ""
    )
  }
  cat("jitter:", attr(x, "jitter"), "with seed", attr(x, "seed"), "\n")
  invisible(x)
}
