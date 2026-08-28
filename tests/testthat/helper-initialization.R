# Simulated resampling-style data sets and the model fitting wrapper used by
# test_initialize.R. The seeds make every call return identical data.

initialize_cv_data <- function() {
  set.seed(472)
  dat <- expand.grid(
    model = c("mod_a", "mod_b", "mod_c"),
    id = paste0("Fold", formatC(1:10, width = 2, flag = "0")),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  dat$statistic <-
    0.8 + rep(rnorm(10, 0, 0.05), each = 3) + rnorm(nrow(dat), 0, 0.02)
  dat
}

initialize_rcv_data <- function() {
  set.seed(473)
  dat <- expand.grid(
    model = c("mod_a", "mod_b", "mod_c"),
    id = paste0("Fold", 1:5),
    id2 = paste0("Rep", 1:2),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  dat$statistic <-
    0.8 + rep(rnorm(10, 0, 0.03), each = 3) + rnorm(nrow(dat), 0, 0.02)
  dat
}

initialize_p4_data <- function() {
  set.seed(474)
  dat <- expand.grid(
    model = c("mod_a", "mod_b", "mod_c", "mod_d"),
    id = paste0("Fold", formatC(1:10, width = 2, flag = "0")),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  dat$statistic <-
    10 + rep(rnorm(10, 0, 1.5), each = 4) + rnorm(nrow(dat), 0, 0.7)
  dat
}

initialize_cv_wide <- function() {
  tidyr::pivot_wider(
    initialize_cv_data(),
    names_from = model,
    values_from = statistic
  )
}

initialize_test_fit <- function(formula, data, inits) {
  suppressWarnings(
    rstanarm::stan_glmer(
      formula,
      data = data,
      init = inits,
      chains = 2,
      iter = 200,
      seed = 32,
      refresh = 0
    )
  )
}
