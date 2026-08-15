# starting value inputs are checked

    Code
      stan_glmer_inits(~ model + (1 | id), data = cv_data)
    Condition
      Error in `check_inits_inputs()`:
      ! `formula` should be a two-sided formula.

---

    Code
      stan_glmer_inits(statistic ~ model, data = cv_data)
    Condition
      Error in `glmer_init_layout()`:
      ! `formula` should contain at least one random effect term (e.g. `(model + 0 | id)`) since the starting values are for models fit with `rstanarm::stan_glmer()`.

---

    Code
      stan_glmer_inits(statistic ~ model + 0 + (1 | id), data = cv_data)
    Condition
      Error in `glmer_init_layout()`:
      ! `formula` should include a fixed-effect intercept; models without one use a different parameterization that is not supported.

---

    Code
      stan_glmer_inits(statistic ~ model + (1 | id), data = as.matrix(cv_data))
    Condition
      Error in `check_inits_inputs()`:
      ! `data` should be a data frame.

---

    Code
      stan_glmer_inits(statistic ~ potato + (1 | id), data = cv_data)
    Condition
      Error in `check_inits_inputs()`:
      ! Some variables in `formula` are not in `data`: 'potato'.

---

    Code
      stan_glmer_inits(statistic ~ model + (1 | id), data = missing_y)
    Condition
      Error in `check_inits_inputs()`:
      ! There are missing values in column(s) 'statistic'. Starting values require complete data; otherwise the random effect values may not align with the levels used in the model fit.

---

    Code
      stan_glmer_inits(statistic ~ model + (1 | id), data = missing_id)
    Condition
      Error in `check_inits_inputs()`:
      ! There are missing values in column(s) 'id'. Starting values require complete data; otherwise the random effect values may not align with the levels used in the model fit.

---

    Code
      stan_glmer_inits(statistic ~ model + (1 | id), data = constant_y)
    Condition
      Error in `glmer_init_layout()`:
      ! The outcome has zero variance so no starting values can be computed.

---

    Code
      stan_glmer_inits(statistic ~ model + (1 | id), data = cv_data, jitter = -1)
    Condition
      Error in `check_inits_inputs()`:
      ! `jitter` should be a single non-negative number.

---

    Code
      stan_glmer_inits(statistic ~ model + (1 | id), data = cv_data, seed = 1:2)
    Condition
      Error in `check_inits_inputs()`:
      ! `seed` should be a single integer.

---

    Code
      stan_glmer_inits(statistic ~ model + (1 | id), data = cv_data, blocks = "potato")
    Condition
      Error in `stan_glmer_inits()`:
      ! `blocks` must be one of "fixed", "aux", or "ranef", not "potato".

# starting value prints

    Code
      print(inits)
    Output
      Starting values for `rstanarm::stan_glmer()`
      formula: statistic ~ model + (model + 0 | id) 
      initialized: fixed, aux, ranef 
      random effect terms:
        id: 3 coefficient(s) x 10 levels
      jitter: 0.1 with seed 1 

# perf_mod initialization arguments are checked

    Code
      perf_mod(cv_wide, initialize = "yes")
    Condition
      Error in `perf_mod_inits()`:
      ! `initialize` should be a single logical value.

---

    Code
      perf_mod(cv_wide, initialize = TRUE, init = "random")
    Condition
      Error in `perf_mod_inits()`:
      ! When `initialize = TRUE`, an `init` value cannot also be passed to `rstanarm::stan_glmer()`. Use one or the other.

---

    Code
      perf_mod(cv_wide, initialize = TRUE, family = poisson)
    Condition
      Error in `perf_mod_inits()`:
      ! `initialize = TRUE` computes starting values for the default Gaussian model with an identity link and cannot be used with other families or links.

---

    Code
      perf_mod(cv_wide, initialize = TRUE, family = "poisson")
    Condition
      Error in `perf_mod_inits()`:
      ! `initialize = TRUE` computes starting values for the default Gaussian model with an identity link and cannot be used with other families or links.

