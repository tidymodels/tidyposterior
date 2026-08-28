# `initialize_keys()` needs a perf_mod object

    Code
      initialize_keys(mtcars)
    Condition
      Error in `initialize_keys()`:
      ! `x` should be an object produced by `perf_mod()`, not <data.frame>.

---

    Code
      initialize_keys("nope")
    Condition
      Error in `initialize_keys()`:
      ! `x` should be an object produced by `perf_mod()`, not <character>.

# bad arguments

    Code
      compare_to_leader(mtcars)
    Condition
      Error in `compare_to_leader()`:
      ! `x` should be an object produced by `perf_mod()`, not <data.frame>.

---

    Code
      compare_to_leader(fit_bt)
    Condition
      Error in `check_direction()`:
      ! Can't tell whether the metric should be maximized or minimized.
      * `x` records this only when `perf_mod()` is given a `tune_results` object or a workflow set, since those carry the metric that was used.

---

    Code
      compare_to_leader(bad_dir)
    Condition
      Error in `check_direction()`:
      ! The metric direction recorded in `x` should be either 'maximize' or 'minimize', not 'up'.

---

    Code
      compare_to_leader(fit_max, prob = 2)
    Condition
      Error in `check_prob()`:
      ! `prob` should be a single number greater than 0 and less than 1.

---

    Code
      compare_to_leader(fit_max, prob = c(0.5, 0.9))
    Condition
      Error in `check_prob()`:
      ! `prob` should be a single number greater than 0 and less than 1.

---

    Code
      compare_to_leader(fit_max, size = -1)
    Condition
      Error in `check_size()`:
      ! `size` should be a single positive number or `NULL`.

---

    Code
      compare_to_leader(fit_max, leader = "nope")
    Condition
      Error in `check_leader()`:
      ! `leader` ('nope') is not one of the models in `x`.
      * Possible values are: 'three', 'two', 'one'.

---

    Code
      compare_to_leader(fit_max, leader = c("one", "two"))
    Condition
      Error in `check_leader()`:
      ! `leader` should be a single character string or `NULL`.

---

    Code
      compare_to_leader(fit_max, key = "nope")
    Condition
      Error in `check_key()`:
      ! `key` should be a data frame or `NULL`.

---

    Code
      compare_to_leader(fit_max, key = mtcars)
    Condition
      Error in `check_key()`:
      ! `key` should have a column of model names called 'model' or 'wflow_id'.
      * `initialize_keys()` makes a template with the right columns.

---

    Code
      compare_to_leader(fit_max, key = tibble::tibble(model = "one"))
    Condition
      Error in `check_key()`:
      ! `key` should have a column of labels called 'label'.
      * `initialize_keys()` makes a template with the right columns.

---

    Code
      compare_to_leader(fit_max, key = tibble::tibble(model = "one", label = "first"))
    Condition
      Error in `check_key()`:
      ! Every model in `x` should have a row in `key`.
      * Missing: 'two', 'three'.
      * `initialize_keys()` makes a template with a row for every model.

# at least two models are required

    Code
      compare_to_leader(one_model)
    Condition
      Error in `compare_to_leader()`:
      ! `x` should contain at least two models to compare to the leader.

# printing

    Code
      print(fake)
    Output
      # Comparisons to the leader (a)
      # A tibble: 2 x 14
        model label  rank leader median lower upper mean_diff lower_diff upper_diff
        <fct> <fct> <int> <lgl>   <dbl> <dbl> <dbl>     <dbl>      <dbl>      <dbl>
      1 a     a         1 TRUE        1   0.5   1.5         0        0          0  
      2 b     b         2 FALSE       2   1.5   2.5         1        0.5        1.5
      # i 4 more variables: pr_worse <dbl>, size <dbl>, pract_equiv <dbl>,
      #   pract_worse <dbl>

# `metric_label` must be a single string or expression

    Code
      autoplot(res_max, metric_label = 1)
    Condition
      Error in `check_metric_label()`:
      ! `metric_label` should be a single character string, an expression, or `NULL`.

---

    Code
      autoplot(res_max, metric_label = c("a", "b"))
    Condition
      Error in `check_metric_label()`:
      ! `metric_label` should be a single character string, an expression, or `NULL`.

---

    Code
      autoplot(res_max, metric_label = NA_character_)
    Condition
      Error in `check_metric_label()`:
      ! `metric_label` should be a single character string, an expression, or `NULL`.

# `zero_bar` must be a single number in [0, 1]

    Code
      autoplot(res_max, zero_bar = -1)
    Condition
      Error in `autoplot()`:
      ! `zero_bar` should be a single number between 0 and 1.

---

    Code
      autoplot(res_max, zero_bar = 2)
    Condition
      Error in `autoplot()`:
      ! `zero_bar` should be a single number between 0 and 1.

---

    Code
      autoplot(res_max, zero_bar = c(0.1, 0.2))
    Condition
      Error in `autoplot()`:
      ! `zero_bar` should be a single number between 0 and 1.

# autoplot needs a `compare_to_leader()` result

    Code
      autoplot(bad)
    Condition
      Error in `autoplot()`:
      ! `object` should be a data frame produced by `compare_to_leader()`.

