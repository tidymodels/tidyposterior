# bad arguments

    Code
      perf_mod(test_bt, transform = NULL)
    Condition
      Error in `check_trans()`:
      ! `transform` should have two functions: 'func' and 'inv'

---

    Code
      perf_mod(test_bt, transform = no_trans[1])
    Condition
      Error in `check_trans()`:
      ! `transform` should have two functions: 'func' and 'inv'

---

    Code
      perf_mod(test_bt, transform = list(not = 1, right = 2))
    Condition
      Error in `check_trans()`:
      ! `transform` should have two functions: 'func' and 'inv'

---

    Code
      perf_mod(test_bt, transform = list(func = 1, inc = 2))
    Condition
      Error in `check_trans()`:
      ! `transform` should have two functions: 'func' and 'inv'

---

    Code
      perf_mod(1:10)
    Condition
      Error in `error_cnd()`:
      ! Conditions must have named data fields

# multiple id columns produce a warning about the formula

    Code
      tidyposterior:::make_formula(c("id", "id2"), FALSE, NULL)
    Condition
      Warning:
      There were multiple resample ID columns in the data. It is unclear what the model formula should be for the hierarchical model. This analysis used the formula: `statistic ~ model + (1 | id2/id)` The `formula` arg can be used to change this value.
    Output
      statistic ~ model + (1 | id2/id)
      <environment: base>

# basic usage

    Code
      print(obj_1)
    Output
      Bayesian Analysis of Resampling Results
      Original data: Bootstrap sampling
      

# data frame method

    Code
      print(obj_2)
    Output
      Bayesian Analysis of Resampling Results
      

# rsample method

    Code
      print(obj_4)
    Output
      Bayesian Analysis of Resampling Results
      

# rsample method with repeated cv

    Code
      print(obj_5)
    Output
      Bayesian Analysis of Resampling Results
      Original data: 5-fold cross-validation repeated 2 times
      

# repeated v_fold method

    Code
      print(obj_6)
    Output
      Bayesian Analysis of Resampling Results
      Original data: 5-fold cross-validation repeated 2 times
      

# autoplots

    Code
      ggplot2::get_labs(p_1)
    Output
      $colour
      [1] "model"
      
      $x.sec
      NULL
      
      $x
      [1] "posterior"
      
      $y
      [1] "density"
      attr(,"fallback")
      [1] TRUE
      
      $y.sec
      NULL
      
      $fill
      [1] "fill"
      attr(,"fallback")
      [1] TRUE
      
      $weight
      [1] "weight"
      attr(,"fallback")
      [1] TRUE
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(p_2)
    Output
      $colour
      [1] "model"
      
      $x.sec
      NULL
      
      $x
      [1] "posterior"
      
      $y
      [1] "density"
      attr(,"fallback")
      [1] TRUE
      
      $y.sec
      NULL
      
      $fill
      [1] "fill"
      attr(,"fallback")
      [1] TRUE
      
      $weight
      [1] "weight"
      attr(,"fallback")
      [1] TRUE
      
      $alt
      [1] ""
      

# workflow sets

    Code
      ggplot2::get_labs(p_tidy)
    Output
      $colour
      [1] "model"
      
      $x.sec
      NULL
      
      $x
      [1] "rsq"
      
      $y
      [1] "density"
      attr(,"fallback")
      [1] TRUE
      
      $y.sec
      NULL
      
      $fill
      [1] "fill"
      attr(,"fallback")
      [1] TRUE
      
      $weight
      [1] "weight"
      attr(,"fallback")
      [1] TRUE
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(p_int)
    Output
      $colour
      [1] "workflow"
      
      $x.sec
      NULL
      
      $x
      [1] "Workflow Rank"
      
      $y
      [1] "rsq"
      
      $y.sec
      NULL
      
      $ymin
      [1] ".lower"
      
      $ymax
      [1] ".upper"
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(p_rope)
    Output
      $colour
      [1] "workflow"
      
      $x.sec
      NULL
      
      $x
      [1] "Workflow Rank"
      
      $y
      [1] "Probability of Practical Equivalence"
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

# `select_best` must be a single logical

    Code
      perf_mod(unfitted, select_best = "yes")
    Condition
      Error in `perf_mod()`:
      ! `select_best` should be a single `TRUE` or `FALSE`.

---

    Code
      perf_mod(unfitted, select_best = c(TRUE, TRUE))
    Condition
      Error in `perf_mod()`:
      ! `select_best` should be a single `TRUE` or `FALSE`.

