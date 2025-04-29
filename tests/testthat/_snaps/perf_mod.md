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
      

