# bad args

    Code
      contrast_models(fit_bt, "one", c("two", "three"))
    Condition
      Error in `contrast_models()`:
      ! `list_1` and `list_2` should be the same length.

# autoplot for contrasts

    Code
      ggplot2::get_labs(p_1)
    Output
      $x.sec
      NULL
      
      $x
      [1] "difference"
      
      $y
      [1] "Posterior Probability"
      
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
      

