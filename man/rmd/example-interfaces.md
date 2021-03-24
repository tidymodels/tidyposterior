

#' ## Input formats
#' 
#' There are several ways to give resapling results to the `perf_mod()` function. To #' illustrate, here are some example objects using 10-fold cross-validation for a #' simple two-class problem: 
#' 
#' 
#' ```r
#'    library(tidymodels)
#'    library(tidyposterior)
#'    library(workflowsets)
#'    
#'    data(two_class_dat, package = "modeldata")
#'    
#'    set.seed(100)
#'    folds <- vfold_cv(two_class_dat)
#' ```
#' 
#' We can define two different models (for simplicity, with no tuning parameters).
#' 
#' 
#' ```r
#'    logistic_reg_glm_spec <-
#'      logistic_reg() %>%
#'      set_engine('glm')
#'    
#'    mars_earth_spec <-
#'      mars(prod_degree = 1) %>%
#'      set_engine('earth') %>%
#'      set_mode('classification')
#' ```
#' 
#' For tidymodels, the [tune::fit_resamples()] function can be used to estimate #' performance for each model/resample:
#' 
#' 
#' ```r
#'    rs_ctrl <- control_resamples(save_workflow = TRUE)
#'    
#'    logistic_reg_glm_res <- 
#'      logistic_reg_glm_spec %>% 
#'      fit_resamples(Class ~ ., resamples = folds, control = rs_ctrl)
#'    
#'    mars_earth_res <- 
#'      mars_earth_spec %>% 
#'      fit_resamples(Class ~ ., resamples = folds, control = rs_ctrl)
#' ```
#' 
#' From these, there are several ways to pass the results to `perf_mod()`.
#' 
#' ### Data Frame as Input
#' 
#' The most general approach is to have a data frame with the resampling labels (i.e., #' one or more id columns) as well as columns for each model that you would like to #' compare. 
#' 
#' For the model results above, [tune::collect_metrics()] can be used along with some #' basic data manipulation steps: 
#' 
#' 
#' ```r
#'    logistic_roc <- 
#'      collect_metrics(logistic_reg_glm_res, summarize = FALSE) %>% 
#'      dplyr::filter(.metric == "roc_auc") %>% 
#'      dplyr::select(id, logistic = .estimate)
#'    
#'    mars_roc <- 
#'      collect_metrics(mars_earth_res, summarize = FALSE) %>% 
#'      dplyr::filter(.metric == "roc_auc") %>% 
#'      dplyr::select(id, mars = .estimate)
#'    
#'    resamples_df <- full_join(logistic_roc, mars_roc, by = "id")
#'    resamples_df
#' ```
#' 
#' ```
#'    ## # A tibble: 10 x 3
#'    ##   id     logistic  mars
#'    ##   <chr>     <dbl> <dbl>
#'    ## 1 Fold01    0.908 0.875
#'    ## 2 Fold02    0.904 0.917
#'    ## 3 Fold03    0.924 0.938
#'    ## 4 Fold04    0.881 0.881
#'    ## 5 Fold05    0.863 0.864
#'    ## 6 Fold06    0.893 0.889
#'    ## # … with 4 more rows
#' ```
#' 
#' We can then give this directly to `perf_mod()`: 
#' 
#' 
#' ```r
#'    set.seed(101)
#'    roc_model_via_df <- perf_mod(resamples_df, refresh = 0)
#'    tidy(roc_model_via_df) %>% summary()
#' ```
#' 
#' ```
#'    ## # A tibble: 2 x 4
#'    ##   model     mean lower upper
#'    ##   <chr>    <dbl> <dbl> <dbl>
#'    ## 1 logistic 0.892 0.879 0.906
#'    ## 2 mars     0.888 0.875 0.902
#' ```
#' 
#' ### rsample Object as Input
#' 
#' Alternatively, the result columns can be merged back into the original `rsample` #' object. The up-side to using this method is that `perf_mod()` will know exactly #' which model formula to use for the Bayesian model:
#' 
#' 
#' ```r
#'    resamples_rset <- 
#'      full_join(folds, logistic_roc, by = "id") %>% 
#'      full_join(mars_roc, by = "id")
#'    
#'    set.seed(101)
#'    roc_model_via_rset <- perf_mod(resamples_rset, refresh = 0)
#'    tidy(roc_model_via_rset) %>% summary()
#' ```
#' 
#' ```
#'    ## # A tibble: 2 x 4
#'    ##   model     mean lower upper
#'    ##   <chr>    <dbl> <dbl> <dbl>
#'    ## 1 logistic 0.892 0.879 0.906
#'    ## 2 mars     0.888 0.875 0.902
#' ```
#' 
#' ### Workflow Set Object as Input
#' 
#' Finally, for tidymodels, a workflow set object can be used. This is a collection of #' models/preprocessing combinations in one object. We can emulate a workflow set using #' the existing example results then pass that to `perf_mod()`: 
#' 
#' 
#' ```r
#'    example_wset <-
#'      as_workflow_set(logistic = logistic_reg_glm_res, mars = mars_earth_res)
#'    
#'    set.seed(101)
#'    roc_model_via_wflowset <- perf_mod(example_wset, refresh = 0)
#'    tidy(roc_model_via_rset) %>% summary()
#' ```
#' 
#' ```
#'    ## # A tibble: 2 x 4
#'    ##   model     mean lower upper
#'    ##   <chr>    <dbl> <dbl> <dbl>
#'    ## 1 logistic 0.892 0.879 0.906
#'    ## 2 mars     0.888 0.875 0.902
#' ```
#' 
#' ### caret resamples object
#' 
#' The `caret` package can also be used. An equivalent set of models are created:
#' 
#' 
#' 
#' ```r
#'    library(caret)
#'    
#'    set.seed(102)
#'    logistic_caret <- train(Class ~ ., data = two_class_dat, method = "glm", 
#'                            trControl = trainControl(method = "cv"))
#'    
#'    set.seed(102)
#'    mars_caret <- train(Class ~ ., data = two_class_dat, method = "gcvEarth",
#'                        tuneGrid = data.frame(degree = 1),
#'                        trControl = trainControl(method = "cv"))
#' ```
#' 
#' Note that these two models use the same resamples as one another due to setting the #' seed prior to calling `train()`. However, these are different from the tidymodels #' results used above (so the final results will be different).
#' 
#' `caret` has a `resamples()` function that can collect and collate the resamples. #' This can also be given to `perf_mod()`: 
#' 
#' 
#' ```r
#'    caret_resamples <- resamples(list(logistic = logistic_caret, mars = mars_caret))
#'    
#'    set.seed(101)
#'    roc_model_via_caret <- perf_mod(caret_resamples, refresh = 0)
#'    tidy(roc_model_via_caret) %>% summary()
#' ```
#' 
#' ```
#'    ## # A tibble: 2 x 4
#'    ##   model     mean lower upper
#'    ##   <chr>    <dbl> <dbl> <dbl>
#'    ## 1 logistic 0.821 0.801 0.842
#'    ## 2 mars     0.822 0.802 0.842
#' ```
#' 
