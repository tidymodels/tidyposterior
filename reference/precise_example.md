# Example Data Sets

Example Data Sets

## Value

Tibbles with the additional class `rset`

## Details

Several data sets are contained in the package as examples. Each
*simulates* an `rset` object but the `splits` columns are not included
to save space.

- `precise_example` contains the results of the classification analysis
  of a real data set using 10-fold CV. The holdout data sets contained
  thousands of examples and have precise performance estimates. Three
  models were fit to the original data and several performance metrics
  are included.

- `noisy_example` was also generated from a regression data simulation.
  The original data set was small (50 samples) and 10-repeated of
  10-fold CV were used with four models. There is an excessive of
  variability in the results (probably more than the
  resample-to-resample variability). The RMSE distributions show fairly
  right-skewed distributions.

- `concrete_example` contains the results of the regression case study
  from the book *Applied Predictive Modeling*. The original data set
  contained 745 samples in the training set. 10-repeats of 10-fold CV
  was also used and 13 models were fit to the data.

- `ts_example` is from a data set where rolling-origin forecast
  resampling was used. Each assessment set is the summary of 14
  observations (i.e. 2 weeks). The analysis set consisted of a base of
  about 5,500 samples plus the previous assessment sets. Four regression
  models were applied to these data.

- `ex_object` objects were generated from the `two_class_dat` data in
  the `modeldata` package. Basic 10-fold cross validation was used to
  evaluate the models. The `posterior_samples` object is samples of the
  posterior distribution of the model ROC values while
  `contrast_samples` are posterior probabilities form the differences in
  ROC values.

## Examples

``` r
data(precise_example)
precise_example
#> #  10-fold cross-validation using stratification 
#> # A tibble: 10 × 29
#>    splits id     glm_Accuracy glm_Kappa glm_ROC glm_Sens glm_Spec
#>    <lgl>  <chr>         <dbl>     <dbl>   <dbl>    <dbl>    <dbl>
#>  1 NA     Fold01        0.722     0.328   0.798    0.729    0.720
#>  2 NA     Fold02        0.696     0.290   0.778    0.720    0.691
#>  3 NA     Fold03        0.701     0.297   0.790    0.723    0.696
#>  4 NA     Fold04        0.704     0.316   0.795    0.763    0.691
#>  5 NA     Fold05        0.721     0.324   0.797    0.722    0.721
#>  6 NA     Fold06        0.711     0.303   0.780    0.706    0.712
#>  7 NA     Fold07        0.702     0.305   0.790    0.739    0.694
#>  8 NA     Fold08        0.718     0.321   0.784    0.729    0.715
#>  9 NA     Fold09        0.720     0.328   0.795    0.739    0.715
#> 10 NA     Fold10        0.719     0.324   0.796    0.728    0.717
#> # ℹ 22 more variables: glm_PRAUC <dbl>, glm_Precision <dbl>,
#> #   glm_Recall <dbl>, glm_F <dbl>, knn_Accuracy <dbl>,
#> #   knn_Kappa <dbl>, knn_ROC <dbl>, knn_Sens <dbl>, knn_Spec <dbl>,
#> #   knn_PRAUC <dbl>, knn_Precision <dbl>, knn_Recall <dbl>,
#> #   knn_F <dbl>, nnet_Accuracy <dbl>, nnet_Kappa <dbl>,
#> #   nnet_ROC <dbl>, nnet_Sens <dbl>, nnet_Spec <dbl>,
#> #   nnet_PRAUC <dbl>, nnet_Precision <dbl>, nnet_Recall <dbl>, …
```
