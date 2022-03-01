# nocov
.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("dplyr::dplyr_reconstruct", "posterior", method = posterior_reconstruct)
  vctrs::s3_register("dplyr::dplyr_reconstruct", "posterior_diff", method = posterior_diff_reconstruct)

  vctrs::s3_register("ggplot2::autoplot", "perf_mod")
  vctrs::s3_register("ggplot2::autoplot", "perf_mod_workflow_set")
  vctrs::s3_register("ggplot2::autoplot", "posterior")
  vctrs::s3_register("ggplot2::autoplot", "posterior_diff")
}

# nocov end
