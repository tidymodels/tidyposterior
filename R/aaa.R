.onLoad <- function(libname, pkgname) {

  if (requireNamespace("dplyr", quietly = TRUE)) {
    register_s3_method("dplyr", "arrange", "posterior")
    register_s3_method("dplyr", "filter", "posterior")
    register_s3_method("dplyr", "mutate", "posterior")
    register_s3_method("dplyr", "rename", "posterior")
    register_s3_method("dplyr", "select", "posterior")
    register_s3_method("dplyr", "slice", "posterior")
  }
  
  invisible()
  
}