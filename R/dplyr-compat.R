## adapted from
## https://github.com/hadley/dtplyr/blob/2308ff25e88bb81fe84f9051e37ddd9d572189ee/R/compat-dplyr-0.6.0.R
## and based on
## https://github.com/tidyverse/googledrive/commit/95455812d2e0d6bdf92b5f6728e3265bf65d8467#diff-ba61d4f2ccd992868e27305a9ab68a3c

## function is called in .onLoad()
register_s3_method <- function(pkg, generic, class, fun = NULL) { # nocov start
  stopifnot(is_string(pkg))
  envir <- asNamespace(pkg)

  stopifnot(is_string(generic))
  stopifnot(is_string(class))
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  stopifnot(is.function(fun))

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = envir)
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = envir)
    }
  )
} # nocov end

new_posterior <- function(x) {
  stopifnot(inherits(x, "data.frame"))
  structure(x, class = c("posterior", "tbl_df", "tbl", "data.frame"))
}


is_posterior <- function(x) {
  is_tibble(x) &&
    all(c("model", "posterior") %in% names(x))
}

#' @export
`[.posterior` <- function(x, i, j, drop = FALSE) {
  maybe_posterior(NextMethod())
}

maybe_posterior <- function(x) {
  if (is_posterior(x)) {
    new_posterior(x)
  } else {
    as_tibble(x)
  }
}

## tidyposterior does not import any generics from dplyr,
## but if dplyr is loaded and main verbs are used on a `posterior`
## object generated from `tidy.perf_mod`, we want to retain
## the `posterior`` class if it is proper to do so therefore these
## S3 methods are registered manually in .onLoad()

arrange.posterior <- function(.data, ...) {
  maybe_posterior(NextMethod())
}

filter.posterior <- function(.data, ...) {
  maybe_posterior(NextMethod())
}

mutate.posterior <- function(.data, ...) {
  maybe_posterior(NextMethod())
}

rename.posterior <- function(.data, ...) {
  maybe_posterior(NextMethod())
}

select.posterior <- function(.data, ...) {
  maybe_posterior(NextMethod())
}

slice.posterior <- function(.data, ...) {
  maybe_posterior(NextMethod())
}
