# ------------------------------------------------------------------------------
# posterior_diff

posterior_diff_reconstruct <- function(x, to) {
  if (posterior_diff_reconstructable(x, to)) {
    df_reconstruct(x, to)
  } else {
    tib_upcast(x)
  }
}

posterior_diff_reconstructable <- function(x, to) {
  x_names <- names(x)
  to_names <- names(to)
  req_names <- c("difference", "model_1", "model_2", "contrast")
  if (!all(req_names %in% x_names)) {
    return (FALSE)
  } else {
    if (!is.numeric(x[["difference"]])) {
      return(FALSE)
    }
    if (!is.character(x[["model_1"]]) & !is.factor(x[["model_1"]])) {
      return(FALSE)
    }
    if (!is.character(x[["model_2"]]) & !is.factor(x[["model_2"]])) {
      return(FALSE)
    }
    if (!is.character(x[["contrast"]]) & !is.factor(x[["contrast"]])) {
      return(FALSE)
    }
  }
  TRUE
}

# ------------------------------------------------------------------------------

#' @export
`[.posterior_diff` <- function(x, i, j, ...) {
  out <- NextMethod()
  posterior_diff_reconstruct(out, x)
}

#' @export
`names<-.posterior_diff` <- function(x, value) {
  out <- NextMethod()
  posterior_diff_reconstruct(out, x)
}

# ------------------------------------------------------------------------------
# posterior_diff vctrs functions

#' Extra methods for the `posterior_diff` class to work with dplyr verbs
#'
#' Objects with class `posterior_diff` are defined to be tibbles with required
#' columns `difference` (numeric) and character columns `model_1`, `model_2`,
#' and `contrast`. If operations on these objects break those rules, they are
#' down-cast to basic tibbles.
#' @export
#' @rdname vctrs_methods_posterior_diff
#' @keywords internal
vec_restore.posterior_diff <- function(x, to, ...) {
  posterior_diff_reconstruct(x, to)
}


#' @export
#' @rdname vctrs_methods_posterior_diff
#' @keywords internal
vec_proxy.posterior_diff <- function(x, ...) {
  vctrs::new_data_frame(x)
}


#' @export
#' @rdname vctrs_methods_posterior_diff
#' @keywords internal
vec_ptype2.posterior_diff.posterior_diff <- function(x, y, ..., x_arg = "", y_arg = "") {
  tibble::tibble(
    difference = numeric(0),
    model_1 = character(0),
    model_2 = character(0),
    contrast = character(0)
  )
}
#' @export
#' @rdname vctrs_methods_posterior_diff
#' @keywords internal
vec_ptype2.posterior_diff.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}
#' @export
#' @rdname vctrs_methods_posterior_diff
#' @keywords internal
vec_ptype2.tbl_df.posterior_diff <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}
#' @export
#' @rdname vctrs_methods_posterior_diff
#' @keywords internal
vec_ptype2.posterior_diff.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}
#' @export
#' @rdname vctrs_methods_posterior_diff
#' @keywords internal
vec_ptype2.data.frame.posterior_diff <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

#' @export
#' @rdname vctrs_methods_posterior_diff
#' @keywords internal
vec_cast.posterior_diff.posterior_diff <- function(x, to, ..., x_arg = "", to_arg = "") {
  x
}
#' @export
#' @rdname vctrs_methods_posterior_diff
#' @keywords internal
vec_cast.posterior_diff.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_posterior_diff(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @rdname vctrs_methods_posterior_diff
#' @keywords internal
vec_cast.tbl_df.posterior_diff <- function(x, to, ..., x_arg = "", to_arg = "") {
  vctrs::tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @rdname vctrs_methods_posterior_diff
#' @keywords internal
vec_cast.posterior_diff.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_posterior_diff(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @rdname vctrs_methods_posterior_diff
#' @keywords internal
vec_cast.data.frame.posterior_diff <- function(x, to, ..., x_arg = "", to_arg = "") {
  vctrs::df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}

stop_incompatible_cast_posterior_diff <- function(x, to, ..., x_arg, to_arg) {
  details <- "Can't cast to a <posterior_diff> because columns names and types are likely incompatible."
  vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg, details = details)
}
