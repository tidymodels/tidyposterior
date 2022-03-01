dplyr_pre_1.0.0 <- function() {
  utils::packageVersion("dplyr") <= "0.8.5"
}

# ------------------------------------------------------------------------------
# posterior

posterior_reconstruct <- function(x, to) {
  if (posterior_reconstructable(x, to)) {
    df_reconstruct(x, to)
  } else {
    tib_upcast(x)
  }
}

posterior_reconstructable <- function(x, to) {
  x_names <- names(x)
  to_names <- names(to)
  req_names <- c("model", "posterior")
  if (!all(req_names %in% x_names)) {
    return(FALSE)
  } else {
    if (!is.numeric(x[["posterior"]])) {
      return(FALSE)
    }
    if (!is.character(x[["model"]]) & !is.factor(x[["model"]])) {
      return(FALSE)
    }
  }
  TRUE
}

# ------------------------------------------------------------------------------

#' @export
`[.posterior` <- function(x, i, j, ...) {
  out <- NextMethod()
  posterior_reconstruct(out, x)
}

#' @export
`names<-.posterior` <- function(x, value) {
  out <- NextMethod()
  posterior_reconstruct(out, x)
}

# ------------------------------------------------------------------------------

# Maybe this should live in vctrs?
# Fallback to a tibble from the current data frame subclass.
# Removes subclass specific attributes and additional ones added by the user.
tib_upcast <- function(x) {
  size <- df_size(x)

  # Strip all attributes except names to construct
  # a bare list to build the tibble back up from.
  attributes(x) <- list(names = names(x))

  tibble::new_tibble(x, nrow = size)
}

df_size <- function(x) {
  if (!is.list(x)) {
    rlang::abort("Cannot get the df size of a non-list.")
  }

  if (length(x) == 0L) {
    return(0L)
  }

  col <- x[[1L]]

  vctrs::vec_size(col)
}

# ------------------------------------------------------------------------------

# Maybe this should live in vctrs?
df_reconstruct <- function(x, to) {
  attrs <- attributes(to)
  attrs$names <- names(x)
  attrs$row.names <- .row_names_info(x, type = 0L)
  attributes(x) <- attrs
  x
}

# ------------------------------------------------------------------------------
# posterior vctrs functions

#' Extra methods for the posterior class to work with dplyr verbs
#'
#' Objects with class `posterior` are defined to be tibbles with required
#' columns `model` (character) and `posterior` (numeric). If operations on these
#' objects break those rules, they are down-cast to basic tibbles.
#' @export
#' @rdname vctrs_methods_posterior
#' @keywords internal
vec_restore.posterior <- function(x, to, ...) {
  posterior_reconstruct(x, to)
}


#' @export
#' @rdname vctrs_methods_posterior
#' @keywords internal
vec_proxy.posterior <- function(x, ...) {
  vctrs::new_data_frame(x)
}


#' @export
#' @rdname vctrs_methods_posterior
#' @keywords internal
vec_ptype2.posterior.posterior <- function(x, y, ..., x_arg = "", y_arg = "") {
  tibble::tibble(model = character(0), posterior = numeric(0))
}
#' @export
#' @rdname vctrs_methods_posterior
#' @keywords internal
vec_ptype2.posterior.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}
#' @export
#' @rdname vctrs_methods_posterior
#' @keywords internal
vec_ptype2.tbl_df.posterior <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}
#' @export
#' @rdname vctrs_methods_posterior
#' @keywords internal
vec_ptype2.posterior.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}
#' @export
#' @rdname vctrs_methods_posterior
#' @keywords internal
vec_ptype2.data.frame.posterior <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

#' @export
#' @rdname vctrs_methods_posterior
#' @keywords internal
vec_cast.posterior.posterior <- function(x, to, ..., x_arg = "", to_arg = "") {
  x
}
#' @export
#' @rdname vctrs_methods_posterior
#' @keywords internal
vec_cast.posterior.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_posterior(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @rdname vctrs_methods_posterior
#' @keywords internal
vec_cast.tbl_df.posterior <- function(x, to, ..., x_arg = "", to_arg = "") {
  vctrs::tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @rdname vctrs_methods_posterior
#' @keywords internal
vec_cast.posterior.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_posterior(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @rdname vctrs_methods_posterior
#' @keywords internal
vec_cast.data.frame.posterior <- function(x, to, ..., x_arg = "", to_arg = "") {
  vctrs::df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}

stop_incompatible_cast_posterior <- function(x, to, ..., x_arg, to_arg) {
  details <- "Can't cast to a <posterior> because columns names and types are likely incompatible."
  vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg, details = details)
}
