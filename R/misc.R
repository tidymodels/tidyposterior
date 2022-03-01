check_trans <- function(x) {
  trans_msg <- "`transform` should have two functions: 'func' and 'inv'"
  if (length(x) != 2) {
    stop(trans_msg, call. = FALSE)
  } else {
    if (!all(sort(names(x)) == c("func", "inv"))) {
      stop(trans_msg, call. = FALSE)
    }
    if (!all(is.function(transform))) {
      stop(trans_msg, call. = FALSE)
    }
  }
  invisible(x)
}


is_repeated_cv <- function(x) {
  all(grepl("^Fold", x$values$Resample) & grepl("\\.Rep", x$values$Resample))
}


get_id_vals <- function(x) {
  id_vars <- grep("(^id$)|(^id[1-9]$)", names(x), value = TRUE)
  map(x[, id_vars, drop = FALSE], function(x) unique(as.character(x)))
}
