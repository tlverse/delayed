#' @title Find error in delayed chain
#'
#' @description Searches through a network of delayed objects for the first
#'  object with state "error"
#'
#' @param delayed_object the object in which an error occured
#'
#' @examples
#' delayed_error <- delayed_fun(stop)
#' error_message <- "this is an error"
#' broken_delayed <- delayed_error(error_message)
#' broken_delayed$expect_error <- TRUE
#' result <- broken_delayed$compute()
#' @export
find_delayed_error <- function(delayed_object) {
  if (delayed_object$state == "error") {
    return(delayed_object)
  } else {
    errors <- sapply(delayed_object$delayed_dependencies, find_delayed_error)

    errors <- unlist(errors)
    if (length(errors) > 0) {
      return(errors[[1]])
    } else {
      return(NULL)
    }
  }
}

#' @rdname bundle_delayed
#'
#' @param ... Ignore (this is a convenience function)
#'
#' @keywords internal
bundle_args <- function(...) {
  return(list(...))
}

get_all_runtimes <- function(delayed_obj) {
  dd_runtimes <- lapply(delayed_obj$delayed_dependencies, get_all_runtimes)
  this_runtime <- list(uuid = delayed_obj$uuid, runtime = delayed_obj$runtime_self)

  result <- c(unlist(dd_runtimes, recursive = FALSE), list(this_runtime))
  return(result)
}
