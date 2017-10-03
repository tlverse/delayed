#' @title Find error in delayed chain
#' @description Searches through a network of delayed objects for the first object with state "error"
#' @param delayed_object the object in which an error occured
#' @export 
find_delayed_error <- function(delayed_object){
  if(delayed_object$state=='error'){
    return(delayed_object)
  } else{
    errors <- sapply(delayed_object$delayed_dependencies,find_delayed_error)
    
    errors <- unlist(errors)
    if(length(errors)>0){
      return(errors[[1]])
    } else {
      return(NULL)
    }
  }
}

#' @rdname bundle_delayed
#'
#' @param ... Ignore (this is a convenience function)
#
bundle_args <- function(...) {
  return(list(...))
}

################################################################################

