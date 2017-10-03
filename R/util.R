find_delayed_error <- function(delayed_object){
  if(delayed_object$state=='error'){
    return(delayed_object)
  } else{
    errors <- sapply(delayed_object$delayed_dependencies,find_first_delayed_error)
    
    errors <- unlist(errors)
    if(length(errors)>0){
      return(errors[[1]])
    } else {
      return(NULL)
    }
  }
}