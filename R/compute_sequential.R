#' @export
compute_sequential <- function(delayed_object){
  #first compute all dependencies
  delayed_components <- delayed_object$delayed_components
  lapply(delayed_components,compute_sequential)
  value <- delayed_object$resolve()
  
  return(value)
}
