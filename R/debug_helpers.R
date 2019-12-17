.delayed_traceback <- NULL

delayed_traceback <- function(){
  traceback(.delayed_traceback)
}

delayed_log_traceback <- function(){
  
}