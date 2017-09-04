#' Scheduler class that orders compute tasks and dispatches tasks to workers via \code{make_future()}
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom rstackdeque rstack
#' @export
Scheduler=R6Class(classname = "Scheduler",
                     cloneable = FALSE,
                     portable = TRUE,
                     class = TRUE,
                     public = list(
                       initialize = function(delayed_object) {
                          private$.stack <- rstack()
                          private$.delayed_object <- delayed_object
                          self$order_task(delayed_object)
                          
                          invisible(self)
                       },
                       print = function(){
                         print(private$.delayed_object)
                       },
                       order_task = function(delayed_object){
                         
                         private$.stack <- insert_top(private$.stack, delayed_object)
                         
                         unresolved <- delayed_object$unresolved_dependencies
                         if(length(unresolved)>0){
                          # todo: order peer nodes based on importance
                          # https://github.com/dask/dask/blob/master/dask/order.py#L118
                          lapply(unresolved,self$order_task)
                         }
                       }, 
                       compute = function(){
                         
                         
                         while(!private$.delayed_object$resolved){
                           
                           current_task <- self$next_ready_task
                           if(!is.null(current_task)){
                             # print(current_task)
                             current_task$make_future()
                           }
                         }
                         
                         return(private$.delayed_object$value)
                       }),
                     active = list(
                       name = function(){
                         return(private$.name)
                       },
                       task_list = function(){
                         return(as.list(private$.stack))
                       },
                       next_ready_task = function(){
                         current_stack <- private$.stack
                         while(!empty(current_stack)){
                           current_task <- peek_top(current_stack)  
                           
                           if(current_task$ready){
                             return(current_task)
                           } else {
                             current_stack <- without_top(current_stack) 
                           }
                         }
                         
                         #made it to the end of the stack without finding anything that's ready
                         return(NULL)
                         
                         
                       }

                     ),
                     private = list(
                       .delayed_object = NULL,
                       .stack = NULL
                     )
)