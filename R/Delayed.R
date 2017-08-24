#' @docType class
#' @importFrom R6 R6Class
#' @export
Delayed=R6Class(classname = "Delayed",
                     cloneable = FALSE,
                     portable = TRUE,
                     class = TRUE,
                     public = list(
                       initialize = function(expression) {

                         private$.expression <- expression
                         private$.name <- deparse(expression)
                         private$.components <- lapply(as.list(expression)[-1],eval)
                       },
                       print = function(){
                         print(private$.name)
                         #todo: list components in a nested fashion
                       },
                       resolve = function(){
                         delayed_components  <-  self$delayed_components
                         components_resolved <- sapply(delayed_components,`[[`,"resolved")
                         
                         if(!all(components_resolved)){
                           unresolved_component_labels <- names(which(!components_resolved))
                           unresolved_components  <- delayed_components[unresolved_component_labels]
                           unresolved_names  <- sapply(unresolved_components,`[[`,"name")
                           stop("Cannot resolve object has unresolved dependencies:",unresolved_names)
                         }
                         
                         delayed_values  <- lapply(delayed_components,`[[`, "value")
                         expr_list  <- as.list(self$expression)
                         expr_list[names(delayed_values)]  <- delayed_values
                         new_expr  <- as.call(expr_list)
                         private$.value  <- eval(new_expr)
                         private$.resolved  <- T
                         
                         return(self$value)
                       },
                       make_graph=function(graph=NULL){
                         #todo: maybe store each node's subgraph on call
                         if(is.null(graph)){
                           graph <- make_empty_graph()
                         }
                         
                         #todo: this doesn't protect against identical calls with different instantiations of a variable
                         if(!(self$name%in%names(V(graph)))){
                          graph <- graph + vertex(self$name, shape="rectangle")
                         }
                         delayed_components <-  self$delayed_components
                         for(i in seq_along(delayed_components)){
                           graph <- delayed_components[[i]]$make_graph(graph)
                           graph <- graph + edge(delayed_components[[i]]$name, self$name, label=names(delayed_components)[i])
                         }
                         
                         
                         return(graph)
                         
                       },
                       compute=function(){
                         compute_sequential(self)
                       }),
                     
                     active = list(
                       name = function(){
                         return(private$.name)
                       },
                       expression = function(){
                         return(private$.expression)
                       },
                       components = function(){
                         return(private$.components)
                       },
                       delayed_components = function(){
                         delayed_components <- private$.components[sapply(private$.components,inherits, "Delayed")]
                         return(delayed_components)
                       },
                       resolved = function(){
                         return(private$.resolved)
                       },
                       value = function(){
                         return(private$.value)
                       }
                     ),
                     private = list(
                       .name = NULL,
                       .expression = NULL,
                       .components = list(),
                       .value = NULL,
                       .resolved = FALSE
                     )
)

# todo, combine delayed and delayed_fun
#' @export
delayed <- function(expr){
  call <- match.call()
  Delayed$new(call$expr)
}

#' @export
delayed_fun <- function(fun){
  fun_sym=match.call()$fun
  # fun_name=as.character(fun_sym)
  delayed_f=function(...){
    
    call=match.call(fun,sys.call())
    # replace delayed function with true function in call
    true_call=as.call(c(fun_sym,as.list(call)[-1])) 
    Delayed$new(true_call)
  }
  
}

