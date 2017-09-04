# state diagram
# (waiting->ready->running->resolved seems to be enough for our purposes)

#' @docType class
#' @importFrom R6 R6Class
#' @importFrom future resolved
#' @importFrom data.table address
#' @export
Delayed=R6Class(classname = "Delayed",
                     cloneable = FALSE,
                     portable = TRUE,
                     class = TRUE,
                     public = list(
                       initialize = function(qexpr) {
                         private$.qexpr <- qexpr
                         private$.name <- paste(deparse(UQE(qexpr)), collapse="")
                         private$.dependencies <- lapply(lang_tail(UQ(qexpr)), eval_tidy, env=f_env(qexpr))
                       },
                       print = function(){
                         print(sprintf("delayed(%s)", private$.name))
                         #todo: list dependencies in a nested fashion
                       },
                       make_future = function(){
                         if(!self$ready){
                           stop("Task has unresolved dependencies: ",names(self$unresolved_dependencies))
                         }
                         
                         undelay <- function(x){
                           if(inherits(x,"Delayed")){
                             return(x$value)
                           } else {
                             return(x)
                           }
                         }
                         
                         args  <- lapply(self$dependencies, undelay)
                         expr=UQE(self$expression)
                         
                         #this seems really dangerous
                         mut_node_cdr(expr,as.pairlist(args))

                         env <- f_env(self$expression)
                         private$.future <- future(expr, env ,substitute = FALSE, globals = TRUE, packages = NULL)

                         private$.state <- "running" 
                         return(private$.future)
                       },
                       make_graph=function(graph=NULL){
                         #todo: maybe store each node's subgraph on call
                         
                         if(is.null(graph)){
                           graph <- make_empty_graph()
                         }
                         
                         #todo: this doesn't protect against identical calls with different instantiations of a variable
                         my_address <- data.table::address(self)
                         arg_text <- as.character(lang_tail(UQ(self$expression)))
                         node_name <- as.character(lang_head(UQ(self$expression)))
                         if(!(my_address%in%names(V(graph)))){
                          graph <- graph + vertex(my_address, shape="rectangle", label=node_name)
                         }
                         delayed_dependencies <-  self$delayed_dependencies
                         for(i in seq_along(delayed_dependencies)){
                           graph <- delayed_dependencies[[i]]$make_graph(graph)
                           arg_name <- ""#arg_text[i]
                           if(is.null(arg_name)){
                             arg_name <- ""
                           }
                           graph <- graph + edge(data.table::address(delayed_dependencies[[i]]), my_address, label=arg_name)
                         }
                         
                         
                         return(graph)
                         
                       },
                       
                       compute=function(){
                         scheduler <- Scheduler$new(self)
                         value <- scheduler$compute()
                         return(value)
                       }),
                     
                     active = list(
                       name = function(){
                         return(private$.name)
                       },
                       expression = function(){
                         return(private$.qexpr)
                       },
                       dependencies = function(){
                         return(private$.dependencies)
                       },
                       delayed_dependencies = function(){
                         delayed_dependencies <- private$.dependencies[sapply(private$.dependencies,inherits, "Delayed")]
                         return(delayed_dependencies)
                       },
                       unresolved_dependencies = function(){
                         delayed_dependencies  <-  self$delayed_dependencies
                         dependencies_resolved <- sapply(delayed_dependencies,`[[`,"resolved")
                         unresolved_dependencies  <- delayed_dependencies[which(!dependencies_resolved)]
                         return(unresolved_dependencies)
                       },
                       ready = function(){
                         if(self$state=="ready"){
                           return(TRUE)
                         } else if((self$state=="waiting") && (length(self$unresolved_dependencies)==0)){
                           private$.state="ready"
                           return(TRUE)
                         } else {
                           return(FALSE)
                         }
                       },
                       resolved = function(){
                         if(self$state=="resolved"){
                           return(TRUE)
                         } else if((self$state=="running") && (future::resolved(private$.future))){
                           private$.state="resolved"
                           return(TRUE)
                         } else {
                           return(FALSE)
                         }
                       },
                       value = function(){
                         if(is.null(private$.future)){
                           return(NULL)
                         }
                         return(future::value(private$.future))
                       },
                       state = function(){
                         return(private$.state)
                       }
                     ),
                     private = list(
                       .name = NULL,
                       .qexpr = NULL,
                       .dependencies = list(),
                       .dependants = list(),
                       .future = NULL,
                       .state = "waiting"
                     )
)

# todo, combine delayed and delayed_fun
#' @export
delayed <- function(expr){
  qexpr <- enquo(expr)
  Delayed$new(qexpr)
}

#' @export
delayed_fun <- function(fun){
  fun_name <- as.character(match.call()[[2]])
  delayed_f=function(...){
      call <- match.call(fun,sys.call())
      call[[1]] <- as.name(fun_name)
      
      #make sure we have the right function in the env
      env <- new.env(parent=caller_env())
      assign(fun_name,fun, envir=env)
      pq <- as_quosure(call, env)
      Delayed$new(pq)
  }
  
}

