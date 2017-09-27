#' A Job encapsulates the act of evaluating a given delayed object in a particular way
#' \code{SequentialJob}s evaluate immediately, blocking until they are complete
#' \code{FutureJob}s leverage the \code{future} package to evaluate according to the specified \code{plan}
#' @docType class
#' @importFrom R6 R6Class
#' @export
Job <- R6Class(classname = "Job",
                 cloneable = FALSE,
                 portable = TRUE,
                 class = TRUE,
                 public = list(
                   initialize = function(delayed_object){
                     private$.delayed_object = delayed_object
                     delayed_object$register_job(self)
                     
                     invisible(self)
                   }),
                  
                 active = list(
                   finished = function(){
                     return(FALSE)
                   },
                   value = function(){
                     return(private$.result)
                   }
                 ),
            private = list(
              .delayed_object=NULL,
              .result = NULL
            )
            
)

#' A Job that will evaluate immediately
#' @docType class
#' @importFrom R6 R6Class
#' @export
SequentialJob <- R6Class(classname = "SequentialJob",
                  cloneable = FALSE,
                  portable = TRUE,
                  class = TRUE,
                  inherit = Job,
                  public = list(
                    initialize = function(delayed_object){
                      to_eval <- delayed_object$prepare_eval()
                      private$.result <- rlang::eval_bare(expr=to_eval$expr, env=to_eval$env)
                      super$initialize(delayed_object)
                    }),
                  
                  active = list(
                    finished = function(){
                        return(TRUE)
                    },
                    value = function(){
                      return(private$.result)
                    }
                  ),
                  private = list(
                    .worker=NULL,
                    .result_uuid = NULL
                  )
                  
)

#' A Job that leverages the \code{future} framework to evaluate asynchronously
#' @docType class
#' @importFrom R6 R6Class
#' @export
FutureJob <- R6Class(classname = "FutureJob",
                  cloneable = FALSE,
                  portable = TRUE,
                  class = TRUE,
                  inherit = Job,
                  public = list(
                    initialize = function(delayed_object){
                      to_eval <- delayed_object$prepare_eval()
                      private$.future <- future(expr=to_eval$expr, env=to_eval$env, substitute=FALSE, globals=as.list(to_eval$env), packages=NULL)
                      super$initialize(delayed_object)
                    }),
                  
                  active = list(
                    finished = function(){
                      return(resolved(private$.future))
                    },
                    value = function(){
                      if(is.null(private$.result)){
                        private$.result <- value(private$.future)
                      }
                      return(private$.result)
                    }
                    
                  ),
                  private = list(
                    .future=NULL
                  )
                  
)