#' Helper Function to Evaluate Delayed
#' @param to_eval a list as generated from Delayed$prepare_eval()
#' @param timeout a timeout indicating when to terminate the job
#' @export
#' @importFrom R.utils withTimeout TimeoutException
#' @importFrom R.oo throw
eval_delayed <- function(to_eval, timeout = Inf) {
  if (timeout < 0) {
    R.oo::throw(R.utils::TimeoutException("time exhausted in other steps"))
  }

  if (is.finite(timeout)) {
    setTimeLimit(timeout, timeout, transient = TRUE)
  }
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })

  result <- tryCatchLog::tryCatchLog({
    result <- rlang::eval_bare(
      expr = to_eval$expr,
      env = to_eval$env
    )
  })
  return(result)
}

#' Evaluation of Delayed Objects
#'
#' @description A \code{Job} encapsulates the act of evaluating a given
#' \code{delayed} object in a particular way. \code{SequentialJob}s evaluate
#' immediately, blocking the current process until they are complete.
#' \code{FutureJob}s leverages \pkg{future} to evaluate according to the
#' specified \code{\link[future]{plan}}.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @keywords internal
Job <- R6Class(
  classname = "Job",
  cloneable = FALSE,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(delayed_object) {
      private$.delayed_object <- delayed_object
      delayed_object$register_job(self)
      invisible(self)
    }
  ),

  active = list(
    finished = function() {
      return(FALSE)
    },

    value = function() {
      return(private$.result)
    },

    runtime = function() {
      return(private$.runtime)
    }
  ),

  private = list(
    .delayed_object = NULL,
    .result = NULL,
    .runtime = NULL
  )
)

################################################################################

#' Sequential Delayed Jobs
#'
#' @description A \code{Job} that will evaluate immediately (i.e., in a
#' sequential fashion), blocking the current process until it completes.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @examples
#' d <- delayed(3 + 4)
#' sched <- Scheduler$new(d, SequentialJob)
#' @export
SequentialJob <- R6Class(
  classname = "SequentialJob",
  cloneable = FALSE,
  portable = TRUE,
  class = TRUE,
  inherit = Job,
  public = list(
    initialize = function(delayed_object) {
      to_eval <- delayed_object$prepare_eval()
      start_time <- proc.time()

      set.seed(delayed_object$seed)
      private$.result <- try(
        {
          eval_delayed(to_eval, delayed_object$timeout)
        },
        silent = TRUE
      )

      private$.runtime <- (proc.time() - start_time)[[3]]
      super$initialize(delayed_object)
    }
  ),

  active = list(
    finished = function() {
      return(TRUE)
    },
    value = function() {
      return(private$.result)
    }
  ),

  private = list(
    .worker = NULL,
    .result_uuid = NULL
  )
)

################################################################################

#' Future Delayed Jobs
#'
#' @description A \code{Job} that leverages the \code{future} framework to
#' evaluate asynchronously.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom future future resolved value
#'
#' @examples
#' library(future)
#' plan(multicore, workers = 1)
#' d <- delayed(3 + 4)
#' sched <- Scheduler$new(d, FutureJob, nworkers = 1)
#' @export
FutureJob <- R6Class(
  classname = "FutureJob",
  cloneable = FALSE,
  portable = TRUE,
  class = TRUE,
  inherit = Job,
  public = list(
    initialize = function(delayed_object) {
      env <- list(
        eval_delayed = eval_delayed,
        to_eval = delayed_object$prepare_eval(),
        timeout = delayed_object$timeout
      )

      private$.start_time <- proc.time()
      private$.future <- future(
        expr = quote(eval_delayed(to_eval, timeout)),
        # expr = to_eval$expr,
        # env = to_eval$env,
        substitute = FALSE,
        globals = env,
        packages = NULL,
        seed = delayed_object$seed
      )
      super$initialize(delayed_object)
    }
  ),

  active = list(
    finished = function() {
      finished <- resolved(private$.future)
      if (finished) {
        private$.runtime <- (proc.time() - private$.start_time)[[3]]
      }
      return(finished)
    },
    value = function() {
      if (is.null(private$.result)) {
        private$.result <- value(private$.future, signal = FALSE)
      }
      return(private$.result)
    }
  ),

  private = list(
    .future = NULL,
    .start_time = NULL
  )
)
