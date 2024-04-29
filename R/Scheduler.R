#' Scheduler class that orders compute tasks and dispatches tasks to workers
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom rstackdeque rstack
#' @importFrom future plan
#' @importFrom progress progress_bar
#'
#' @examples
#' d <- delayed(3 + 4)
#' sched <- Scheduler$new(d, SequentialJob)
#' sched$compute()
#' @export
Scheduler <- R6Class(
  classname = "Scheduler",
  cloneable = FALSE,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(delayed_object,
                          job_type = FutureJob,
                          nworkers = NULL,
                          verbose = FALSE,
                          progress = FALSE, ...) {
      private$.delayed_object <- delayed_object
      private$.task_lists <- list(
        waiting = env(),
        ready = env(),
        running = env(),
        resolved = env(),
        error = env()
      )

      self$enumerate_tasks(delayed_object)

      private$.n_tasks <- sum(sapply(private$.task_lists, length))

      if (progress) {
        private$.progress <- progress_bar$new(total = private$.n_tasks)
      }
      private$.job_type <- job_type

      if (is.null(nworkers)) {
        if (job_type$classname == "FutureJob") {
          nworkers <- formals(plan("next"))$workers
          if (is.null(nworkers)) {
            nworkers <- 1
          }
        } else {
          nworkers <- 1
        }
      }
      private$.nworkers <- nworkers
      private$.verbose <- verbose
      invisible(self)
    },

    print = function() {
      print(private$.delayed_object)
    },

    enumerate_tasks = function(delayed_object,
                               dependent_uuid = NULL) {
      state <- delayed_object$update_state
      uuid <- delayed_object$uuid
      
      if (!(delayed_object$sequential || ("sequential"%in%attr(plan(),"class")))) {
        # if we're using a parallel future plan, 
        # we should set seeds for reproducibility
        delayed_object$seed <- runif(1,0,1e6)
      } 
      
      
      private$.n_tasks <- private$.n_tasks + 1
      delayed_object$task_order <- private$.n_tasks
      assign(uuid, delayed_object, envir = private$.task_lists[[state]])
      if (!is.null(dependent_uuid)) {
        delayed_object$register_dependent(dependent_uuid)
      }
      unresolved <- delayed_object$unresolved_dependencies

      if (length(unresolved) > 0) {
        lapply(unresolved, self$enumerate_tasks, dependent_uuid = uuid)
      }
    },

    update_task = function(task, old_category, new_category) {
      if (private$.verbose) {
        message(sprintf(
          "updating %s from %s to %s", task$name,
          old_category, new_category
        ))
      }
      rm(list = task$uuid, envir = private$.task_lists[[old_category]])
      assign(task$uuid, task, private$.task_lists[[new_category]])
    },

    update_tasks = function(category, search_list = NULL) {
      task_env <- private$.task_lists[[category]]

      if (is.null(search_list)) {
        search_list <- ls(task_env)
      }

      changed_uuids <- list()
      for (task_uuid in search_list) {
        task <- task_env[[task_uuid]]
        state <- task$update_state
        if (state != category) {
          self$update_task(task, category, state)
          changed_uuids <- c(changed_uuids, task$uuid)
        }
      }

      return(changed_uuids)
    },
    compute_step = function() {
      updated_tasks <- c()
      nrunning <- length(ls(private$.task_lists[["running"]]))
      nready <- length(ls(private$.task_lists[["ready"]]))

      if (private$.verbose) {
        message(sprintf(
          "run:%d ready:%d workers:%d",
          nrunning, nready, private$.nworkers
        ))
      }

      if ((nready > 0) && (nrunning < private$.nworkers)) {
        # get a ready task and assign it to a worker
        current_task <- self$next_ready_task

        if (!is.null(current_task)) {
          job_type <- private$.job_type

          if (current_task$sequential || ("sequential"%in%attr(plan(),"class"))) {
            SequentialJob$new(current_task)
            self$update_task(current_task, "ready", "running")
          } else {
            current_task$timeout <- self$time_left
            job <- job_type$new(current_task)
            self$update_task(current_task, "ready", "running")
          }

          updated_tasks <- c(current_task)
        }
      } else {
        # check for newly completed tasks
        completed <- self$update_tasks("running")
        # completed <- 1
        # if any tasks completed, update ready tasks
        if (length(completed) > 0) {
          newly_completed <- mget(
            unlist(completed),
            private$.task_lists[["resolved"]]
          )
          updated_tasks <- c(updated_tasks, newly_completed)
          lapply(newly_completed, `[[`, "value") # force value collection
          # check for errors (currently detected on Delayed$value)
          # force value collection
          new_states <- sapply(newly_completed, `[[`, "state")
          if (any(new_states == "error")) {
            errored_tasks <- newly_completed[which(new_states == "error")]
            first_error <- errored_tasks[[1]]
            message(sprintf("Failed on %s", first_error$name))
            stop(first_error$value)
          }
          all_dependents <- unique(unlist(lapply(
            newly_completed,
            `[[`, "dependents"
          )))

          ready <- self$update_tasks("waiting", all_dependents)
          if (length(ready) > 0) {
            newly_ready <- mget(
              unlist(ready),
              private$.task_lists[["ready"]]
            )
            updated_tasks <- c(updated_tasks, newly_ready)
          }
        }
      }

      if (!is.null(private$.progress)) {
        complete_or_error <- length(private$.task_lists$resolved) +
          length(private$.task_lists$error)
        private$.progress$update(complete_or_error / private$.n_tasks)
      }
      return(updated_tasks)
    },
    compute = function() {
      private$.start_time <- proc.time()
      while (!private$.delayed_object$resolved) {
        updated_tasks <- self$compute_step()
        if (length(updated_tasks) == 0) {
          # nothing was updated, so lets wait a bit before we check again
          Sys.sleep(0.01)
        }
      }

      return(private$.delayed_object$value)
    }
  ),

  active = list(
    name = function() {
      return(private$.name)
    },

    task_lists = function() {
      return(private$.task_lists)
    },

    next_ready_task = function() {
      ready_tasks <- as.list(private$.task_lists[["ready"]])
      if (length(ready_tasks) > 0) {
        counts <- sapply(
          ready_tasks,
          closest_dependent_count,
          private$.task_lists[["waiting"]]
        )

        orders <- sapply(ready_tasks, `[[`, "task_order")
        nrt <- ready_tasks[order(counts, orders)][[1]]
        return(nrt)
      } else {
        return(NULL)
      }
    },
    delayed_object = function() {
      return(private$.delayed_object)
    },
    time_left = function() {
      timeout <- self$delayed_object$timeout
      if (is.null(timeout)) {
        return(Inf)
      } else {
        time_elapsed <- (proc.time() - private$.start_time)[[3]]
        time_left <- timeout - time_elapsed
      }

      return(time_left)
    }
  ),

  private = list(
    .delayed_object = NULL,
    .stack = NULL,
    .task_list = NULL,
    .job_type = NULL,
    .workers = NULL,
    .nworkers = NULL,
    .verbose = FALSE,
    .progress = NULL,
    .n_tasks = 0,
    .task_lists = list(),
    .start_time = NULL
  )
)

###############################################################################

closest_dependent_count <- function(task, waiting_tasks) {
  if (length(task$dependents) == 0) {
    return(0)
  }
  dependents <- mget(task$dependents, waiting_tasks)

  dependent_counts <- sapply(dependents, function(dependent) {
    length(dependent$unresolved_dependencies)
  })
  return(min(dependent_counts))
}
