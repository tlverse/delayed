#' Scheduler class that orders compute tasks and dispatches tasks to workers via \code{make_future()}
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom rstackdeque rstack
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
                            verbose = FALSE, ...) {

        private$.delayed_object <- delayed_object

        private$.task_lists <- list(waiting = env(),
                                    ready = env(),
                                    running = env(),
                                    resolved = env()
                                  )

        self$enumerate_tasks(delayed_object)
        private$.job_type <- job_type

        if (is.null(nworkers)) {
          if (job_type$classname == "FutureJob") {
            nworkers = future::availableCores()
          } else {
            nworkers = 1
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
        assign(uuid, delayed_object, envir = private$.task_lists[[state]])
        if (!is.null(dependent_uuid)) {
          delayed_object$register_dependent(dependent_uuid)
        }
        unresolved <- delayed_object$unresolved_dependencies

        if (length(unresolved) > 0) {
          lapply(unresolved,self$enumerate_tasks, dependent_uuid = uuid)
        }
      },

      update_task = function(task, old_category, new_category) {

        if (private$.verbose) {
          message(sprintf("updating %s from %s to %s", task$name,
                          old_category, new_category))
        }
        rm(list = task$uuid, envir = private$.task_lists[[old_category]])
        assign(task$uuid, task, private$.task_lists[[new_category]])
      },

      update_tasks = function(category, search_list = NULL) {
        task_env <- private$.task_lists[[category]]

        if (is.null(search_list)) {
          search_list = ls(task_env)
        }

        changed_uuids = list()
        for (task_uuid in search_list) {
          task <- task_env[[task_uuid]]
          state <- task$update_state
          if (state != category) {
            self$update_task(task, category, state)
            changed_uuids = c(changed_uuids, task$uuid)
          }
        }

        return(changed_uuids)
      },

      compute = function() {
        while (!private$.delayed_object$resolved) {
          nrunning <- length(ls(private$.task_lists[['running']]))
          nready <- length(ls(private$.task_lists[['ready']]))

          if (private$.verbose) {
            message(sprintf("run:%d ready:%d workers:%d",
                            nrunning, nready, private$.nworkers))
          }

          if ((nready > 0) && (nrunning<private$.nworkers)) {
            # get a ready task and assign it to a worker
            current_task <- self$next_ready_task

            if (!is.null(current_task)) {
              job_type <- private$.job_type

              if (current_task$sequential) {
                SequentialJob$new(current_task)
                self$update_task(current_task, "ready", "running")
              } else {
                job <- job_type$new(current_task)
                self$update_task(current_task, "ready", "running")
              }
            }
          } else {
            # check for newly completed tasks
            completed <- self$update_tasks("running")
            # completed <- 1
            # if any tasks completed, update ready tasks
            if (length(completed) > 0) {
              newly_completed <- mget(unlist(completed),
                                      private$.task_lists[["resolved"]])
              lapply(newly_completed, `[[`,"value") # force value collection
              all_dependents <- unique(unlist(lapply(newly_completed,
                                       `[[`,"dependents")))

              ready <- self$update_tasks("waiting", all_dependents)
            }
          }
        }
        return(private$.delayed_object$value)
      }
    ),

    active = list(
      name = function() {
        return(private$.name)
      },

      waiting_tasks = function() {
        return(private$.waiting_tasks)
      },

      ready_tasks = function() {
        return(private$.ready_tasks)
      },

      running_tasks = function() {
        return(private$.running_tasks)
      },

      resolved_tasks = function() {
        return(private$.resolved_tasks)
      },

      task_lists = function() {
        return(private$.task_lists)
      },

      next_ready_task = function() {
        ready_tasks <- as.list(private$.task_lists[['ready']])
        if (length(ready_tasks) > 0) {
          counts <- sapply(ready_tasks,
                           closest_dependent_count,
                           private$.task_lists[['waiting']])
          return(ready_tasks[[which.min(counts)]])
        } else {
          return(NULL)
        }
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
      .task_lists = list()
    )
)

################################################################################

closest_dependent_count <- function(task, waiting_tasks) {

  if(length(task$dependents) == 0) {
    return(0)
  }
  dependents <- mget(task$dependents, waiting_tasks)

  dependent_counts <- sapply(dependents, function(dependent) {
    length(dependent$unresolved_dependencies)
  })
  return(min(dependent_counts))
}

