# inspiration:
# https://distributed.readthedocs.io/en/latest/scheduling-policies.html
# https://distributed.readthedocs.io/en/latest/scheduling-state.html
# maintian both DAG and reverse DAG
# generate a task ordering:
# 1) do DFS
# 2) at a given depth, order nodes based on ndependencies.
# ndependencies 
# 3) if a delayed dynamically creates more, they get priority (top of the stack), using the above ordering
# 4) so, more or less maintain a stack
# when a worker comes free, it runs the next ready task in the stack
# (almost) always use futures for evaluation

# tasks have state (waiting, ready, running, done seems to be enough for our purposes)
# for now let's only do delayed on train(), because it returns a fit object and has no side effects

# DFS for next unresolved dependency
get_next_unresolved <- function(delayed_object){
  if(delayed_object$resolved){
    return(NULL)
  }
  
  unresolved_dependencies <- delayed_object$unresolved_dependencies
  if(length(unresolved_dependencies)>0){
    return(get_next_unresolved(unresolved_dependencies[[1]]))
  } else {
    return(delayed_object)
  }
}

#' @export
compute_sequential <- function(delayed_object){
  while(!delayed_object$resolved){
    next_unresolved <- get_next_unresolved(delayed_object)
    next_unresolved$resolve()
  }
  value <- delayed_object$resolve()
  
  return(value)
}

#define futures in the right order with the right resources
#manage available cores
#nested futures idea - priority queue?

#' @export
compute_future <- function(delayed_object){
  num_workers <- future::nbrOfWorkers()
  if(!is.finite(num_workers)){
    stop("Please specify number of workers in future::plan (manually if necessary)")
  }
  
  # for now assume we are managing all workers
  # todo: actually verify workers are free
  running_futures=list()
    
  #first compute all dependencies
  delayed_dependencies <- delayed_object$delayed_dependencies
  lapply(delayed_dependencies,compute_sequential)
  value <- delayed_object$resolve()
  
  return(value)
}

