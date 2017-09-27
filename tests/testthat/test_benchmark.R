library(future)

make_random_adder_graph <- function(seed){
  set.seed(seed)
  adder <- function(x,y){
    # Sys.sleep(0.5)
    x+y
  }
  delayed_adder <- delayed_fun(adder)

  nums <- floor(runif(3,1,10))
  delayed_tasks <- nums
  
  for(i in 1:100){
    rand_args<-sample(delayed_tasks,2)
    new_task <- delayed_adder(rand_args[[1]],rand_args[[2]])
    delayed_tasks<-c(delayed_tasks, new_task)
  }

  return(new_task)
}

adder_seed <- 1999

big_adder <- make_random_adder_graph(adder_seed)
big_adder$compute()
plot(big_adder)

big_adder <- make_random_adder_graph(adder_seed)


big_adder <- make_random_adder_graph(adder_seed)
time_sequential <- system.time({
  result_sequential <- big_adder$compute(SequentialJob)
})

nworkers = 4

big_adder <- make_random_adder_graph(adder_seed)
plan(multicore, workers = nworkers)
Sys.sleep(0.5)
time_future_mc <- system.time({
  result_future_mc <- big_adder$compute(FutureJob, nworkers=nworkers)
})

big_adder <- make_random_adder_graph(adder_seed)
plan(multisession, workers = nworkers)
Sys.sleep(0.5)
time_future_ms <- system.time({
  result_future_ms <- big_adder$compute(FutureJob, nworkers=nworkers)
})

data.frame(val=c(result_sequential, result_future_mc, 
                 result_future_ms), 
           time=rbind(time_sequential, time_future_mc, 
                      time_future_ms))