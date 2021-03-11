library(future)
adder <- function(x, y) {
  Sys.sleep(2)
  x + y
}

delayed_adder <- delayed_fun(adder)
make_adder_list <- function() {
  bundle_delayed(lapply(1:4, delayed_adder, 4))
}

# big_adder <- make_adder_list()
# big_adder$compute()
# plot(big_adder)


big_adder <- make_adder_list()

time_sequential <- system.time({
  result_sequential <- big_adder$compute(SequentialJob)
})

nworkers <- 2

big_adder <- make_adder_list()
plan(multicore, workers = nworkers)
Sys.sleep(0.5)

time_future_mc <- system.time({
  result_future_mc <- big_adder$compute(FutureJob, nworkers = nworkers)
})


big_adder <- make_adder_list()
plan(multisession, workers = nworkers)
Sys.sleep(0.5)

time_future_ms <- system.time({
  result_future_ms <- big_adder$compute(FutureJob, nworkers = nworkers)
})

print(data.frame(
  val = c(
    sum(unlist(result_sequential)),
    sum(unlist(result_future_mc)),
    sum(unlist(result_future_ms))
  ),
  time = rbind(
    time_sequential, time_future_mc,
    time_future_ms
  )
))
