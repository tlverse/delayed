library(delayed)
library(testthat)

context("compute_sequential")
load_all()

test_that("can compute entire graph",{
  adder <- function(x,y){x+y}
  # wrap a function to delay its evalaution
  delayed_adder <- delayed_fun(adder)
  
  # nest delayed objects
  z <- delayed_adder(3,4)
  z2 <- delayed_adder(z,4)
  z3 <- delayed_adder(z2,z)
  
  result <- z3$compute()
  expect_equal(result,18)
})
# 
# delayed_adder <- delayed_fun(adder)
# z <- delayed_adder(3,4)
# z2 <- delayed_adder(z,4)
# z3 <- delayed_adder(3,z)
# z4 <- delayed_adder(z2,z3)
# z4_scheduler <- Scheduler$new(z4)
# 
# z4_scheduler$task_list
# 
# current_task <- z4_scheduler$next_ready_task
# while(!is.null(current_task)){
#   print(current_task)
#   current_task$make_future()
#   current_task <- z4_scheduler$next_ready_task
# }
# 
# 
# z4$resolved
# z4$value
