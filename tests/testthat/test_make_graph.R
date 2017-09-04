library(delayed)
library(testthat)

context("make_graph")


test_that("can plot graph",{
  adder <- function(x,y){x+y}
  # wrap a function to delay its evalaution
  delayed_adder <- delayed_fun(adder)
  
  # nest delayed objects
  z <- delayed_adder(3,4)
  z2 <- delayed_adder(z,4)
  z3 <- delayed_adder(z2,z)
  
  graph <- z3$make_graph()
  plot(graph,vertex.size=100, vertex.size2=10)  
})
