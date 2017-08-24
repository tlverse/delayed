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
  
  result <- compute_sequential(z3)
  expect_equal(result,18)
})
