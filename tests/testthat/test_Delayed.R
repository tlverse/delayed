library(delayed)
library(testthat)

context("Delayed")
load_all()

#this fails because expects an actual expression
# test_that("can generate delayed from simple expression",{
#   d <- delayed(3)
#   d$resolve()
#   expect_equal(d$value,3)
# })

test_that("can generate delayed from expression",{
  d <- delayed(3+4)
  d$resolve()
  expect_equal(d$value,7)
})

test_that("can generate delayed from nested expression",{
  d <- delayed(3+(2+4))
  d$resolve()
  expect_equal(d$value,9)
})

test_that("can wrap function in delayed",{
  adder <- function(x,y){x+y}
  # wrap a function to delay its evalaution
  delayed_adder <- delayed_fun(adder)

  # nest delayed objects
  z <- delayed_adder(3,4)
  z2 <- delayed_adder(z,4)
  
  #z2 can't resolve because it depends on unresolved z
  # expect_error(z2$resolve())
  
  #resolve first z and then z2
  z$resolve()
  expect_equal(z$value,7)
  z2$resolve()
  expect_equal(z2$value,11)
})

