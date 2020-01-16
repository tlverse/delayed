library(delayed)
library(testthat)
library(future)
context("Delayed")

# this fails because expects an actual expression
# todo: make delay just return objects unwrapped
# test_that("can generate delayed from simple expression",{
#   d <- delayed(3)
#   d$resolve()
#   expect_equal(d$value,3)
# })

test_that("can generate delayed from expression", {
  d <- delayed(3 + 4)
  d2 <- delayed(d + 4)
  d$compute()
  expect_equal(d$value, 7)
  d2$compute()
  expect_equal(d2$value, 11)
})

test_that("can generate delayed from nested expression", {
  d <- delayed(3 + (2 + 4))
  d$compute()
  expect_equal(d$value, 9)
})

test_that("can wrap function in delayed", {
  adder <- function(x, y) {
    x + y
  }
  # wrap a function to delay its evalaution
  delayed_adder <- delayed_fun(adder)

  # nest delayed objects
  z <- delayed_adder(3, 4)
  z2 <- delayed_adder(z, 4)
  # z2 can't resolve because it depends on unresolved z
  # expect_error(z2$resolve())

  # resolve first z and then z2
  z$compute()
  expect_equal(z$value, 7)
  z2$compute()
  expect_equal(z2$value, 11)
})


test_that("can wrap delayed in function", {
  wrapper <- function() {
    a <- 1
    b <- 4
    d <- delayed(a + b)
    return(d$compute())
  }

  expect_equal(wrapper(), 5)
})

test_that("more scoping tests", {
  adder <- function(x, y) {
    x + y
  }
  delayed_adder <- delayed_fun(adder)

  wrapper <- function(my_delayed_fun) {
    a <- 1
    b <- 4
    d <- my_delayed_fun(a, b)
    return(d$compute())
  }


  expect_equal(wrapper(delayed_adder), 5)

  wrapper2 <- function() {
    delayed_adder2 <- delayed_fun(adder)
    a <- 3
    b <- 1
    d <- delayed_adder(a, b)
    return(d)
  }

  expect_equal(wrapper2()$compute(), 4)
})



test_that("progress bar", {
  ident_fun <- function(x){Sys.sleep(0.01); x}
  delayed_ident <- delayed_fun(ident_fun)
  d_list <- lapply(1:1e2, delayed_ident)
  d_bundle <- bundle_delayed(d_list)
  res <- d_bundle$compute(progress=TRUE)

  
  delayed_adder <- delayed_fun(adder)
  
  wrapper <- function(my_delayed_fun) {
    a <- 1
    b <- 4
    d <- my_delayed_fun(a, b)
    return(d$compute())
  }
  
  
  expect_equal(wrapper(delayed_adder), 5)
  
  wrapper2 <- function() {
    delayed_adder2 <- delayed_fun(adder)
    a <- 3
    b <- 1
    d <- delayed_adder(a, b)
    return(d)
  }
  
  expect_equal(wrapper2()$compute(), 4)
})
