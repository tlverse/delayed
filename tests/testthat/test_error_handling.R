library(delayed)
library(testthat)
library(future)
context("Delayed Error Handling")

delayed_error <- delayed_fun(stop)
error_message <- "this is an error"


test_that("compute returns first error it hits", {
  broken_delayed <- delayed_error(error_message)
  expect_error(broken_delayed$compute(), error_message)
})

test_that("compute will return error instead of raising it if error is expected", {
  broken_delayed <- delayed_error(error_message)
  broken_delayed$expect_error <- TRUE
  result <- broken_delayed$compute()
  expect_error(stop(result), error_message)
})
