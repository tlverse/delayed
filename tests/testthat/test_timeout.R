library(future)
adder <- function(x, y) {
  Sys.sleep(0.5)
  x + y
}

delayed_adder <- delayed_fun(adder, expect_error = TRUE)
make_adder_list <- function() {
  bundle_delayed(lapply(1:4, delayed_adder, 4))
}

# big_adder <- make_adder_list()
# big_adder$compute()
# plot(big_adder)


big_adder <- make_adder_list()
big_adder$timeout <- 1
result_sequential <- big_adder$compute(SequentialJob)
errors <- sapply(result_sequential, inherits, "try-error")
expect_equal(sum(errors), 3)

# plan(multicore, workers = 2)
# big_adder <- make_adder_list()
# big_adder$timeout <- 1
# result_sequential <- big_adder$compute()
# errors <- sapply(result_sequential, inherits, "try-error")
# expect_equal(sum(errors), 2)
