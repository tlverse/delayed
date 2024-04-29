library(future)

gen_rand <- function(x) {
  # cat(sprintf("generating for x=%d\n", x))
  x <- runif(1)
  Sys.sleep(runif(1,0,2))
  x
}

dgr <- delayed_fun(gen_rand)

# generate random numbers
set.seed(1234)
non_delayed_1 <- unlist(lapply(1:5, gen_rand))

set.seed(1234)
non_delayed_2 <- unlist(lapply(1:5, gen_rand))

plan(sequential)

set.seed(1234)
delayed_obj <- bundle_delayed(lapply(1:5, dgr))
delayed_sequential <- unlist(delayed_obj$compute())


plan(multicore, workers = 2)

set.seed(1234)
delayed_obj <- bundle_delayed(lapply(1:5, dgr))
delayed_mc_1 <- unlist(delayed_obj$compute())

set.seed(1234)
delayed_obj <- bundle_delayed(lapply(1:5, dgr))
delayed_mc_2 <- unlist(delayed_obj$compute())

# print(non_delayed_1)
# print(non_delayed_2)
# print(delayed_sequential)
# print(delayed_mc_1)
# print(delayed_mc_2)

test_that(
  "delayed preserves ordering (needed for RNG reproducibility)",
  expect_true(
    all(
      all.equal(non_delayed_1, non_delayed_2),
      all.equal(non_delayed_1, delayed_sequential),
      all.equal(delayed_mc_1, delayed_mc_2)
    )
  )
)
