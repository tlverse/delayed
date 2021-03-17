library(future)

gen_rand <- function(x) {
  # cat(sprintf("generating for x=%d\n", x))
  x <- runif(1)
  Sys.sleep(x)
  x
}
dgr <- delayed_fun(gen_rand)

# generate random numbers
set.seed(1234)
non_delayed1 <- unlist(lapply(1:5, gen_rand))

set.seed(1234)
non_delayed2 <- unlist(lapply(1:5, gen_rand))

plan(sequential)

set.seed(1234)
delayed_obj <- bundle_delayed(lapply(1:5, dgr))
delayed1 <- unlist(delayed_obj$compute())


plan(multicore, workers = 2)

set.seed(1234)
delayed_obj <- bundle_delayed(lapply(1:5, dgr))
delayed2 <- unlist(delayed_obj$compute())

print(non_delayed1)
print(non_delayed2)
print(delayed1)
print(delayed2)

test_that(
  "delayed preserves ordering (needed for RNG reproducibility)",
  expect_true(
    all(
      all.equal(non_delayed1, non_delayed2),
      all.equal(delayed1, delayed2)
    )
  )
)
