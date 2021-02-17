gen_rand <- function(x) {
  cat(sprintf("generating for x=%d\n", x))
  runif(1)
}
dgr <- delayed_fun(gen_rand)

# generate random numbers
set.seed(1234)
non_delayed1 <- unlist(lapply(1:3, gen_rand))

set.seed(1234)
non_delayed2 <- unlist(lapply(1:3, gen_rand))


set.seed(1234)
delayed_obj <- bundle_delayed(lapply(1:3, dgr))
delayed1 <- unlist(delayed_obj$compute())

set.seed(1234)
delayed_obj <- bundle_delayed(lapply(1:3, dgr))
delayed2 <- unlist(delayed_obj$compute())
test_that(
  "sequential delayed preserves ordering (needed for RNG reproducibility",
  expect_true(
    all(
      all.equal(non_delayed1, non_delayed2),
      all.equal(non_delayed1, delayed1),
      all.equal(non_delayed1, delayed2)
    )
  )
)
