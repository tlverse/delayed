.onAttach <- function(...) {
  packageStartupMessage("delayed: Framework for Parallelizing Dependent Tasks")
  packageStartupMessage(
    "Version: ",
    utils::packageDescription("delayed")$Version
  )
}
