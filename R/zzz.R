.onAttach <- function(...) {
  packageStartupMessage(paste0(
    "delayed v", utils::packageDescription("delayed")$Version,
    ": Framework for Parallelizing Dependent Tasks"
  ))
}
