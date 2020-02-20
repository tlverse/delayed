.onAttach <- function(...) {
  packageStartupMessage(paste0(
    "delayed v",
    utils::packageDescription("delayed")$Version, ": ",
    utils::packageDescription("delayed")$Title
  ))
}
