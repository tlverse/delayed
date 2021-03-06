% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/task_graph.R
\name{plot.Delayed}
\alias{plot.Delayed}
\title{Plot Method for Delayed Objects}
\usage{
\method{plot}{Delayed}(x, color = TRUE, height = "500px", width = "100\%", ...)
}
\arguments{
\item{x}{An object of class \code{Delayed} for which a task dependency graph
will be generated.}

\item{color}{If \code{TRUE}, color-code nodes according to status, and
display legend}

\item{height}{passed to visNetwork}

\item{width}{passed to visNetwork}

\item{...}{Additional arugments (passed to visNetwork).}
}
\description{
Plot Method for Delayed Objects
}
\examples{
adder <- function(x, y) {
  x + y
}
delayed_adder <- delayed_fun(adder)
z <- delayed_adder(3, 4)
z2 <- delayed_adder(z, 4)
z2$sequential <- TRUE
z3 <- delayed_adder(z2, z)
plot(z3)
}
