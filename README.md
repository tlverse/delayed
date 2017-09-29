
<!-- README.md is generated from README.Rmd. Please edit that file -->
R/`delayed`
===========

[![Travis-CI Build Status](https://travis-ci.org/jeremyrcoyle/delayed.svg?branch=master)](https://travis-ci.org/jeremyrcoyle/delayed) [![Build status](https://ci.appveyor.com/api/projects/status/wid97j656ga0elme?svg=true)](https://ci.appveyor.com/project/jeremyrcoyle/delayed) [![Coverage Status](https://img.shields.io/codecov/c/github/jeremyrcoyle/delayed/master.svg)](https://codecov.io/github/jeremyrcoyle/delayed?branch=master) [![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

> A framework for parallelizing dependent tasks

**Author:** [Jeremy Coyle](https://github.com/jeremyrcoyle)

------------------------------------------------------------------------

What's `delayed`?
-----------------

`delayed` is an R package that provides a framework for parallelizing dependent tasks in an efficient manner. It brings to R a subset of the functionality implemented in Python's [Dask library](https://dask.pydata.org/en/latest/). For details on how best to use `delayed`, please consult the package [documentation](https://nhejazi.github.io/delayed/) and [vignette](https://nhejazi.github.io/delayed/articles/delayed.html) online, or do so from within [R](https://www.r-project.org/).

------------------------------------------------------------------------

Installation
------------

<!--
For standard use, we recommend installing the package from
[CRAN](https://cran.r-project.org/) via


```r
install.packages("delayed")
```
-->
Install the most recent *stable release* from GitHub via [`devtools`](https://www.rstudio.com/products/rpackages/devtools/):

``` r
devtools::install_github("jeremyrcoyle/delayed")
```

------------------------------------------------------------------------

Issues
------

If you encounter any bugs or have any specific feature requests, please [file an issue](https://github.com/jeremyrcoyle/delayed/issues).

------------------------------------------------------------------------

Example
-------

This minimal example shows how to use `delayed` to handle dependent computations via chaining of tasks:

``` r
library(delayed)
#> delayed: Framework for Parallelizing Dependent Tasks
#> Version: 0.2.1

# delay a function that does a bit of math
mapfun <- function(x, y) {(x + y) / (x - y)}
delayed_mapfun <- delayed_fun(mapfun)

set.seed(14765)
library(future)
plan(multicore, workers = 2)
const <- 7

# re-define the delayed object from above
delayed_norm <- delayed(rnorm(n = const))
delayed_pois <- delayed(rpois(n = const, lambda = const))
chained_norm_pois <- delayed_mapfun(delayed_norm, delayed_pois)

# compute it using the future plan (multicore with 2 cores)
chained_norm_pois$compute(nworkers = 2, verbose = TRUE)
#> run:0 ready:2 workers:2
#> updating rpois(n = const, lambda = const) from ready to running
#> run:1 ready:1 workers:2
#> updating rnorm(n = const) from ready to running
#> run:2 ready:0 workers:2
#> updating rnorm(n = const) from running to resolved
#> updating rpois(n = const, lambda = const) from running to resolved
#> updating mapfun(x = delayed_norm, y = delayed_pois) from waiting to ready
#> run:0 ready:1 workers:2
#> updating mapfun(x = delayed_norm, y = delayed_pois) from ready to running
#> run:1 ready:0 workers:2
#> updating mapfun(x = delayed_norm, y = delayed_pois) from running to resolved
#> [1] -0.8580934 -0.5487926 -2.2290142 -1.0234833 -1.4300754 -0.8045187
#> [7] -1.0919853
```

*Remark:* In the above, the delayed computation is carried out in parallel using the framework offered by the excellent [`future` package](https://github.com/HenrikBengtsson/future).

------------------------------------------------------------------------

License
-------

© 2017 [Jeremy R. Coyle](https://github.com/jeremyrcoyle)

The contents of this repository are distributed under the GPL-3 license. See file `LICENSE` for details.
