
<!-- README.md is generated from README.Rmd. Please edit that file -->

# echo

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/echo)](https://CRAN.R-project.org/package=echo)
[![R-CMD-check](https://github.com/jmbarbone/echo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jmbarbone/echo/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jmbarbone/echo/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jmbarbone/echo?branch=main)
<!-- badges: end -->

The goal of `{echo}` is to provide a function for evaluating and logging
a script.

## Installation

You can install the development version of `{echo}` like so:

``` r
remotes::install_github("jmbarbone/echo")
```

## Example

`echo()` allows you to print

``` r
library(echo)
try(echo(level = 0, exprs = {
  print(1 + 1)
  data.frame(a = 1:5, b = letters[1:5])
  message("hello!")
  warning("hello!")
  stop("error here")
}), silent = TRUE)
#> [2023-02-10 05:47:17] [EXP] print(1 + 1)
#> [2023-02-10 05:47:17] [OUT] #> [1] 2
#> [2023-02-10 05:47:17] [EXP] data.frame(a = 1:5, b = letters[1:5])
#> [2023-02-10 05:47:17] [EXP] message("hello!")
#> [2023-02-10 05:47:17] [MSG] #> hello!
#> [2023-02-10 05:47:17] [EXP] warning("hello!")
#> [2023-02-10 05:47:17] [WRN] #> hello!
#> [2023-02-10 05:47:17] [EXP] stop("error here")
#> [2023-02-10 05:47:17] [ERR] #> error here
```
