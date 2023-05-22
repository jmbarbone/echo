
<!-- README.md is generated from README.Rmd. Please edit that file -->

# echo

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/echo)](https://CRAN.R-project.org/package=echo) -->
[![R-CMD-check](https://github.com/jmbarbone/echo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jmbarbone/echo/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jmbarbone/echo/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jmbarbone/echo?branch=main)
<!-- badges: end -->

The goal of `{echo}` is to provide a function for evaluating and logging
a script.

## Installation

Install `{echo}` from CRAN with:

``` r
install.packages("echo")
```

Alternatively, you can install the development version of `{echo}`
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("jmbarbone/echo")

## Example

`echo()` allows you to print

``` r
library(echo)
try(echo(
  expr = {
    print(1 + 1)
    df <- data.frame(a = 1:5, b = letters[1:5])
    print(df)
    message("hello!")
    warning("hello!")
    stop("error here")
  },
  level = 0
), silent = TRUE)
#> [2023-05-22 09:58:20] [EXP] print(1 + 1)
#> [2023-05-22 09:58:20] [OUT] #> [1] 2
#> [2023-05-22 09:58:20] [EXP] df <- data.frame(a = 1:5, b = letters[1:5])
#> [2023-05-22 09:58:20] [EXP] print(df)
#> [2023-05-22 09:58:20] [OUT] #>   a b
#> [2023-05-22 09:58:20] [OUT] #> 1 1 a
#> [2023-05-22 09:58:20] [OUT] #> 2 2 b
#> [2023-05-22 09:58:20] [OUT] #> 3 3 c
#> [2023-05-22 09:58:20] [OUT] #> 4 4 d
#> [2023-05-22 09:58:20] [OUT] #> 5 5 e
#> [2023-05-22 09:58:20] [EXP] message("hello!")
#> [2023-05-22 09:58:20] [MSG] #> hello!
#> [2023-05-22 09:58:20] [EXP] warning("hello!")
#> [2023-05-22 09:58:20] [WRN] #> hello!
#> [2023-05-22 09:58:20] [EXP] stop("error here")
#> [2023-05-22 09:58:20] [ERR] #> error here
```
