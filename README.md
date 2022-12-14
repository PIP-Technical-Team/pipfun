
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipfun

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/PIP-Technical-Team/pipfun/branch/main/graph/badge.svg)](https://app.codecov.io/gh/PIP-Technical-Team/pipfun?branch=main)
<!-- badges: end -->

The goal of pipfun is to …

## Installation

You can install the development version of pipfun from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("PIP-Technical-Team/pipfun")
```

## Examples

This is a basic example which shows you how to solve a common problem:

``` r
library(pipfun)
## basic example code
```

### Saving files

Save a list

``` r

  tdir <- fs::path_temp("pipfun-l")

  lx <- list(x = 1)
  measure <- "ltst"

  pip_sign_save(x = lx,
              measure = measure,
              msrdir = tdir,
              save_dta = TRUE)
#> ! Data signature has changed
#> 'ltst.rds' has been updated
  
  fs::dir_tree(tdir)
#> C:/Users/wb384996/AppData/Local/Temp/RtmpSWrAQv/pipfun-l
#> +-- ltst.qs
#> +-- ltst.rds
#> +-- ltst_datasignature.txt
#> \-- _vintage
#>     +-- ltst_20220809123559.qs
#>     \-- ltst_20220809123559.rds
  fs::dir_delete(tdir)
```

Save a data-frame

``` r

  tdir <- fs::path_temp("pipfun-df")

  lx <- data.frame(x = 1)
  measure <- "ltst"

  pip_sign_save(x = lx,
              measure = measure,
              msrdir = tdir,
              save_dta = TRUE)
#> Warning: package 'fstcore' was built under R version 4.1.3
#> ! Data signature has changed
#> 'ltst.fst' has been updated
  
  fs::dir_tree(tdir)
#> C:/Users/wb384996/AppData/Local/Temp/RtmpSWrAQv/pipfun-df
#> +-- ltst.dta
#> +-- ltst.fst
#> +-- ltst.qs
#> +-- ltst_datasignature.txt
#> \-- _vintage
#>     +-- ltst_20220809123602.dta
#>     +-- ltst_20220809123602.fst
#>     \-- ltst_20220809123602.qs
  fs::dir_delete(tdir)
```
