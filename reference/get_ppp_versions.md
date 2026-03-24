# get most recent version of PPP

THis data is extracted from the `aux_ppp` repo in branch DEV_V2

## Usage

``` r
get_ppp_versions()

get_latest_ppp_versions(ppps = getOption("pipfun.ppps"))
```

## Arguments

- ppps:

  numeric: vector of PPP years.

## Value

data frame with all PPP years and versions available

data frame with most recent versions for `ppps` selected

## Examples

``` r
get_ppp_versions()
#> Git credentials are missing or invalid in non-interactive mode.
#>    ppp_year ppp_rv ppp_av
#>       <int>  <int>  <int>
#> 1:     2005      1      1
#> 2:     2011      1      1
#> 3:     2011      1      2
#> 4:     2011      2      1
#> 5:     2011      2      2
#> 6:     2017      1      1
#> 7:     2017      1      2
```
