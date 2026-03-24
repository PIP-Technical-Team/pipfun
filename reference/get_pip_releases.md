# Get PIP releases

All the releases available in PIP in any of the servers. These are the
releases available in the `releases` branch of the `pip_info` repo. If
you need to load the `releases` available in the `.pipenv` environment,
you may use `get_from_pipenv("releases")`

## Usage

``` r
get_pip_releases(
  owner = getOption("pipfun.ghowner"),
  repo = "pip_info",
  file_path = "releases.csv",
  branch = "releases",
  verbose = getOption("pipfun.verbose"),
  creds = NULL
)
```

## Arguments

- owner:

  character: owner of repo

- repo:

  character: repository name

- file_path:

  character: file or folder path

- branch:

  character: branch where the file or folder is

- verbose:

  logical: whether to display additional information

- creds:

  list. Basically, it is
  [`get_github_creds()`](https://pip-technical-team.github.io/pipfun/reference/get_github_creds.md)

## Value

data.table with releases table

## Examples

``` r
get_pip_releases()
#> Git credentials are missing or invalid in non-interactive mode.
#>      release identity
#>        <int>   <char>
#>  1: 20240326     PROD
#>  2: 20241101     PROD
#>  3: 20250203     TEST
#>  4: 20250501     TEST
#>  5: 20250711     TEST
#>  6: 20250717     TEST
#>  7: 20250718     TEST
#>  8: 20250721     TEST
#>  9: 20250725     TEST
#> 10: 20250730     TEST
#> 11: 20250805     TEST
#> 12: 20250806     TEST
#> 13: 20250807     TEST
#> 14: 20250808     TEST
#> 15: 20250810     TEST
#> 16: 20250811     TEST
#> 17: 20251205     TEST
#> 18: 20251211     TEST
#> 19: 20260203     TEST
#> 20: 20260204     TEST
#> 21: 20260206     TEST
#> 22: 20260202     TEST
#> 23: 20260101     TEST
#> 24: 20260221     TEST
#> 25: 20260223     TEST
#>      release identity
#>        <int>   <char>
```
