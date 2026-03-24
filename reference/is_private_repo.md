# check if the Repo is private or not

check if the Repo is private or not

## Usage

``` r
is_private_repo(
  measure = NULL,
  owner = getOption("pipfun.ghowner"),
  repo = paste0("aux_", measure)
)
```

## Arguments

- measure:

  character: measure to be loaded

- owner:

  character: Github repo owner. Default is `getOption("pipfun.ghowner")`

- repo:

  character: name of the repo

## Value

logical

## Examples

``` r
if (FALSE) { # \dontrun{
is_private_repo("cpi")
is_private_repo("nan")} # }
```
