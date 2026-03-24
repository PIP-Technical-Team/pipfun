# confirm branch exists in repo

confirm branch exists in repo

## Usage

``` r
confirm_branch_exists(
  branch,
  measure = NULL,
  owner = getOption("pipfun.ghowner"),
  repo = ifelse(is.null(measure), NA, paste0("aux_", measure))
)
```

## Arguments

- branch:

  branch to confirm

- measure:

  name of auxiliary repo

- owner:

  owner of repo

- repo:

  character: name of repository

## Value

logical

## Examples

``` r
if (FALSE) { # \dontrun{
# Exists
confirm_branch_exists("DEV", "regions")

# Does not exist
confirm_branch_exists("ijijiji", "regions")} # }
```
