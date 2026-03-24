# Create new branch in repo

By default it will create a new branch in aux PIP repo, but it can
create a new branch in repo

## Usage

``` r
create_new_branch(
  measure = NULL,
  owner = getOption("pipfun.ghowner"),
  repo = ifelse(is.null(measure), NA, paste0("aux_", measure)),
  ref_branch = "DEV",
  new_branch = NULL,
  verbose = getOption("pipfun.verbose")
)
```

## Arguments

- measure:

  character: measure to be loaded

- owner:

  character: Github repo owner. Default is `getOption("pipfun.ghowner")`

- repo:

  character: name of the repo

- ref_branch:

  Character: reference branch from which the new branch will be created.

- new_branch:

  character: name of new branch. Default is
  `paste0(release, "_", identity)`

- verbose:

  A logical: whether to print detailed messages about the process. The
  default is `TRUE`

## Value

TRUE if new branch already exists or if it was created

## Examples

``` r
if (FALSE) { # \dontrun{
# success
  create_new_branch("regions",
  release = "20240903")

  create_new_branch("regions",
  new_branch = "test")

# Fail
  create_new_branch("fjfjf",
  new_branch = "test") |>
  try()} # }
```
