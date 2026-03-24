# delete branch in Github repo

delete branch in Github repo

## Usage

``` r
delete_branch(
  branch_to_delete,
  measure = NULL,
  owner = getOption("pipfun.ghowner"),
  repo = ifelse(is.null(measure), NA, paste0("aux_", measure)),
  ask = interactive(),
  verbose = getOption("pipfun.verbose")
)
```

## Arguments

- branch_to_delete:

  character: branch to delete

- measure:

  character: measure to be loaded

- owner:

  character: Github repo owner. Default is `getOption("pipfun.ghowner")`

- repo:

  character: name of the repo

- ask:

  logical: whether to ask the user to confirm. Default is
  [`interactive()`](https://rdrr.io/r/base/interactive.html)

- verbose:

  A logical: whether to print detailed messages about the process. The
  default is `TRUE`

## Value

logical, whether or not branch was deleted

## Examples

``` r
if (FALSE) { # \dontrun{
create_new_branch(
  measure = "regions",
  release = "20240903")

delete_branch(branch_to_delete = "20240903_PROD",
              measure = "regions",
              ask = FALSE)

create_new_branch("regions",
new_branch = "test")

delete_branch(branch_to_delete = "test",
              measure = "regions",
              ask = FALSE)} # }
```
