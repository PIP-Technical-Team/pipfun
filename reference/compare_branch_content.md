# Compare content of two branches

Compare content of two branches

## Usage

``` r
compare_branch_content(
  owner = getOption("pipfun.ghowner"),
  repo,
  branch1 = "main",
  branch2 = "dev",
  verbose = TRUE
)
```

## Arguments

- owner:

  owner of repo

- repo:

  character: name of repository

- branch1:

  character: name of one branch

- branch2:

  character: name of the other branch

- verbose:

  logical: whether to print messages about the comparison. Default is
  FALSE

## Value

list of 3 elements: tree sha of branch 1, tree sha of branch 2 and "same
content" (TRUE if branches have same content, FALSE otherwise)

## Examples

``` r
if (FALSE) { # \dontrun{
# Different content
compare_branch_content(repo   = "aux_ppp",
                      branch1 = "DEV",
                      branch2 = "DEV_v2")} # }
```
