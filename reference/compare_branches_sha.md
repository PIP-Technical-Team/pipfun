# Compare the SHA of the latest commit of two branches

Compare the SHA of the latest commit of two branches

## Usage

``` r
compare_branches_sha(
  owner = getOption("pipfun.ghowner"),
  measure = NULL,
  repo = ifelse(is.null(measure), NA, paste0("aux_", measure)),
  branch1 = "main",
  branch2 = "DEV",
  verbose = FALSE
)
```

## Arguments

- owner:

  owner of repo

- measure:

  name of auxiliary repo

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

list of 3 elements: sha1, sha2 and updated (logical, TRUE if sha codes
are equal)
