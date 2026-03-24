# Create or Update release branch of a GH repo

This function checks if a GitHub repository has a release branch. If a
release branch exists, it updates it with the latest `DEV` branch. If no
release branch exists, it creates a new one from `DEV`

## Usage

``` r
sync_release_branch(
  owner = getOption("pipfun.ghowner"),
  repo,
  ref_branch = "DEV",
  target_branch = NULL,
  verbose = FALSE
)
```

## Arguments

- owner:

  Character. The GitHub owner or organization name. Defaults to
  `getOption("pipfun.ghowner")`

- repo:

  Character. The name of the repository.

- ref_branch:

  Character. The branch from which the release branch should be created
  or updated. Defaults to `"DEV"`

- target_branch:

  Character. The name of the release branch to create or update. If
  `NULL`, it will be set to `paste0(release, "_", identity)`

- verbose:

  logical: whether to print messages about the process. Default is FALSE

## Value

Invisible `TRUE` if the process succeeds, otherwise an error message is
displayed

## Examples

``` r
if (FALSE) { # \dontrun{
sync_release_branch(owner = "PIP-Technical-Team", repo = "aux_gdp")} # }
```
