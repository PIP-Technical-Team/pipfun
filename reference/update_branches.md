# Update Branches of a GitHub Repository

This function compares the commit history and content between two
branches of a GitHub repository. If the content of the branches is
different, it updates `branch2` to match the latest commit of `branch1`.

## Usage

``` r
update_branches(
  owner = getOption("pipfun.ghowner"),
  repo,
  branch1,
  branch2,
  force = TRUE,
  verbose = TRUE
)
```

## Arguments

- owner:

  The GitHub username or organization name. Defaults to the option
  `"pipfun.ghowner"` if not specified.

- repo:

  The name of the GitHub repository.

- branch1:

  The source branch whose latest commit is used to update `branch2`.

- branch2:

  The target branch that will be updated to match the latest commit of
  `branch1`.

- force:

  logical. If `FALSE`, ask permission to user before merging. Default is
  TRUE

- verbose:

  logical. If `TRUE`, prints messages about the comparison and update
  process. Default is TRUE.

## Value

Returns `TRUE` if the update was successful or if the branches were
already up-to-date, `FALSE` if an error occurred during the update.
