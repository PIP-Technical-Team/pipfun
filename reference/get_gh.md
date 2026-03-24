# Get tags from specific Github repo

Get tags from specific Github repo

## Usage

``` r
get_gh(owner, repo, what = c("tags", "branches", "releases", "contents"))
```

## Arguments

- owner:

  character: Github username that owns the repo

- repo:

  character: Github repository name

- what:

  character: either "tags" or "branches"

## Value

character vector with tags
