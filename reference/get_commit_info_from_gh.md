# Get info of latest commit of a GitHub repo

Get info of latest commit of a GitHub repo

## Usage

``` r
get_commit_info_from_gh(
  owner = getOption("pipfun.ghowner"),
  repo,
  branch = "main"
)
```

## Arguments

- owner:

  character: owner of repo

- repo:

  character: repository name

- branch:

  character: branch name (default is "main")

## Value

A list containing detailed information about the latest commit on the
specified branch.
