# Get latest PIP release

latest PIP release per identity

## Usage

``` r
get_latest_pip_release(identity = getOption("pipfun.identities"), ...)
```

## Arguments

- identity:

  character: one of "PROD", "INT", or "TEST"

- ...:

  Arguments passed on to
  [`get_pip_releases`](https://pip-technical-team.github.io/pipfun/reference/get_pip_releases.md)

  `verbose`

  :   logical: whether to display additional information

  `owner`

  :   character: owner of repo

  `repo`

  :   character: repository name

  `file_path`

  :   character: file or folder path

  `branch`

  :   character: branch where the file or folder is

  `creds`

  :   list. Basically, it is
      [`get_github_creds()`](https://pip-technical-team.github.io/pipfun/reference/get_github_creds.md)

## Value

data.table with most recent release

## Examples

``` r
get_latest_pip_release()
#> Git credentials are missing or invalid in non-interactive mode.
#>     release identity
#>       <int>   <char>
#> 1: 20260223     TEST
```
