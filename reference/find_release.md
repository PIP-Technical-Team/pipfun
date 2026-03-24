# Find release in releases table

Find release in releases table

## Usage

``` r
find_release(pr = NULL, release, identity)
```

## Arguments

- pr:

  PIP Releases table from
  [get_pip_releases](https://pip-technical-team.github.io/pipfun/reference/get_pip_releases.md)

- release:

  character: release name in the form of "%Y%m%d". Defaults to current
  date.

- identity:

  character: one of "PROD", "INT", or "TEST"

## Value

invisible data frame with filtered release
