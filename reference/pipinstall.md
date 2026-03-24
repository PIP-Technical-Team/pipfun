# Install PIP packages.

This is wrapper around remotes::install_github(). By default the owner
of the repo is the PIP-Technical-team, which could be changed at any
time. This is just a convenient, yet restricted way to install PIP
package from GH.

## Usage

``` r
pipinstall(package, ref = "HEAD", owner = getOption("pipfun.ghowner"), ...)
```

## Arguments

- package:

  character: Name of the package

- ref:

  Desired git reference. Could be a commit, tag, or branch name, or a
  call to
  [`github_pull()`](https://remotes.r-lib.org/reference/github_refs.html)
  or
  [`github_release()`](https://remotes.r-lib.org/reference/github_refs.html).
  Defaults to `"HEAD"`, which means the default branch on GitHub and for
  git remotes. See
  [setting-the-default-branch](https://help.github.com/en/github/administering-a-repository/setting-the-default-branch)
  for more details.

- owner:

  character: Name of the GH owner. Default is
  `getOption("pipfun.ghowner")`

- ...:

  Other arguments passed on to
  [`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html).

## See also

[`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html)

## Examples

``` r
if (FALSE) { # \dontrun{
pipinstall("pipload")
} # }
```
