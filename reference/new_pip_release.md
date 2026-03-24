# Create new release for PIP update

CAUTION: Use this functions with care.

## Usage

``` r
new_pip_release(
  release = format(Sys.Date(), "%Y%m%d"),
  identity = getOption("pipfun.identities"),
  verbose = getOption("pipfun.verbose"),
  root_dir = Sys.getenv("PIP_ROOT_DIR"),
  working_dir = fs::path(root_dir, getOption("pipfun.working_dir")),
  ppps = getOption("pipfun.ppps"),
  ...
)

remove_pip_release(
  release,
  identity = getOption("pipfun.identities"),
  verbose = getOption("pipfun.verbose"),
  working_dir = NULL,
  ppps = getOption("pipfun.ppps"),
  confirm_remove = getOption("pipfun.confirm_remove"),
  ...
)
```

## Arguments

- release:

  character: release name in the form of "%Y%m%d". Defaults to current
  date.

- identity:

  character. Defaults to the first value of
  `getOption("pipfun.identities")`

- verbose:

  A logical: whether to print detailed messages about the process. The
  default is `TRUE`

- root_dir:

  character: Root directory for PIP data, defaults to
  `Sys.getenv("PIP_ROOT_DIR")`

- working_dir:

  character: Working directory where files will be created. Defaults to
  a subdirectory of `root_dir`

- ppps:

  numeric: vector of PPP years.

- ...:

  Arguments passed on to
  [`get_pip_releases`](https://pip-technical-team.github.io/pipfun/reference/get_pip_releases.md)

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

- confirm_remove:

  logical: whether to ask for confirmation before removing folders in
  `remove_pip_release()`. Defaults to
  `getOption("pipfun.confirm_remove")`

## Value

invisible TRUE if everything went fine

invisible

## Examples

``` r
if (FALSE) { # \dontrun{
new_pip_release()
} # }
```
