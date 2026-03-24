# Set up a PIP working release in the local environment

This function resolves a release (either the latest or a specified one),
initializes package globals for that release, creates the required
filesystem layout, registers stamp aliases for each repository folder,
and persists the resulting state into the in-memory `.pipenv`
environment used by the package.

## Usage

``` r
setup_working_release(
  release = NULL,
  identity = getOption("pipfun.identities"),
  force = FALSE,
  owner = getOption("pipfun.ghowner"),
  repo = "pip_info",
  file_path = "releases.csv",
  branch = "releases",
  verbose = getOption("pipfun.verbose"),
  ppp = getOption("pipfun.ppps"),
  creds = NULL,
  main_dir = getOption("pipfun.main_dir"),
  alias_include_release = FALSE,
  ...
)
```

## Arguments

- release:

  character\|null: specific release identifier (e.g. a date string) to
  use. If `NULL`, the latest release is resolved via
  [`get_latest_pip_release()`](https://pip-technical-team.github.io/pipfun/reference/get_latest_pip_release.md).

- identity:

  character: identity token for the release. The default is read from
  `getOption("pipfun.identities")`. One of the allowed identity values
  must be supplied.

- force:

  logical: currently reserved for future use (kept for backward
  compatibility).

- owner:

  character: GitHub owner that contains the pip_info repo.

- repo:

  character: GitHub repository name that contains release metadata.
  Defaults to `"pip_info"`.

- file_path:

  character: path inside the repo to the releases file.

- branch:

  character: branch name to read the releases file from.

- verbose:

  logical: print progress messages when `TRUE`.

- ppp:

  numeric: PPP year to use; defaults to the option `pipfun.ppps` and is
  validated against it.

- creds:

  optional GitHub credentials object passed to GitHub helpers.

- main_dir:

  character: top-level directory where all PIP data repositories live.
  Defaults to `getOption("pipfun.main_dir")`.

- alias_include_release:

  logical: when `TRUE`, release-specific aliases will include the
  release string (useful for coexisting releases in a single stamp
  root).

- ...:

  Additional arguments passed to
  [`pip_create_globals()`](https://pip-technical-team.github.io/pipfun/reference/pip_create_globals.md).

## Value

Invisibly returns a list with elements `release`, `identity` and `ppp`
describing the working release that was set. The function also has side
effects: it writes objects to the package `.pipenv` environment
(`stamp_root`, `wrk_release`, `gls`, `folder_paths`, and `pip_aliases`).

## Details

The function intentionally separates concerns:

- release metadata resolution is handled via GitHub helper functions;

- global variables are created without creating directories in
  [`pip_create_globals()`](https://pip-technical-team.github.io/pipfun/reference/pip_create_globals.md)
  (so folder creation is explicit and controlled here);

- `stamp` aliases are registered for each repository folder so
  downstream code can refer to those repositories by short alias.

## Examples

``` r
if (FALSE) { # \dontrun{
setup_working_release()
} # }
```
