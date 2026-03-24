# Create and return directory paths for a PIP release

This function ensures the on-disk layout required by pipfun exists and
returns a named list with the canonical paths. The function will create
missing directories and initialize a `stamp` root at `main_dir` if one
has not already been initialized. The `rt` suffix (release_identity) is
appended to release-specific folders so multiple releases can coexist.

## Usage

``` r
set_pip_folders(
  main_dir = getOption("pipfun.main_dir"),
  release = NULL,
  identity = getOption("pipfun.identities"),
  verbose = getOption("pipfun.verbose")
)
```

## Arguments

- main_dir:

  character: top-level directory where PIP repositories are stored.

- release:

  character: release identifier (e.g. a date string). If `NULL`, the
  latest release is resolved via
  [`get_latest_pip_release()`](https://pip-technical-team.github.io/pipfun/reference/get_latest_pip_release.md).

- identity:

  character: identity token used as suffix. Default comes from
  `getOption("pipfun.identities")` and is validated with
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html).

- verbose:

  logical: print progress messages when `TRUE`.

## Value

A named list of paths (class `pip_folder_paths`) containing
`stamp_root`, `aux_data`, `aux_metadata`, `dlw_data`, `dlw_inventory`,
`dlw_metadata`, `pip_data`, `pip_metadata`, `pip_inventory`, and
`pip_master_inventory`.

## Examples

``` r
if (FALSE) { # \dontrun{
set_pip_folders(main_dir = "~/pip_data", release = "20251211")
} # }
```
