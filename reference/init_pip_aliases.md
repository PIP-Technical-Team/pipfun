# Initialize and register stamp aliases for PIP folders

Sets up short, human-friendly aliases for each of the repository folders
returned by
[`set_pip_folders()`](https://pip-technical-team.github.io/pipfun/reference/set_pip_folders.md)
using the `stamp` package. When `include_release = TRUE`,
release-specific aliases will include the release string (so aliases
like `pip_meta_20251211` are created).

## Usage

``` r
init_pip_aliases(
  folder_paths,
  verbose = getOption("pipfun.verbose"),
  include_release = FALSE,
  release = NULL
)
```

## Arguments

- folder_paths:

  named list: output of
  [`set_pip_folders()`](https://pip-technical-team.github.io/pipfun/reference/set_pip_folders.md).

- verbose:

  logical: whether to print alias registration messages.

- include_release:

  logical: when `TRUE`, append `_<release>` to aliases for folders that
  are release-specific.

- release:

  character\|null: release identifier used when
  `include_release = TRUE`. Required in that case.

## Value

Invisibly returns a named character vector of the final alias names. The
vector's names correspond to the keys of `folder_paths`.
