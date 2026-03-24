# Retrieve folder paths registered for the active PIP working release

Fetches the `folder_paths` object stored in `.pipenv` and assigns it to
the caller's environment (useful for interactive development).
Optionally returns a single folder path when `folder` is supplied.

## Usage

``` r
get_pip_folders(
  folder = NULL,
  name = "pip_folders",
  verbose = getOption("pipfun.verbose")
)
```

## Arguments

- folder:

  character\|null: optional specific folder name to return (e.g.
  `"aux_data"`). If `NULL` (default) the full named list is returned
  invisibly.

- name:

  character: variable name to assign the full folder list to in the
  calling environment. Defaults to `"pip_folders"`.

- verbose:

  logical: if `TRUE`, prints information about the retrieved paths.

## Value

Invisibly returns the named list of folder paths. If `folder` is
provided, returns the single path (invisibly) for that folder.
