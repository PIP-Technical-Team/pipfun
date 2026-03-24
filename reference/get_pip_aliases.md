# Retrieve stamp aliases registered for PIP folders

Returns the alias mapping that was created by
[`init_pip_aliases()`](https://pip-technical-team.github.io/pipfun/reference/init_pip_aliases.md)
and persisted into `.pipenv`. Aliases map short names (e.g. `"aux"`) to
actual folder paths registered with `stamp`.

## Usage

``` r
get_pip_aliases(
  folder = NULL,
  name = "pip_aliases",
  verbose = getOption("pipfun.verbose")
)
```

## Arguments

- folder:

  character\|null: optional folder name (key of aliases) to return. If
  `NULL`, returns the full named character vector of aliases.

- name:

  character: variable name to assign the aliases to in the calling
  environment. Defaults to `"pip_aliases"`.

- verbose:

  logical: if `TRUE`, prints a summary of aliases retrieved.

## Value

Invisibly returns a named character vector of aliases. If `folder` is
provided, returns the single alias string (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
setup_working_release()
get_pip_aliases()             # returns all aliases
get_pip_aliases("aux_data")  # returns alias for aux_data
} # }
```
