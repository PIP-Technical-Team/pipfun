# Create global variables for PIP data management

Create global variables for PIP data management

## Usage

``` r
pip_create_globals(
  root_dir = Sys.getenv("PIP_ROOT_DIR"),
  out_dir = root_dir,
  vintage = NULL,
  clean = FALSE,
  verbose = getOption("pipfun.verbose"),
  create_dir = FALSE,
  max_year_country = NULL,
  max_year_lineup = NULL,
  max_year_aggregate = NULL
)
```

## Arguments

- root_dir:

  character: root directory of the PIP data

- out_dir:

  character: Output Directory. Default is `root_dir`

- vintage:

  character: name of output folder. It could be "latest", "new", or any
  other name. if it is "latest" (default), the most recent version
  available in the vintage directory of the form "%Y%m%d" will be used.
  If it is "new", a new folder with a name of the form "%Y%m%d" will be
  created. All the names will be coerced to lower cases

- clean:

  logical: if TRUE it cleans all empty directories that have been
  created by mistake. Default is FALSE.

- verbose:

  logical: display messages. Default is `getOption("pipfun.verbose")`

- create_dir:

  logical: If TRUE creates output directory or any other directory that
  is part of the returned global and that does not exist. Otherwise it
  just returns the directory path **even if** the directory does not
  exist

- max_year_country:

  numeric: Max year for country lineup. Default NULL, which is a
  heuristics that depends on the date this function is executed.

- max_year_lineup:

  numeric: Max year for regional lineup.Default NULL, which is two years
  before the current year

- max_year_aggregate:

  numeric: Max year for regional nowcast.Default NULL, which is the
  current year

## Value

list

## Examples

``` r
if (FALSE) { # \dontrun{
pip_create_globals()
} # }
```
