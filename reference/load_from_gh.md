# Load Data from github. This function is inspired from the gh package.

Load Data from github. This function is inspired from the gh package.

## Usage

``` r
load_from_gh(
  measure,
  owner = getOption("pipfun.ghowner"),
  repo = paste0("aux_", measure),
  branch = "DEV",
  tag = branch,
  filename = measure,
  ext = NULL,
  ...
)
```

## Arguments

- measure:

  character: measure to be loaded

- owner:

  character: Github repo owner. Default is `getOption("pipfun.ghowner")`

- repo:

  character: name of the repo

- branch:

  character: either "DEV" or "PROD". Refers to the branch that will be
  used to update either the development server or production.

- tag:

  character: specific release to be used in the update.

- filename:

  character: Name of file name without the ".csv" extension. Default is
  `measure`

- ext:

  character: Extension of `filename`. Default "csv"

- ...:

  parameters to be passed to the loading functions depending of the
  extension used

## Value

data.table
