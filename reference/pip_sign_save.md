# Save PIP data

Save PIP data with data signature.

## Usage

``` r
pip_sign_save(
  x,
  measure,
  msrdir,
  force = FALSE,
  save_dta = FALSE,
  verbose = getOption("pipfun.verbose")
)
```

## Arguments

- x:

  data.frame Data frame to be signed and saved.

- measure:

  character: Measure to be used. e.g., "cpi" or "ppp".

- msrdir:

  character: Directory where the data and data signature will be saved.

- force:

  logical: If TRUE data will be overwritten.

- save_dta:

  logical: If TRUE a Stata (.dta) version of the dataset is also saved.

- verbose:

  logical: display messages. Default is `getOption("pipfun.verbose")`

## Value

logical
