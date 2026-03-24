# Create output directory name

This function creates the name of the directory that used as vintage
control of the PIP project.

## Usage

``` r
pip_create_vintage(
  vintage = list(),
  DATE = format(Sys.Date(), "%Y%m%d"),
  owner = getOption("pipfun.ghowner"),
  branch = c("DEV", "PROD", "main"),
  tag = match.arg(branch)
)
```

## Arguments

- vintage:

  list or character: If list, all objects should be named and only the
  following names are accepted:
  `c("release", "ppp_year", "ppp_rv", "ppp_av", "identity")`.
  Alternatively, it could a single-object list called "names" like
  `vintage = list(name = "some_name")`. If character, it should be of
  length equal to 1.

- DATE:

  character: date in the form %y%m%d.

- owner:

  character: Github repo owner. Default is `getOption("pipfun.ghowner")`

- branch:

  character: either "DEV" or "PROD". Refers to the branch that will be
  used to update either the development server or production.

- tag:

  character: specific release to be used in the update.

## Value

character in the form "%Y%m%d_YYYY\_##\_##\_SSS"
