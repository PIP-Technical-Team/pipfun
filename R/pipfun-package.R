#' @keywords internal
"_PACKAGE"


#' pipfun: A common place for crossed-packages functions in PIP
#'
#' Description These functions are use in different packages across the PIP project
#'
#' @section pipfun functions:
#' The pipfun functions ...
#'
#' @name pipfun
## usethis namespace: start
#' @import data.table
#' @importFrom lifecycle deprecated
#' @importFrom glue glue
#' @importFrom utils menu
## usethis namespace: end
NULL

# Make sure data.table knows we know we're using it
#' @noRd
.datatable.aware = TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    names = c(
      ".",
      ".I",
      ".N",
      ".SD",
      ".",
      "!!",
      ":=",
      "data_level",
      "ppp_av",
      "ppp_default_by_year",
      "ppp_rv",
      "ppp_year",
      "cpi",
      "aux_ver",
      "n",
      "pc_ver",
      "release",
      "working_dir"
    ),
    package = utils::packageName()
  )
}

NULL
