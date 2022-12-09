#' pipfun: A common place for crossed-packages functions in PIP
#'
#' Description These functions are use in different packages across the PIP project
#'
#' @section pipfun functions:
#' The pipfun functions ...
#'
#' @docType package
#' @name pipfun
#' @import data.table
#' @importFrom glue glue

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
      "ppp_year"
    ),
    package = utils::packageName()
  )
}

NULL
