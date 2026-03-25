#' Install PIP packages.
#'
#' This is wrapper around remotes::install_github(). By default the owner of the
#' repo is the PIP-Technical-team, which could be changed at any time. This is
#' just a convenient, yet restricted way to install PIP package from GH.
#'
#' @param package character: Name of the package
#' @param owner character: Name of the GH owner. Default is
#'   `getOption("pipfun.ghowner")`
#' @inheritParams remotes::install_github
#' @param ... Other arguments passed on to [remotes::install_github()].
#' @seealso [remotes::install_github()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pipinstall("pipload")
#' }
pipinstall <- function(package,
                       ref     = "HEAD",
                       owner   = getOption("pipfun.ghowner"),
                       ...) {

  #   ____________________________________________________________________________
  #   on.exit                                                                 ####
  on.exit({

  })

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  stopifnot( exprs = {

  }
  )

  #   ____________________________________________________________________________
  #   Early returns                                                           ####
  if (FALSE) {
    return()
  }

  #   ____________________________________________________________________________
  #   Computations                                                            ####

  # include the right ref
  if (grepl("^[[:alpha:]]", ref)) {
    ref <- glue("@{ref}")
  }

  remotes::install_github(glue("{owner}/{package}{ref}"), ...)

}
