#' Loads PIP release into .pipenv
#'
#' This functions sets all the necessary information into the `.pipenv`
#' environment to be used by other PIP packages. It does not create releases.
#'
#' @inheritParams find_release
#' @inheritParams get_pip_releases
#' @inheritDotParams pip_create_globals -vintage -create_dir
#' @param ppp numeric: PPP year to use.
#'
#' @return invisible table with release information and list object in the
#'   `.pipenv` environment
#' @export
#'
#' @examples
#' # latest PROD release
#' setup_working_release()
#'
#' # error if set up again
#' try(setup_working_release())
setup_working_release <- function(release  = NULL,
                                 identity = getOption("pipfun.identities"),
                                 force    = FALSE,
                                 owner     = getOption("pipfun.ghowner"),
                                 repo      = "pip_info",
                                 file_path = "releases.csv",
                                 branch    = "releases",
                                 verbose   = getOption("pipfun.verbose"),
                                 ppp       = getOption("pipfun.ppps"),
                                 ...) {
  identity <- match.arg(identity)
  ppp      <- ppp[1]
  if (!ppp %in% getOption("pipfun.ppps")) {
    cli::cli_abort(c("Wrong PPP value",
                     i = "PPP values must be {.or {getOption(\"pipfun.ppps\")}}"))
  }


  pr <-
    if (is.null(release)) {
      get_latest_pip_release(identity = identity,
                             owner     = owner,
                             repo      = repo,
                             file_path = file_path,
                             branch    = branch,
                             verbose   = verbose)
    } else {
      get_pip_releases(owner     = owner,
                       repo      = repo,
                       file_path = file_path,
                       branch    = branch,
                       verbose   = verbose) |>
        find_release(release = release,
                     identity = identity)
    }

  # create globals
  gls <- pip_create_globals(create_dir = FALSE,  # for now. Dirs should be created elsewhere
                            vintage    = list(release = release,
                                              ppp_year = ppp,
                                              identity = identity),
                            verbose = verbose,
                            ...)

  # setup working release
  wr <- list(release  = pr[, release],
             identity = pr[, identity],
             ppp      = ppp)

  rlang::env_poke(.pipenv, "working_release", wr)
  rlang::env_poke(.pipenv, "gls", gls)

  if (verbose) {
    cli::cli_alert_info("PIP working release setup to {.field {wr$release}-{wr$identity}}")
  }

  invisible(wr)
}
