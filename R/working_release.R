#' Loads PIP release into .pipenv
#'
#' This functions sets all the necessary information into the `.pipenv`
#' environment to be used by other PIP packages. It does not create releases.
#'
#' @inheritParams find_release
#' @inheritParams get_pip_releases
#' @inheritParams download_and_read_file
#' @inheritDotParams pip_create_globals -vintage -create_dir
#' @param ppp numeric: PPP year to use.
#'
#' @return invisible table with release information and list object in the
#'   `.pipenv` environment
#' @export
#'
#' @examples
#' \dontrun{
#' # latest PROD release
#' setup_working_release()
#'
#' # error if set up again
#' try(setup_working_release())
#' }
setup_working_release <- function(release  = NULL,
                                 identity = getOption("pipfun.identities"),
                                 force    = FALSE,
                                 owner     = getOption("pipfun.ghowner"),
                                 repo      = "pip_info",
                                 file_path = "releases.csv",
                                 branch    = "releases",
                                 verbose   = getOption("pipfun.verbose"),
                                 ppp       = getOption("pipfun.ppps"),
                                 creds     = NULL,
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
                             verbose   = verbose,
                             creds     = creds)
    } else {
      get_pip_releases(owner     = owner,
                       repo      = repo,
                       file_path = file_path,
                       branch    = branch,
                       verbose   = verbose,
                       creds     = creds) |>
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



#' get working release in PIP functions
#'
#' You can place this function at the beginning of any of your PIP function to
#' work with the working release
#'
#' @param name character: Name of the working release object. default is
#'   "wrk_release" and you should leave it like that
#'
#' @return assign `name` object to `parent.frame()` which is the function it is
#'   being called from
#' @export
#'
#' @examples
#' \dontrun{
#' hello <- function() {
#' get_wrk_release()
#' invisible(wrk_release)
#' }
#' setup_working_release()
#' print(hell())
#' }
get_wrk_release <- function(name = "wrk_release",
                            verbose = TRUE) {
  wrk_release <- get_from_pipenv("working_release")
  if (is.null(wrk_release)) {
    cli::cli_abort(
      c(x = "Working release has not been set up",
        i = "You need to set a working release with {.code pipfun::setup_working_release()}"))
  } else {
    if (verbose) cli::cli_alert_info("Your working release is {.field {wrk_release$release}}")
  }

  # Assign to hello()'s environment
  assign(name, wrk_release, envir = parent.frame())
}
