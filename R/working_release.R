#' Loads PIP release into .pipenv
#'
#' This functions sets all the necessary information into the `.pipenv`
#' environment to be used by other PIP packages. It does not create releases.
#'
#' @inheritParams find_release
#' @inheritParams get_pip_releases
#' @inheritDotParams pip_create_globals -vintage -create_dir
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
                                 identity = c("PROD", "INT", "TEST"),
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


  if (rlang::env_has(.pipenv, "working_release") && force == FALSE) {

    wr <- rlang::env_get(.pipenv, "working_release")
    cli::cli_abort(c(
      "There is a working release already setup in env {.env .pipenv}.",
      "i" = "{.field Tip}: Use argument {.code force} to setup a different release",
      "x" = "{.field Current working release}: {wr$release}-{wr$identity}"
      ),
      wrap = TRUE)
  }

  pr <-
    if (is.null(release)) {
      get_latest_pip_release(identity = identity,
                             owner     = owner,
                             repo      = repo,
                             file_path = file_path,
                             branch    = branch,
                             verbose   = verbose,
                             force     = force)
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
