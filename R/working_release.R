#' Loads PIP release into .pipenv
#'
#' This functions sets all the necessary information into the `.pipenv`
#' environment to be used by other PIP packages. It does not create releases.
#'
#' @inheritParams find_release
#' @inheritDotParams get_pip_releases
#'
#' @return invisible table with release information and list object in the
#'   `.pipenv` environment
#' @export
#'
#' @examples
#' # latest PROD release
#' setup_working_release()
#'
#' # error y set up again
#' try(setup_working_release())
setup_working_release <- function(release  = NULL,
                                 identity = c("PROD", "INT", "TEST"),
                                 force    = FALSE,
                                 verbose  = TRUE,
                                 ...) {
  identity <- match.arg(identity)

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
      get_latest_pip_release(identity = identity, ...)
    } else {
      get_pip_releases(...) |>
        find_release(release = release,
                     identity = identity)
    }

  # setup working release

  wr <- list(release  = pr[, release],
             identity = pr[, identity])

  rlang::env_poke(.pipenv, "working_release", wr)

  if (verbose) {
    cli::cli_alert_info("PIP working release setup to {.field {wr$release}-{wr$identity}}")
  }

  invisible(wr)
}
