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
#' @param main_dir character: directory  path where all PIP data is stored. By
#'   default it is available in `getOption("pipfun.main_dir")`, but it is
#'   basically a combination of `Sys.getenv("PIP_ROOT_DIR")` and
#'   `getOption("pipfun.working_dir")`.
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
                                 identity  = getOption("pipfun.identities"),
                                 force     = FALSE,
                                 owner     = getOption("pipfun.ghowner"),
                                 repo      = "pip_info",
                                 file_path = "releases.csv",
                                 branch    = "releases",
                                 verbose   = getOption("pipfun.verbose"),
                                 ppp       = getOption("pipfun.ppps"),
                                 creds     = NULL,
                                 main_dir  = getOption("pipfun.main_dir"),
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
  # for now. Dirs should be created elsewhere
  gls <- pip_create_globals(create_dir = FALSE,
                            vintage    = list(release = release,
                                              ppp_year = ppp,
                                              identity = identity),
                            verbose = verbose,
                            ...)

  # setup working release
  wr <- list(release  = pr[, release],
             identity = pr[, identity],
             ppp      = ppp)

  boards <- set_pip_boards(main_dir = main_dir,
                           release = pr[, release],
                           identity = pr[, identity])

  rlang::env_poke(.pipenv, "wrk_release", wr)
  rlang::env_poke(.pipenv, "gls", gls)
  rlang::env_poke(.pipenv, "pins_boards", boards)

  if (verbose) {
    cli::cli_alert_info("PIP working release setup to {.field {wr$release}-{wr$identity}}")
    print(boards)
  }

  invisible(wr)
}




#' set pins board
#'
#' set all the directory paths that contain pins boards for pip. It should be
#' used inside [setup_working_release] but it could be used interactively for
#' testing purposes.
#'
#' @inheritParams setup_working_release
#'
#' @returns lists of pins boards
#' @export
#'
#' @examples
#' set_pip_boards()
set_pip_boards <- function(main_dir  = getOption("pipfun.main_dir"),
                           release  = NULL,
                           identity  = getOption("pipfun.identities")) {

  identity <- match.arg(identity)
  if (is.null(release)) {
    release <- get_latest_pip_release() |>
      _[, release]
  }

  rt      <- glue("{release}_{identity}")

  # Aux data ---------
  aux_internal <- c("aux_data", "aux_metadata")
  aux_dir <- fs::path(main_dir, "aux_repository", aux_internal) |>
    fs::path(rt) |>
    fs::dir_create()

  aux_data <- pins::board_folder(aux_dir[1], TRUE)
  aux_metadata <- pins::board_folder(aux_dir[2], TRUE)

  # DLW data ---------
  dlw_dir <- fs::path(main_dir, "dlw_repository") |>
    fs::dir_create(recurse = TRUE)

  dlw_data_dir      <- fs::path(dlw_dir, "dlw_data")
  dlw_inventory_dir <- fs::path(dlw_dir, "dlw_inventory", rt) |>
    fs::dir_create(recurse = TRUE)

  dlw_data      <- pins::board_folder(dlw_data_dir, TRUE)
  dlw_inventory <- pins::board_folder(dlw_inventory_dir, TRUE)

  # PIP data ------
  pip_dir <- fs::path(main_dir, "pip_repository") |>
    fs::dir_create(recurse = TRUE)

  pip_data_dir      <- fs::path(pip_dir, "pip_data", "surveys")
  pip_metadata_dir  <- fs::path(pip_dir, "pip_data", "surveys_metadata", rt) |>
    fs::dir_create(recurse = TRUE)

  pip_inventory_dir <- fs::path(pip_dir, "pip_inventory", rt) |>
    fs::dir_create(recurse = TRUE)

  pip_data      <- pins::board_folder(pip_data_dir, TRUE)
  pip_metadata  <- pins::board_folder(pip_metadata_dir, TRUE)
  pip_inventory <- pins::board_folder(pip_inventory_dir, TRUE)


  boards <- list(aux_data      = aux_data,
                 aux_metadata  = aux_metadata,
                 dlw_data      = dlw_data,
                 dlw_inventory = dlw_inventory,
                 pip_data      = pip_data,
                 pip_metadata  = pip_metadata,
                 pip_inventory = pip_inventory)
  class(boards) <- "pip_boards"
  boards
}

#' get working release in PIP functions
#'
#' You can place this function at the beginning of any of your PIP function to
#' work with the working release
#'
#' @inheritParams setup_working_release
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
#' print(hello())
#' }
get_wrk_release <- function(name = "wrk_release",
                            verbose  = getOption("pipfun.verbose")) {
  wrk_release <- get_from_pipenv("wrk_release")
  if (is.null(wrk_release)) {
    cli::cli_abort(
      c(x = "Working release has not been set up",
        i = "You need to set a working release with {.code pipfun::setup_working_release()}"))
  } else {
    if (verbose) cli::cli_alert_info("Your working release is {.field {wrk_release$release}}")
  }

  assign(name, wrk_release, envir = parent.frame())
}




#' Get PIP pins boards from pipenv environment
#' @param board character: name of the PIP board you want to filter
#' @param name character: Name of the pins boards that you want to assign to the
#'   parent.frame() that call this function. default is "pins_boards" and you
#'   should leave it like that. this is just an argument for developers.
#' @inheritParams setup_working_release
#'
#' @returns list of pins boards
#' @export
#' @rdname get_wrk_release
get_pins_boards <- function(board = NULL,
                            name = "pins_boards",
                           verbose  = getOption("pipfun.verbose")) {
  pins_boards <- get_from_pipenv("pins_boards")
  if (is.null(pins_boards)) {
    cli::cli_abort(
      c(x = "PIP pins boards have not been set up",
        i = "You need to set a working release with {.code pipfun::setup_working_release()}"))
  } else {
    if (verbose) pins_boards
  }

  assign(name, pins_boards, envir = parent.frame())

  if (is.null(board)) return(invisible(pins_boards))

  if (!(board %in% names(pins_boards))) {
    cli::cli_abort("{.field {board}} is not available in {.field pins_board}")
  }

  pins_boards[[board]]

}
