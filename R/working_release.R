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
  ppp <- ppp[1]
  if (!ppp %in% getOption("pipfun.ppps")) {
    cli::cli_abort(c("Wrong PPP value",
                     i = "PPP values must be {.or {getOption(\"pipfun.ppps\")}}"))
  }

  pr <- if (is.null(release)) {
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
      find_release(release = release, identity = identity)
  }

   # create globals (no dir creation here)
  gls <- pip_create_globals(create_dir = FALSE,
                            vintage    = list(release = release,
                                              ppp_year = ppp,
                                              identity = identity),
                            verbose = verbose,
                            ...)

  # setup working release info
  wr <- list(release  = pr[, release],
             identity = pr[, identity],
             ppp      = ppp)

  # get directory paths (no pins)
  folder_paths <- set_pip_folders(main_dir = main_dir,
                                 release = pr[, release],
                                 identity = pr[, identity])

  # --- new: initialize aliases per folder and attach them to folder_paths ----
  aliases <- set_pip_aliases(folder_paths)
  folder_paths$aliases <- aliases
  # -------------------------------------------------------------------------

  # save to .pipenv
  rlang::env_poke(.pipenv, "stamp_root", main_dir)
  rlang::env_poke(.pipenv, "wrk_release", wr)
  rlang::env_poke(.pipenv, "gls", gls)
  rlang::env_poke(.pipenv, "folder_paths", folder_paths)  # updated

  if (verbose) {
    cli::cli_alert_info("PIP working release setup to {.field {wr$release}-{wr$identity}}")
    print(folder_paths)
  }

  
  invisible(wr)
}




#' Set PIP directory paths
#'
#' This function creates all necessary directories for a PIP release
#' and returns a named list of paths.
#'
#' @inheritParams setup_working_release
#' @returns Named list of directory paths
#' @export
set_pip_folders <- function(main_dir  = getOption("pipfun.main_dir"),
                            release  = NULL,
                            identity  = getOption("pipfun.identities")) {

  identity <- match.arg(identity)
  if (is.null(release)) {
    release <- get_latest_pip_release() |> _[, release]
  }

  rt <- glue("{release}_{identity}")

  # Aux data
  aux_internal <- c("aux_data", "aux_metadata")
  aux_dir <- fs::path(main_dir, "aux_repository", aux_internal, rt) |>
    fs::dir_create()

  # DLW data
  dlw_dir           <- fs::path(main_dir, "dlw_repository") |>
    fs::dir_create(recurse = TRUE)
  dlw_data_dir      <- fs::path(dlw_dir, "dlw_data") |>
    fs::dir_create(recurse = TRUE)
  dlw_inventory_dir <- fs::path(dlw_dir, "dlw_inventory") |>
    fs::dir_create(recurse = TRUE)
  dlw_metadata_dir  <- fs::path(dlw_dir, "dlw_metadata", rt) |>
    fs::dir_create(recurse = TRUE)

  # PIP data
  pip_dir                  <- fs::path(main_dir, "pip_repository") |>
    fs::dir_create(recurse = TRUE)
  pip_data_dir             <- fs::path(pip_dir, "pip_data", "surveys") |>
    fs::dir_create(recurse = TRUE)
  pip_master_inventory_dir <- fs::path(pip_dir, "pip_data", "master_inventory") |>
    fs::dir_create(recurse = TRUE)
  pip_metadata_dir         <- fs::path(pip_dir, "pip_data", "surveys_metadata", rt) |>
    fs::dir_create(recurse = TRUE)
  pip_inventory_dir        <- fs::path(pip_dir, "pip_inventory", rt) |>
    fs::dir_create(recurse = TRUE)

  # Determine stamp project root (the overall PIP data directory)
  stamp_root <- main_dir

  # Initialize stamp if needed
  if (!fs::dir_exists(fs::path(stamp_root, ".stamp"))) {
    stamp::st_init(root = stamp_root)
  }

  # Return named list of paths
  folder_paths <- list(
    stamp_root    = stamp_root,
    aux_data      = aux_dir[1],
    aux_metadata  = aux_dir[2],
    dlw_data      = dlw_data_dir,
    dlw_inventory = dlw_inventory_dir,
    dlw_metadata  = dlw_metadata_dir,
    pip_data      = pip_data_dir,
    pip_metadata  = pip_metadata_dir,
    pip_inventory = pip_inventory_dir,
    pip_master_inventory = pip_master_inventory_dir
  )

  class(folder_paths) <- "pip_folder_paths"
  folder_paths
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




#' Get PIP folder paths from .pipenv
#'
#' This function retrieves the folder paths that were set up for a PIP release.
#'
#' @param folder character: optional, name of a specific folder to retrieve.
#'   If NULL (default), returns all folder paths.
#' @param name character: name of the object to assign to the calling environment.
#'   Default is `"pip_folders"`.
#' @param verbose logical: whether to print info about the folders retrieved. Default is FALSE
#'
#' @return A named list of folder paths or a single folder path if `folder` is specified.
#' @export
get_pip_folders <- function(folder = NULL,
                            name = "pip_folders",
                            verbose = FALSE) {

  pip_folders <- get_from_pipenv("folder_paths")

  if (is.null(pip_folders)) {
    cli::cli_abort(
      c(x = "PIP folder paths have not been set up",
        i = "You need to set a working release with {.code pipfun::setup_working_release()}"))
  }

  if (verbose) {
    cli::cli_alert_info("Retrieved PIP folder paths")
    print(pip_folders)
  }

  # Assign to parent.frame for developer convenience
  assign(name,
         pip_folders,
         envir = parent.frame())

  if (is.null(folder)) return(invisible(pip_folders))

  if (!(folder %in% names(pip_folders))) {
    cli::cli_abort("{.field {folder}} is not available in {.field pip_folders}")
  }

  invisible(pip_folders[[folder]])
}

#' Initialize stamp aliases for PIP folders
#'
#' Creates an independent .stamp in each folder (if missing) and registers a
#' short, human-friendly alias for each folder.
#'
#' @param folder_paths Named list produced by set_pip_folders()
#' @param alias_map Optional named character vector mapping folder names ->
#'   alias strings. If NULL, a sensible default short mapping is used.
#' @return Named list mapping folder_keys -> alias (invisible)
#' @export
set_pip_aliases <- function(folder_paths,
                            alias_map = NULL) {

  if (is.null(folder_paths) || !is.list(folder_paths)) {
    cli::cli_abort("folder_paths must be the named list returned by set_pip_folders()")
  }

  # default short alias mapping
  default_map <- c(
    aux_data = "aux",
    aux_metadata = "aux_meta",
    dlw_data = "dlw",
    dlw_inventory = "dlw_inv",
    dlw_metadata = "dlw_meta",
    pip_data = "pip",
    pip_metadata = "pip_meta",
    pip_inventory = "pip_inv",
    pip_master_inventory = "pip_master"
  )

  if (is.null(alias_map)) {
    alias_map <- default_map
  }

  # ensure alias_map is named and contains needed keys
  missing_keys <- setdiff(names(default_map), names(alias_map))
  if (length(missing_keys) > 0) {
    # fill missing with defaults
    alias_map[missing_keys] <- default_map[missing_keys]
  }

  # Only operate on keys actually present in folder_paths
  target_keys <- intersect(names(alias_map), names(folder_paths))

   aliases_out <- list()
  for (k in target_keys) {
    root_path <- folder_paths[[k]]
    alias <- as.character(alias_map[[k]])

    if (!fs::dir_exists(root_path)) {
      cli::cli_warn("Target folder {.path {root_path}} for alias {.val {alias}} does not exist; skipping.")
      next
    }

    # create .stamp for this folder only if missing (non-destructive).
    # Let stamp::st_init surface errors on alias conflicts so caller sees them.
    if (!fs::dir_exists(fs::path(root_path, ".stamp"))) {
      stamp::st_init(root = root_path, alias = alias)
    } else {
      # still register the alias in our returned mapping (st_init already run earlier)
      # If an alias with same name was registered for a different folder, st_init would error.
      # If a different alias points to same folder, stamp::st_init() warns; we accept that.
      rlang::warn = NULL
    }

    aliases_out[[k]] <- alias
  }

  invisible(aliases_out)
}
