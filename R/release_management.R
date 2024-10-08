#' Create new release for PIP update
#'
#' @inheritParams create_new_brach
#' @inheritDotParams get_pip_releases
#' @param ppp numeric: vector of PPP years.
#'
#' @return invisible TRUE if everything went fine
#' @export
#'
#' @examples
#' \dontrun{
#' new_pip_release()
#' }
new_pip_release <-
  function(new_release = format(Sys.Date(), "%Y%m%d"),
           identity    = c("PROD", "INT", "TEST"),
           verbose     = getOption("pipfun.verbose"),
           root_dir    = Sys.getenv("PIP_ROOT_DIR"),
           working_dir = fs::path(root_dir,
                                  getOption("pipfun.working_dir")),
           ppps        = getOption("pipfun.ppps"),
           ...) {

  # defenses ----------
  identity <- match.arg(identity)
  call_args <- all_args()
  check_pip_release_inputs(call_args)


  # add new release to pool --------
  ## get current releases ---------
  pr <- get_pip_releases(...)
  mt <- attr(pr, "metadata") # get metadata from GH


  # Create new release ----
  nt <- data.table(release  = new_release,
                   identity = identity)

  dt <- rbindlist(list(pr, nt),
                  fill = TRUE,
                  use.names = TRUE) |>
    unique()

  dt[, n := .I]

  # get PPP metadata
  ppp <- get_latest_ppp_versions(ppps = ppps)
  ppp[, n := .I]


  # create folder for welfare data ----

  df <-
    dt[
    # expand to match ppp
    rep(1:.N, nrow(ppp))
    # merge
     ][ppp, on = "n"]

  df[,
     `:=`(
         ppp_rv = paste0("0", ppp_rv),
         ppp_av = paste0("0", ppp_av)
       )][,
     aux_ver := paste(release, identity, sep = "_")
     ][,
       pc_ver := paste(release, ppp_year,
                         ppp_rv, ppp_av,
                         identity, sep = "_")]


  aux_versions <- df[,unique(aux_ver)]
  pc_versions  <- df[,unique(pc_ver)]


  aux_dir <- create_aux_dir(root_dir     = root_dir,
                            aux_versions = aux_versions)

  pc_dir <- create_pc_dir(root_dir     = root_dir,
                          pc_versions  = pc_versions)


  # Create repo--------

  # Returning list ---------
  ret_obj_names  <- c("aux_dir", "pc_dir")


  lreturn <- vector("list", length = length(ret_obj_names))
  for (i in seq_along(ret_obj_names)) {
    lreturn[[i]] <- get(ret_obj_names[i])
  }

  names(lreturn) <- ret_obj_names
  return(lreturn)

}


#' Create auxiliary directories for new release
#'
#' @param aux_versions character: name of auxiliary folders. they must come in
#'   the form "%Y%m%d_`identify`", where `identify` stands for [c("PROD", "INT",
#'   "TEST")]
#' @inheritParams new_pip_release
#'
#' @rdname create_dir
#' @keywords internal
create_aux_dir <- function(root_dir    = Sys.getenv("PIP_ROOT_DIR"),
                           aux_versions,
                           working_dir = fs::path(root_dir,
                                                  getOption("pipfun.working_dir"))
                           ) {

  wdir    <- fs::path(working_dir, "aux_data")
  newdirs <- create_dir(wdir, ndirs = aux_versions)

  return(newdirs)

}

#' Create poverty calculator directories for new release
#'
#' @param pc_versions character: name of auxiliary folders. they must come in
#'   the form "%Y%m%d_YYYY_MM_AA_`identify`", where `YYYY` stands for the PPP
#'   year, `MM` stands for the master version of the PPPs,  `AA` refers to the
#'   adaptation  version of the pppp`, and where `identify` stands for
#'   [c("PROD", "INT", "TEST")]
#' @inheritParams new_pip_release
#'
#' @rdname create_dir
#' @keywords internal
create_pc_dir <- function(root_dir    = Sys.getenv("PIP_ROOT_DIR"),
                          pc_versions,
                          working_dir = fs::path(root_dir,
                                                 getOption("pipfun.working_dir"))
                          ) {

  wdir    <- fs::path(working_dir, "pc_data")
  newdirs <- create_dir(wdir, ndirs = pc_versions)

  return(newdirs)

}


#' Create directories in folder
#'
#' @param wdir chracter: working directory path
#' @param ndirs chracter: new directories name that will be created inside
#'   `wdir`
#'
#' @return logical vector. the names of the elements correspond to the directory
#'   paths
#' @export
create_dir <- function(wdir, ndirs,
                       verbose = getOption("pipfun.verbose")) {
  dir_ex <- fs::path(wdir, ndirs) |>
    fs::dir_exists()

  if (all(dir_ex == TRUE)) {
    if (verbose)
      cli::cli_alert_info("All versions already exist. No folder will be created")
    return(invisible(dir_ex))
  }

  ndirs <- dir_ex[dir_ex == FALSE]
  ndirs_ex <- names(ndirs) |>
    fs::dir_create() |>
    fs::dir_exists()

  return(ndirs_ex)
}


remove_pip_release <- function() {

}

remove_aux_dir <- function() {

}

remove_pc_dir <- function() {

}

new_aux_release <- function(measure     = NULL,
                            new_release = format(Sys.Date(), "%Y%m%d"),
                            ref_branch  = "DEV",
                            new_branch  = paste(new_release,
                                                identity[1],
                                                sep = "_"),
                            verbose     = getOption("pipfun.verbose")
                            ) {

}


#' Get PIP releases
#'
#' All the releases available in PIP in any of the servers.
#'
#' @inheritParams get_file_info_from_gh
#' @param force logical: whether to load releases from Github even if they
#'   already available in env .pipenv
#' @param verbose logical: whether to display additional information
#'
#' @return data.table with releases table
#' @export
#'
#' @examples
#' get_pip_releases()
get_pip_releases <- function(owner     = getOption("pipfun.ghowner"),
                             repo      = "pip_info",
                             file_path = "releases.csv",
                             branch    = "releases",
                             verbose   = getOption("pipfun.verbose"),
                             force     = FALSE) {


  # Check if releases available in .pipenv
  if (force == FALSE) {
    if (rlang::env_has(.pipenv, "releases")) {
      if (verbose) {
        cli::cli_alert("{.field releases} is already available in env
                        {.code .pipenv}. Use option {.code force} to
                        load them again from gh",
                        wrap = TRUE)
      }
      return(rlang::env_get(.pipenv, "releases"))
    }
  }

  pr <- get_file_from_gh(owner = owner,
                         repo = repo,
                         branch =  branch,
                         file_path = file_path)

  rlang::env_poke(.pipenv, "releases", pr)
  pr

}



#' Get latest PIP release
#'
#' latest PIP release per identity
#'
#' @param identity character: one of "PROD", "INT", or "TEST"
#' @inheritDotParams get_pip_releases
#'
#' @return data.table with most recent release
#' @export
#'
#' @examples
#' get_latest_pip_release()
get_latest_pip_release <- function(identity = c("PROD", "INT", "TEST"),
                                   ...) {

  iden <- match.arg(identity)

    df <- get_pip_releases(...)

    # Filter by identity and get max
    df <- df[identity == iden
             ][,
               .SD[which.max(release)]]
    # return
    df
}




#' check arguments of release functions
#'
#' @param call_args arguments from release function in form of list. they should
#'   be created using [all_args]
#'
#' @return invisible TRUE if everything goes well
#' @keywords internal
check_pip_release_inputs <- function(call_args) {
  list2env(call_args, envir = environment())

  if (exists("new_release")){
    if (!grepl("[0-9]{8}", new_release))
      cli::cli_abort("{.arg new_release} must be a numeric chracter,
                     representing a date in the form {.field \"%Y%m%d\"}.
                     You provided {.strong {new_release}}")
  }

  if (exists("working_dir")){
    if (!fs::dir_exists(working_dir))
      cli::cli_abort("{.file {working_dir}} does not exist. Please check")
  }



}


# new_release = format(Sys.Date(), "%Y%m%d"),
# identity    = c("PROD", "INT", "TEST"),
# verbose     = getOption("pipfun.verbose"),
# root_dir    = Sys.getenv("PIP_ROOT_DIR"),
# working_dir = fs::path(root_dir,
#                        getOption("pipfun.working_dir"))

