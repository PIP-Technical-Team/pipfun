#' Create new release for PIP update
#'
#' @inheritParams create_new_brach
#' @inheritDotParams get_pip_releases
#'
#' @return
#' @export
#'
#' @examples
#'
new_pip_release <-
  function(new_release = format(Sys.Date(), "%Y%m%d"),
           identity    = c("PROD", "INT", "TEST"),
           verbose     = getOption("pipfun.verbose"),
           root_dir    = Sys.getenv("PIP_ROOT_DIR"),
           working_dir = fs::path(root_dir,
                                  getOption("pipfun.working_dir")),
           ...) {


  identity <- match.arg(identity)
  call_args <- all_args()
  x <- check_pip_release_inputs(call_args)
  print(x)

  return("done")
  # create folder for aux
  pr <- get_pip_releases()


  # Create new release
  nt <- data.table(release  = new_release,
                   identity = identity)


  # create folder for welfare data



}

remove_pip_release <- function() {

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
  ls()
}
