#' Create new release for PIP update
#'
#' @inheritParams create_new_brach
#'
#' @return
#' @export
#'
#' @examples
create_new_release <- function(measure     = NULL,
                               owner       = getOption("pipfun.ghowner"),
                               repo        = ifelse(is.null(measure), NA,
                                                    paste0("aux_", measure)) ,
                               new_release = format(Sys.Date(), "%Y%m%d"),
                               identity    = c("PROD", "INT", "TEST"),
                               ref_branch  = "DEV",
                               new_branch  = paste0(new_release, "_", identity[1]),
                               verbose     = getOption("pipfun.verbose")
                               ) {


  # create folder for aux

  # create folder for welfare data



}

#' Get PIP releases
#'
#' @inheritParams get_file_info_from_gh
#'
#' @return data.table with releases table
#' @export
#'
#' @examples
#' get_releases()
get_releases <- function(owner     = getOption("pipfun.ghowner"),
                         repo      = "pip_info",
                         file_path = "releases.csv",
                         branch    = "releases"
                         ) {


  get_file_from_gh(owner, repo, file_path, branch)

}


