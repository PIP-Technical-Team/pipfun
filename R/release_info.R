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




get_release_info <- function() {

}
