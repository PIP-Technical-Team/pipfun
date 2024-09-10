#' convert a dataframe into base64 object. This is mostly used to write data to GitHub
#'
#' @param df Dataframe
#'
#' @return base64 string of the dataframe passed
#' @export
#'
#' @examples
#' \dontrun{
#' convert_df_to_base64(mtcars)
#' }
convert_df_to_base64 <- function(df) {
  if(!is.data.frame(df)) {
    cli::cli_abort("df is not a dataframe")
  }
  df |>
    utils::write.table(quote = FALSE,
                row.names = FALSE,
                sep = ",") |>
    utils::capture.output() |>
    paste(collapse = "\n") |>
    charToRaw() |>
    base64enc::base64encode()
}





#' Get all arguments of calling function
#'
#' Caling function is [parent.frame]
#'
#' @return arguments of calling function as a list
#' @export
#'
#' @examples
#' foo <- function(x, y = 2, ...) {
#'   all_args()
#' }
#' foo(x = 1, y = 3)
#' foo(x = 1, z = 123)
#' foo(x = 1, z = "valid")
#' foo(x = 1, a = TRUE)
all_args <- function() {
  # Capture the function call with all arguments
  call_args <- as.list(parent.frame())

  # Add ... arguments, if they exist
  dots <- evalq(list(...), envir = parent.frame())
  call_args <- c(call_args, dots)

  return(call_args)
}

# all_args <- function() {
#   # Capture the full function call, with defaults evaluated
#   frms <- formals(sys.function(sys.parent(n = 1)))
#   frms <- names(frms)
#
#   if ("..." %in% frms) {
#     call_args <- c(as.list(parent.frame()),
#                    evalq(list(...), envir = parent.frame()))
#   } else {
#     call_args <- as.list(parent.frame())
#   }
#
#   return(call_args)
# }
#





#' get most recent version of PPP
#'
#' @return data frame with all PPP years and versions available
#' @export
#'
#' @examples
#' get_ppp_versions()
get_ppp_versions <- function() {

  get_file_from_gh(repo   = "aux_ppp",
                   branch = "DEV_v2",
                   file_path = "ppp_vintage.csv")

}

#' @rdname get_ppp_versions
#' @inheritParams new_pip_release
#' @return data frame with most recent versions for [ppps] selected
#' @export
get_latest_ppp_versions <- function(ppps = getOption("pipfun.ppps")) {

  pppm <- get_ppp_versions()

  pppmf <- pppm[ppp_year %in% ppps]
  if (nrow(pppmf) == 0)
    cli::cli_abort(c("{as.character(ppps)} {?is/are} not PPP
                       year{?s} available",
                     i = "Years available: {unique(pppm$ppp_year)}"))

  pppmf <- pppmf[,
                 .SD[which.max(ppp_av)],
                 by = .(ppp_year, ppp_rv)
  ][,
    .SD[which.max(ppp_rv)],
    by = ppp_year
  ]
  pppmf
}
