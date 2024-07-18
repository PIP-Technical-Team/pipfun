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
    write.table(quote = FALSE,
                row.names = FALSE,
                sep = ",") |>
    capture.output() |>
    paste(collapse = "\n") |>
    charToRaw() |>
    base64enc::base64encode()
}
