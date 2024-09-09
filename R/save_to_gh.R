#' Save to GitHub
#'
#' @param df A dataframe object
#' @inheritParams load_from_gh
#' @return NULL
#' @importFrom gh gh
#' @export
#'
#' @examples
#' \dontrun{
#' save_to_gh(iris, "gdp")
#' }
save_to_gh <- function(df,
                       measure,
                       owner     = getOption("pipfun.ghowner"),
                       repo      = paste0("aux_", measure),
                       branch    = "DEV",
                       tag       = branch,
                       filename  = measure,
                       ext       = "csv",
                         ...) {

  creds <- get_github_creds()

  # Get existing sha of the file
  out <- gh(
    "GET /repos/{owner}/{repo}/contents/{file_path}",
    owner     = owner,
    repo      = repo,
    file_path = glue::glue("{filename}.{ext}"),
    .params   = list(ref = branch)
  )
  # Update the file
  gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner   = owner,
    repo    = repo,
    path    = glue::glue("{filename}.{ext}"),
    .params = list(
      branch  = branch,
      message = "updating data",
      sha     = out$sha,
      content = convert_df_to_base64(df)
    ),
    .token = creds$password
  )

  cli::cli_alert_success("File {filename}.{ext} saved to {branch} branch of {repo} in GitHub successfully!!")
  return(NULL)
}
