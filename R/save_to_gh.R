#' Title
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

  # Get existing sha of the file (NULL if file doesn't exist yet)
  out <- tryCatch(
    gh(
      "GET /repos/{owner}/{repo}/contents/{file_path}",
      owner     = owner,
      repo      = repo,
      file_path = glue::glue("{filename}.{ext}"),
      .params   = list(ref = branch)
    ),
    http_error_404 = function(e) NULL
  )

  # Build PUT params; sha is only required when updating an existing file
  put_params <- list(
    branch  = branch,
    message = if (is.null(out)) "adding data" else "updating data",
    content = convert_df_to_base64(df)
  )
  if (!is.null(out)) {
    put_params$sha <- out$sha
  }

  # Create or update the file
  gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner   = owner,
    repo    = repo,
    path    = glue::glue("{filename}.{ext}"),
    .params = put_params,
    .token  = creds$password
  )

  cli::cli_alert_success("File {filename}.{ext} saved to {branch} branch of {repo} in GitHub successfully!!")
  return(NULL)
}
