#' Title
#'
#' @inheritParams load_from_gh
#' @return NULL
#' @export
#'
#' @examples
save_to_gh <- function(measure,
                       owner     = getOption("pipfun.ghowner"),
                       repo      = paste0("aux_", measure),
                       branch    = "DEV",
                       tag       = branch,
                       filename  = measure,
                       ext       = "csv",
                         ...) {

  metapip:::check_github_token()

  gh::gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner   = owner,
    repo    = repo,
    path    = glue::glue("{filename}.{ext}"),
    .params = list(
      branch  = branch,
      message = "updating data",
      #sha     = out$sha,
      content = pipaux:::convert_df_to_base64(mtcars)
    ),
    .token = Sys.getenv('GITHUB_PAT')
  )
}
