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
#' gdp <- data.frame(a = 1:10, b = letters[1:10])
#' save_to_gh(gdp, repo = "aux_gdp", filename = "gdp")
#' }
save_to_gh <- function(df,
                       repo,
                       owner     = getOption("pipfun.ghowner"),
                       branch    = "DEV",
                       filename  = repo,
                       ext       = "csv",
                       ...) {

  creds <- get_github_creds()  # Assumes you have a function to get GitHub credentials

  # Construct the file path
  file_path <- glue::glue("{filename}.{ext}")

  # Try to get existing SHA of the file (if it exists)
  out <- tryCatch({
    gh::gh(
      "GET /repos/{owner}/{repo}/contents/{file_path}",
      owner     = owner,
      repo      = repo,
      file_path = file_path,
      .params   = list(ref = branch),
      .token    = creds$password
    )
  }, error = function(e) {
    if (grepl("404", e$message)) {
      NULL  # File does not exist; will create a new file
    } else {
      cli::cli_abort(e)
    }
  })

  # Convert data frame to base64-encoded content based on the file extension
  content <- convert_df_to_base64(df, ext)

  # Prepare parameters for the GitHub API request
  params <- list(
    branch  = branch,
    message = paste("Updating data via R script on", Sys.time()),
    content = content
  )

  # Include 'sha' parameter if the file already exists (for updating)
  if (!is.null(out)) {
    params$sha <- out$sha
  }

  # Upload the file to GitHub
  gh::gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner   = owner,
    repo    = repo,
    path    = file_path,
    .params = params,
    .token  = creds$password
  )

  cli::cli_alert_success("File {filename}.{ext} saved to
                         {branch} branch of {repo} in GitHub successfully!")
  return(NULL)
}


# Helper function to convert data frame to base64-encoded content based on file extension
convert_df_to_base64 <- function(df, ext) {
  ext <- tolower(ext)


  if (ext == "csv") {

    content <- readr::format_csv(df)
    encoded <- base64enc::base64encode(charToRaw(content))
    return(encoded)

  } else if (ext == "json") {
    content <- jsonlite::toJSON(df, pretty = TRUE, auto_unbox = TRUE)
    encoded <- base64enc::base64encode(charToRaw(content))
    return(encoded)

  } else if (ext == "rds") {
    raw_content <- serialize(df, NULL)
    encoded <- base64enc::base64encode(raw_content)
    return(encoded)

  } else if (ext == "qs") {
    raw_content <- qs::qserialize(df)
    encoded <- base64enc::base64encode(raw_content)
    return(encoded)

  } else if (ext == "fst") {
    temp_file <- tempfile(fileext = ".fst")
    on.exit(unlink(temp_file), add = TRUE)
    fst::write_fst(df, temp_file)
    raw_content <- readBin(temp_file, what = "raw", n = file.info(temp_file)$size)
    encoded <- base64enc::base64encode(raw_content)
    return(encoded)

  } else if (ext == "dta") {
    temp_file <- tempfile(fileext = ".dta")
    on.exit(unlink(temp_file), add = TRUE)
    haven::write_dta(df, temp_file)
    raw_content <- readBin(temp_file, what = "raw", n = file.info(temp_file)$size)
    encoded <- base64enc::base64encode(raw_content)
    return(encoded)

  } else {
    cli::cli_abort("Unsupported file extension: {.ext {ext}}")
  }
}
