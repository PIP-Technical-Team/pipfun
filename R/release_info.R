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




#' Title
#'
#' @inheritParams get_file_info_from_gh
#'
#' @return data.table with releases table
#' @export
#'
#' @examples
get_releases <- function(owner     = getOption("pipfun.ghowner"),
                         repo      = "pip_info",
                         file_path = "releases.csv",
                         branch    = "releases"
                         ) {


  get_file_from_gh(owner, repo, file_path, branch)

}


#' Get file from Github
#'
#' @inheritParams get_file_info_from_gh
#' @return a file in a data.table class
#' @export
#'
#' @examples
get_file_from_gh <- function(owner= getOption("pipfun.ghowner"),
                             repo,
                             file_path,
                             branch = "main") {


  # Fetch the content metadata using gh, with authentication
  metadata <- get_file_info_from_gh(owner, repo, file_path, branch)

  # Determine the file type
  file_type <- fs::path_ext(file_path)

  # Switch to handle different file types
  data <- switch(file_type,
                 rds = {
                   # Decode base64 content and read RDS
                   rawContent <- base64enc::base64decode(metadata$content)
                   readRDS(rawConnection(rawContent))
                 },
                 json = {
                   # Parse JSON from base64-encoded content
                   json_content <- rawToChar(base64enc::base64decode(metadata$content))
                   jsonlite::fromJSON(json_content)
                 },
                 csv = {
                   # Read CSV directly from base64-decoded content
                   csv_content <- rawToChar(base64enc::base64decode(metadata$content))
                   read.csv(text = csv_content)
                 },
                 fst = {
                   # Download and read fst file; fst requires local file access
                   download_and_read_file(metadata$download_url, "fst")
                 },
                 qs = {
                   # Download and read qs file; qs requires local file access
                   download_and_read_file(metadata$download_url, "qs")
                 },
                 cli::cli_abort("{.field {file_type}} is unsupported file type for downloading and reading")
  )

  return(data)

}



#' Download and read file
#'
#' Helper function to handle file downloads and reading for types like .fst and
#' .qs
#'
#' @param url character: url of file. usually it comes
#'   [get_file_info_from_gh()$download_url]
#' @param type character: file format
#'
#' @return data in data.table format
#' @keywords internal
download_and_read_file <- function(url, type) {
  temp_file <- tempfile()
  download.file(url, temp_file, mode = "wb")
  on.exit(unlink(temp_file))

  if (type == "fst") {

    return(fst::read.fst(temp_file, as.data.table = TRUE))
  } else if (type == "qs") {
    out <- qs::qread(temp_file) |>
      setDT()
    return(out)
  } else {
    cli::cli_abort("{.field {type}} is unsupported file type for downloading and reading")
  }
}


#' Get info of a file in a Github repo
#'
#'
#' @param owner character: owner of repo
#' @param repo character: repository name
#' @param file_path character: file path
#' @param branch character: branch where the file is
#'
#' @return Complete response from GET method of Github API
#' @export
#'
#' @examples
get_file_info_from_gh <- function(owner= getOption("pipfun.ghowner"),
                                  repo,
                                  file_path,
                                  branch) {



  creds = get_github_creds()

  # alternative
  # gh::gh("GET /repos/:owner/:repo/contents/:path",
  #        owner = owner, repo = repo, path = path, ref = ref,
  #        .token = Sys.getenv("GITHUB_PAT"))

  gh::gh(
    "GET /repos/{owner}/{repo}/contents/{file_path}",
    owner     = owner,
    repo      = repo,
    file_path = file_path,
    .params   = list(ref = branch),
    .token = creds$password
  )

}


