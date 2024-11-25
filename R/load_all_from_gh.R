#' Get all files from folder in Github
#'
#' @inheritParams get_file_info_from_gh
#' @param folder_path character: folder path
#' @param output_path character: folder path
#'
#' @return file of extension in [path]
#' @export
#'
#' @examples
#' load_all_from_gh(owner     = getOption("pipfun.ghowner"),
#'                  repo      = "pipfaker",
#'                  file_path = "data/20240627_2017_01_02_PROD/_aux",
#'                  branch    = "aux_estimations")
load_all_from_gh <- function(owner= getOption("pipfun.ghowner"),
                                repo,
                                branch = "main",
                                folder_path,
                                output_path) {


  # output_path <- "E:/PovcalNet/01.personal/wb535623/PIP/temp"
  # output_path <- file.path(output_path, "20240627_2017_01_02_PROD","_aux")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Fetch the content metadata using gh, with authentication

  metadata <- get_file_info_from_gh(owner     = owner,
                                    repo      = repo,
                                    branch    = branch,
                                    file_path = folder_path)

  # Create lists of paths

  urls <- lapply(metadata, function(df) df$url)
  output_paths <- lapply(as.list(names(urls)),function(base) path <- fs::path(output_path, base))

  # Download all files

  temp_files <- mapply(download_from_gh, urls, output_paths)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(invisible(temp_files))

}

