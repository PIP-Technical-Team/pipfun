
library(fs)
library(fastverse)
library(joyn)
library(data.table)
library(DT)

# Comparing directories by time ####
#' Comparing directories' common files by date of last modification
#'
#' @param old path to directory 1
#' @param new path to directory 2
#' @return list of info data frame and its visualization
#' @keywords internal
#' @noRd

compare_directories <- function(old, new) {

  # Get info on directory 1
  old_dir_info <- directory_info(dir = old)

  # Get info on directory 2
  new_dir_info <- directory_info(dir = new)

  # Combine info for common files only
  jn_info <- joyn::joyn(x                = old_dir_info,
                        y                = new_dir_info,
                        by               = "file_name",
                        keep_common_vars = TRUE,
                        keep             = "inner",
                        reportvar        = FALSE)

  # Rename cols and add `new` logical var
  jn_info <- jn_info |>
    collapse::ftransform(last_modified_old = last_modified.x, last_modified_new = last_modified.y) |>
    collapse::ftransform(last_modified.x = NULL, last_modified.y = NULL) |>
    collapse::ftransform(new = last_modified_new > last_modified_old)

  # Create visualization
  table_display <- DT::datatable(jn_info,
                                  options = list(
                                  pageLength = 10, # number of rows to display per page
                                  columnDefs = list(
                                    list(targets = "new",
                                         createdCell = JS(
                                           "function(td, cellData, rowData, row, col) {
                                  if (cellData === true) {
                                    $(td).css({'background-color': 'lightgreen'});
                                  } else {
                                    $(td).css({'background-color': 'orange'});
                                  }
                                }"
                                           )
                                         )
                                    )))

  # return info data and table
  return(list(info    = jn_info,
              display = table_display))
}


# Directory info ####
directory_info <- function(dir) {

  # List of files -also in sub-directories
  files <- fs::dir_ls(path = dir,
                      #glob = "*.R",
                      recurse = TRUE)

  # Filtering out special files
  files <- files[!grepl("^\\.\\.$|^\\.$", files)]

  # Create an empty data frame to store the results
  info_df <- data.frame(
    file_name     = character(),
    last_modified = character()
  )

  for (file in files) {

    file_name <- fs::path_file(file)

    last_modified <- fs::file_info(path = file)$modification_time

    info_df <- rbind(info_df, data.frame(
      file_name = file_name,
      last_modified = last_modified
    ))

  }

  return(info_df)

}

# Example usage ####

# dir1_path = "C:\\WBG\\Packages\\joyn"
# new <- dir2_path <- "C:\\WBG\\Packages\\pipster"
# old <- dir2_new_path <- "C:\\Users\\wb621604\\OneDrive - WBG\\Desktop\\pipster"
#
# comparison_results <- compare_directories(old = old,
#                                           new = new)
#
# comparison_results$info
# comparison_results$display



