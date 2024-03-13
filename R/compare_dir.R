
library(fs)
library(fastverse)
library(joyn)
library(data.table)
library(DT)

# # Function to compare individual files
# compare_files <- function(file1, file2) {
#   if (!fs::file_exists(file2)) return(c(new = TRUE, old = FALSE))  # New file in dir1
#   if (!fs::file_exists(file1)) return(c(new = FALSE, old = TRUE))   # Old file in dir2
#
#   # Compare creation times
#   time1 <- fs::file_info(file1)$modification_time
#   time2 <- fs::file_info(file2)$modification_time
#
#   if (time1 > time2) return(c(new = TRUE, old = FALSE))  # Newer file in dir1
#   return(c(new = FALSE, old = TRUE))                        # Older file in dir2
# }
#
#
# compare_directories <- function(dir1, dir2, recurse = FALSE) {
#   # Check directory paths
#   stopifnot(exprs = {
#     fs::dir_exists(dir1)
#     fs::dir_exists(dir2)
#   })
#
#
#   # Initialize results data.table
#   results <- data.table(path1 = character(),
#                         path2 = character(),
#                         new = logical(),
#                         old = logical())
#
#   # Get info of files
#   file_list1 <- fs::dir_ls(dir1,
#                            recurse = recurse,
#                            type = "file") |>
#     fs::file_info() |>
#     # File directory without the root.
#     ftransform(wo_root = gsub(dir1, "", path))
#
#   file_list2 <- fs::dir_ls(dir2,
#                            recurse = recurse,
#                            type = "file") |>
#     fs::file_info() |>
#     # File directory without the root.
#     ftransform(wo_root = gsub(dir2, "", path))
#
#
#
#   dt_compare <- joyn::joyn(file_list1,
#                            file_list2,
#                            by = "wo_root",
#                            suffixes = c("_old", "_new"),
#                            match_type = "1:1")
#
#   # compare files shared in both dirs
#   dt_sf <- dt_compare |>
#     fsubset(.joyn == "x & y")
#
#   comparison <- vector("list", length = nrow(dt_sf))
#   for (i in seq_len(nrow(dt_sf))) {
#
#     file_name <- fs::path_file(file_path)
#
#     # Skip special files (e.g., ., ..)
#     if (fs::file_name(file_name) %in% c(".", "..")) next
#
#     # Check if corresponding file exists in dir2
#
#     # Compare files and add results to data.table
#     comparison[i] <- compare_files(file_list$path_old[i], file_list$path_new[i])
#
#   }
#
#   return(results)
# }

# Example usage
#directory1 <- "C:/path/to/directory1"
#directory2 <- "C:/path/to/directory2"
#comparison_results <- compare_directories(directory1, directory2, recurse = TRUE)

##TODO:
# Add new working function to compare and visualize files changes by dates

--------------------------------- << >> ----------------------------------------

  # Comparing directories - version 0 ####

## Store info on last modification_time for common files in new vs old dir ####

# library(fs)
# library(fastverse)
# library(dplyr)
#
# compare_directories <- function(old, new) {
#
#   # Get list of files in directories and sub-directories of dir1 and dir2
#   old_files <- fs::dir_ls(path = old,
#                            #glob = "*.R",
#                            recurse = TRUE)
#   new_files <- fs::dir_ls(path = new,
#                            #glob = "*.R",
#                            recurse = TRUE)
#
#   # Get common files, excluding special files
#   common_files <- intersect(fs::path_file(old_files),
#                             fs::path_file(new_files))
#
#   filetred_common_files <- common_files[!grepl("^\\.\\.$|^\\.$", common_files)]
#
#   # Create an empty data frame to store the results
#   info_df <- data.frame(
#     file_name = character(),
#     last_modified_old = character(),
#     last_modified_new = character(),
#     # new, initialized to logical, then:
#     #  -TRUE if file in new dir modified after that in old dir,
#     #  -FALSE otherwise
#     new = logical()
#   )
#
#   # Iterate over common files and collect information
#   for (file in old_files) {
#
#     file_name <- fs::path_file(old_files)
#     old_file_path <-
#     new_file_path <-
#
#     last_modified_old <- fs::file_info(path = old_file_path)
#     last_modified_new <- fs::file_info(path = new_file_path)
#
#     new_modified <- last_modified_new > last_modified_old
#
#
#     info_df <- rbind(info_df, data.frame(
#       file_name = fs::path_file(file),
#       last_modified_old = last_modified_old,
#       last_modified_new = last_modified_new,
#       new = new
#     ))
#
#   }
#
#   return(info_df)
# }

# Comparing directories by time - version 1 ####

compare_directories <- function(old, new) {

  # Get info on directory 1
  old_dir_info <- directory_info(dir = old)

  # Get info on directory 2
  new_dir_info <- directory_info(dir = new)

  # Combine info for common files only
  jn_info <- joyn::joyn(x = old_dir_info,
                        y = new_dir_info,
                        by = "file_name",
                        keep_common_vars = TRUE,
                        keep = "inner",
                        reportvar = FALSE)


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
  return(list(info = jn_info,
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
    file_name = character(),
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
new <- dir2_path <- "C:\\WBG\\Packages\\pipster"
old <- dir2_new_path <- "C:\\Users\\wb621604\\OneDrive - WBG\\Desktop\\pipster"

comparison_results <- compare_directories(old = old,
                                          new = new)
print(comparison_results)



