
library(fs)
library(fastverse)

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

library(fs)
library(fastverse)
library(dplyr)

compare_directories <- function(old, new) {

  # Get list of .R files in directories and sub-directories of dir1 and dir2
  old_files <- fs::dir_ls(path = old,
                           glob = "*.R",
                           recurse = TRUE)
  new_files <- fs::dir_ls(path = new,
                           glob = "*.R",
                           recurse = TRUE)

  # Iterate over files in both directories - ISSUE
  common_files <- intersect(fs::path_file(old_files),
                            fs::path_file(new_files))

  # Create an empty data frame to store the results
  info_df <- data.frame(
    file_name = character(),
    last_modified_old = character(),
    last_modified_new = character(),
    stringsAsFactors = FALSE
  )

  # Iterate over common files and collect information
  for (file in common_files) {

    file_name <- file

    last_modified_old <- file_info(file) %>%
      pull(modified) %>%
      as.character()

    last_modified_new <- file_info(file_path(file, dir = new_dir)) %>%
      pull(modified) %>%
      as.character()

    results_df <- rbind(results_df, data.frame(
      file_name = file_name,
      last_modified_old = last_modified_old,
      last_modified_new = last_modified_new,
      stringsAsFactors = FALSE
    ))

  }

  return(results_df)
}

## Get time of last modification of a file ####

# Example usage ####

dir1_path = "C:\\WBG\\Packages\\joyn"
dir2_path <- "C:\\WBG\\Packages\\pipster"
dir2_new_path <- "C:\\Users\\wb621604\\OneDrive - WBG\\Desktop\\pipster"

#comparison_results <- compare_directories(old_dir, new_dir)
#print(comparison_results)



