
# Directory info ####
# Should we keep and improve this function or get rid of it?

directory_info <- function(dir,
                           recurse = TRUE,
                           ...) {

  # List of files -also in sub-directories
  files <- fs::dir_ls(path = dir,
                      type = "file",
                      recurse = recurse,
                      ...)

  # Filtering out special files
  files <- files[!grepl("^\\.\\.$|^\\.$", files)]

  # Get all dir info available in file_info
  info_df <- fs::file_info(files)

  return(info_df)

}

# compare directories - new version ####
compare_directories <- function(old,
                                new,
                                recurse = TRUE,
                                by = "date",
                                ...) {

  # memo: Add checks on arguments
  #     -> should by match specific options, based on what's available in file_info?


  # Get info on directory 1
  old_dir_info <- directory_info(dir     = old,
                                 recurse = recurse) |>
    ftransform(wo_root = gsub(old, "", path))

  # Get info on directory 2
  new_dir_info <- directory_info(dir     = new,
                                 recurse = recurse) |>
    ftransform(wo_root = gsub(new, "", path))

  # Combine info with a full join
  dt_compare <- joyn::joyn(x                = old_dir_info,
                           y                = new_dir_info,
                           by               = "wo_root",
                           keep_common_vars = TRUE,
                           suffixes         = c("_old", "_new"),
                           match_type       = "1:1",
                           reportvar        = ".joyn",
                           verbose          = FALSE)

  # Track files that are in new but not in old directory
  new_only <- dt_compare |>
    fsubset(.joyn == "y", wo_root) |>
    ftransform(file_name = fs::path_file(wo_root),
               wo_root = NULL)

  # Do comparison based on by argument -If by date
  if (by == "date") {

    dt_compare <- dt_compare |>
      fsubset(.joyn == "x & y") |>
      fselect(wo_root, modification_time_old, modification_time_new) |>
      ftransform(file_name = fs::path_file(wo_root),
                 wo_root = NULL,
                 is_new = modification_time_new > modification_time_old)

    # visualization
    table_display <- DT::datatable(dt_compare,
                                   options = list(
                                     pageLength = 10, # number of rows to display per page
                                     columnDefs = list(
                                       list(targets = "is_new",
                                            createdCell = JS(
                                              "function(td, cellData, rowData, row, col) {
                                  if (cellData === true) {
                                    $(td).css({'background-color': 'lightblue'});
                                  } else {
                                    $(td).css({'background-color': 'pink'});
                                  }
                                }"
                                            )
                                       )
                                     )))
    return(list(new_files = new_only, dir_compare = dt_compare, display = table_display))

  }

  else {
    # note: to complete
    return(list(new_files = new_only, dir_compare = dt_compare))

    }

} # close function

# Move a file from old to new dir or viceversa

update_dir <- function(dt_compare,
                       source,
                       destination) {


#TODO

}


#### My example
new <-  "C:/WBG/Packages/pipster"
old <-  "C:/Users/wb621604/OneDrive - WBG/Desktop/pipster"

# OLD ####
#' # Comparing directories by time ####
#' #' Comparing directories' common files by date of last modification
#' #'
#' #' @param old path to directory 1
#' #' @param new path to directory 2
#' #' @return list of info data frame and its visualization
#' #' @keywords internal
#' #' @noRd
#'
#'
#' # Add by argument -by default would be "date"
#' compare_directories_v0 <- function(old,
#'                                 new,
#'                                 by = "date") {
#'
#'
#'   # Get info on directory 1
#'   old_dir_info <- directory_info(dir = old)
#'
#'   # Get info on directory 2
#'   new_dir_info <- directory_info(dir = new) |>
#'     ftransform(wo_root = gsub(new, "", path))
#'
#'   # Combine info for common files only
#'   jn_info <- joyn::joyn(x                = old_dir_info,
#'                         y                = new_dir_info,
#'                         by               = "file_name",
#'                         keep_common_vars = TRUE,
#'                         keep             = "inner",
#'                         reportvar        = FALSE)
#'
#'   # Track files that are in new but not in old directory
#'   # Do it here based on reportvar
#'
#'   # Rename cols and add `new` logical var
#'   jn_info <- jn_info |>
#'     ftransform(last_modified_old = last_modified.x, last_modified_new = last_modified.y) |>
#'     collapse::ftransform(last_modified.x = NULL, last_modified.y = NULL) |>
#'     collapse::ftransform(new = last_modified_new > last_modified_old)
#'
#'   # Create visualization
#'   table_display <- DT::datatable(jn_info,
#'                                   options = list(
#'                                   pageLength = 10, # number of rows to display per page
#'                                   columnDefs = list(
#'                                     list(targets = "new",
#'                                          createdCell = JS(
#'                                            "function(td, cellData, rowData, row, col) {
#'                                   if (cellData === true) {
#'                                     $(td).css({'background-color': 'lightgreen'});
#'                                   } else {
#'                                     $(td).css({'background-color': 'orange'});
#'                                   }
#'                                 }"
#'                                            )
#'                                          )
#'                                     )))
#'
#'   # return info data and table
#'   return(list(new_only_files = new_only,
#'               info           = jn_info,
#'               display        = table_display))
#' }
#'
#'

