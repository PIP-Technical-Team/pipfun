# Preliminary operations

# Sample data frame for testing
df_sample <- data.frame(
  x = 1:5,
  y = letters[1:5],
  stringsAsFactors = FALSE
)

repo <- "aux_test"
owner <- getOption("pipfun.ghowner")
creds <- get_github_creds()


# Load packages
library(base64enc)   # For base64 encoding/decoding
library(mockery)

# -------------------------------------------- #
# Test save_to_gh()  ####
# -------------------------------------------- #

## Inputs ####

test_that("save_to_gh aborts if 'gh' package is not installed", {

  if (requireNamespace("gh", quietly = TRUE)) {
    skip("Test skipped because 'gh' is already installed.")
  }

  expect_error(
    save_to_gh(df = df_sample,
               repo = "aux_test",
               filename = "test_save",
               ext = "csv"),
    "Package 'gh' is required. Please install it using install.packages('gh')."
  )
})

test_that("save_to_gh throws an error if metadata is missing 'sha' or 'path'", {

  # Case 1: Metadata without 'sha'
  metadata_no_sha <- list(path = "path/to/file.csv")
  expect_error(
    save_to_gh(df       = df_sample,
               repo     = "aux_test",
               filename = "test_save",
               ext      = "csv",
               metadata = metadata_no_sha)
  )

  # Case 2: Metadata without 'path'
  metadata_no_path <- list(sha = "12345abcde")
  expect_error(
    save_to_gh(df       = df_sample,
               repo     = "aux_test",
               filename = "test_save",
               ext      = "csv",
               metadata = metadata_no_path)
  )

  # Case 3: Metadata with neither 'sha' nor 'path'
  metadata_no_sha_no_path <- list()
  expect_error(
    save_to_gh(df       = df_sample,
               repo     = "aux_test",
               filename = "test_save",
               ext      = "csv",
               metadata = metadata_no_sha_no_path)
  )
})

## Save file correctly, 3 cases:
# 1. new file, new data (data_change is TRUE)
# 2. old file, new data (data_change is TRUE)
# 3. old file, old data (data_change is FALSE)

test_that("save_to_gh saves file correctly", {

  # Case 1.
  res <- save_to_gh(
    df = df_sample,
    repo = "aux_test",
    owner = getOption("pipfun.ghowner"),
    branch = "DEV",                # Replace with the branch you want to test
    filename = "new_data_test",     # Replace with a file name that exists in the repo
    ext = "csv",
    metadata = NULL,
    verbose = TRUE
  )

  res$init |>
    expect_null() # init should be NULL because file did not exist

  res$data_change |>
    expect_equal(TRUE)

  # -- delete new file for to prevent subsequent tests call from failing --- #
  gh::gh(
    "DELETE /repos/{owner}/{repo}/contents/{path}",
    owner = owner,
    repo = repo,
    path = "new_data_test.csv",
    message = "delete file for testing",         # Commit message
    .token = creds$password,
    sha = res$content$sha,
    branch = "DEV"                   # Branch where the file exists
  )


  # Case 2.
  set.seed(Sys.time()) #Ensure randomness across sessions

  res <- save_to_gh(
    df = data.frame(
      id = 1:5,
      value = runif(5, 0, 100),      # Random numeric values between 0 and 100
      category = sample(letters[1:3], 5, replace = TRUE) # Random categories
    ),
    repo = "aux_test",
    owner = getOption("pipfun.ghowner"),
    branch = "DEV",                  # Replace with the branch you want to test
    filename = "test_save",          # Replace with a file name that exists in the repo
    ext = "csv",
    metadata = NULL,
    verbose = TRUE
  )

  res$init |>
    is.null() |>
    expect_false() # init should be available because file existed

  res$data_change |>
    expect_equal(TRUE)


  # Case 3.

  # metadata is available and file exists
  res <- save_to_gh(
    df = data.frame(x = 1:5,
                    y = letters[1:5]),
    repo = "aux_test",
    owner = getOption("pipfun.ghowner"),
    branch = "DEV",                # Replace with the branch you want to test
    filename = "data_test",     # Replace with a file name that exists in the repo
    ext = "csv",
    metadata = NULL,
    verbose = TRUE
  )

  res$init |>
    is.null() |>
    expect_false() # init should not be NULL because file already existed

  res$init$path |>
    expect_equal("data_test.csv")

  res$data_change |>
   expect_equal(FALSE)


})


# # -------------------------------
# # Tests for convert_df_to_base64()
# # -------------------------------
#
test_that("convert_df_to_base64 works correctly for all supported file extensions", {
  # Skip on CI/CD environments like GitHub Actions
  testthat::skip_on_ci()

  # Supported extensions
  extensions <- c("csv", "json", "rds", "qs", "fst", "dta")

  for (ext in extensions) {
    # Test that the function returns a base64-encoded string
    encoded_content <- convert_df_to_base64(df_sample, ext)
    expect_true(is.character(encoded_content))
    expect_true(nchar(encoded_content) > 0)

    # Decode the base64 string
    decoded_content <- base64enc::base64decode(encoded_content)

    # For csv and json, we can check if the decoded content matches the original data frame
    if (ext == "csv") {
      content_string <- rawToChar(decoded_content)
      read_df <- readr::read_csv(content_string, show_col_types = FALSE)
      expect_equal(df_sample, as.data.frame(read_df))

    } else if (ext == "json") {
      content_string <- rawToChar(decoded_content)
      read_df <- jsonlite::fromJSON(content_string)
      expect_equal(df_sample, as.data.frame(read_df))

    } else if (ext == "rds") {
      read_df <- unserialize(decoded_content)
      expect_equal(df_sample, read_df)

    } else if (ext == "qs") {
      read_df <- qs::qdeserialize(decoded_content)
      expect_equal(df_sample, read_df)

    } else if (ext == "fst") {
      # For 'fst', write the decoded content to a temp file and read it back
      temp_file <- tempfile(fileext = ".fst")
      on.exit(unlink(temp_file), add = TRUE)
      writeBin(decoded_content, temp_file)
      read_df <- fst::read_fst(temp_file)
      expect_equal(df_sample, as.data.frame(read_df))

    } else if (ext == "dta") {
      # For 'dta', write the decoded content to a temp file and read it back
      temp_file <- tempfile(fileext = ".dta")
      on.exit(unlink(temp_file), add = TRUE)
      writeBin(decoded_content, temp_file)
      read_df <- haven::read_dta(temp_file)
      expect_equal(df_sample,
                   as.data.frame(read_df),
                   ignore_attr = TRUE)
    }
  }

})


# --------------------------------
# Tests for save_to_gh()
# --------------------------------

# test_that("save_to_gh works correctly with mocked functions", {
#   # Skip on CI/CD environments like GitHub Actions
#   testthat::skip_on_ci()
#
#   # Mock functions
#   local_mocked_bindings(
#     get_github_creds  = function() list(password = "dummy_token")
#   )
#   with_mocked_bindings(code = {
#
#     result <- save_to_gh(
#       df = df_sample,
#       repo = "dummy_repo",
#       owner = "dummy_owner",
#       branch = "main",
#       filename = "dummy_file",
#       ext = "csv"
#     )
#     # Expect that the function returns NULL
#     expect_null(result)
#   },
#   gh = function(endpoint, ..., .token) {
#     if (grepl("^GET", endpoint)) {
#       # Simulate a file not found error (as when the file does not exist)
#       stop(structure(list(message = "Not Found (404)", call = NULL),
#                      class = c("http_error_404", "error", "condition")))
#     } else if (grepl("^PUT", endpoint)) {
#       # Simulate a successful file upload
#       return(list(content = "dummy_response", sha = "dummy_sha"))
#     }
#   }, .package = "gh"
#   )
#
#   # Expect that the function returns NULL
#   expect_null(result)
# })
