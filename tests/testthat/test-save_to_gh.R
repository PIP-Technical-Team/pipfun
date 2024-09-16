test_that("save_to_gh works correctly", {
  Sys.setenv(GITHUB_PAT = 'code')
  testthat::local_mocked_bindings(gh = function(...) NULL)
  expect_message(save_to_gh(iris, "test"), "File test.csv saved to DEV branch of aux_test in GitHub successfully")
  expect_null(save_to_gh(iris, "test"))
})

# # Load required packages
# library(testthat)
# library(mockery)     # For mocking functions
# library(base64enc)   # For base64 encoding/decoding

# Source your functions (adjust the path as needed)
# source('path/to/your/github_functions.R')

# For demonstration, let's assume your functions are already in the environment

# Sample data frame for testing
df_sample <- data.frame(
  x = 1:5,
  y = letters[1:5],
  stringsAsFactors = FALSE
)

# -------------------------------
# Tests for convert_df_to_base64()
# -------------------------------

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

test_that("save_to_gh works correctly with mocked functions", {
  # Skip on CI/CD environments like GitHub Actions
  testthat::skip_on_ci()

  # Mock functions
  local_mocked_bindings(
    get_github_creds  = function() list(password = "dummy_token")
  )
  with_mocked_bindings(code = {

    result <- save_to_gh(
      df = df_sample,
      repo = "dummy_repo",
      owner = "dummy_owner",
      branch = "main",
      filename = "dummy_file",
      ext = "csv"
    )
    # Expect that the function returns NULL
    expect_null(result)
  },
  gh = function(endpoint, ..., .token) {
    if (grepl("^GET", endpoint)) {
      # Simulate a file not found error (as when the file does not exist)
      stop(structure(list(message = "Not Found (404)", call = NULL),
                     class = c("http_error_404", "error", "condition")))
    } else if (grepl("^PUT", endpoint)) {
      # Simulate a successful file upload
      return(list(content = "dummy_response", sha = "dummy_sha"))
    }
  }, .package = "gh"
  )

  # Expect that the function returns NULL
  expect_null(result)
})
