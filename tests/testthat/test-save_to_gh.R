test_that("save_to_gh works correctly", {
  Sys.setenv(GITHUB_PAT = 'code')
  testthat::local_mocked_bindings(gh = function(...) NULL,
                                  .package = "gh")
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
      return(list(content = list(name = "dummy_name",
                                 url = "https://git.com/dummy_url"),
                  sha = "dummy_sha"))
    }
  }, .package = "gh"
  )

  # Expect that the function returns NULL
  expect_null(result)
})
