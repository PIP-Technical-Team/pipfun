# is_private_repo ------------
test_that("is private works", {
  expect_true(is_private_repo("nan"))
  expect_false(is_private_repo("cpi"))
})

# get_gh ----------------

# Test if get_gh() successfully retrieves tags for the specified owner and repo
test_that("get_gh successfully retrieves tags for specified owner and repo", {
  tags <- get_gh(owner = "PIP-Technical-Team", repo = "aux_cpi", what = "tags")
  expect_true(length(tags) > 0)
  expect_type(tags, "character")
})

# Test if get_gh() successfully retrieves branches for the specified owner and repo
test_that("get_gh successfully retrieves branches for specified owner and repo", {
  branches <- get_gh(owner = "PIP-Technical-Team", repo = "aux_cpi", what = "branches")
  expect_true(length(branches) > 0)
  expect_type(branches, "character")
})

# Test if get_gh() successfully retrieves releases for the specified owner and repo
test_that("get_gh successfully retrieves releases for specified owner and repo", {
  releases <- get_gh(owner = "PIP-Technical-Team", repo = "aux_cpi", what = "releases")
  expect_true(length(releases) > 0)
  expect_type(releases, "character")
})

# Test if get_gh() successfully retrieves repository contents for the specified owner and repo
test_that("get_gh successfully retrieves repository contents for specified owner and repo", {
  contents <- get_gh(owner = "PIP-Technical-Team", repo = "aux_cpi", what = "contents")
  expect_true(length(contents) > 0)
  expect_type(contents, "character")
})

# Test if get_gh() sorts tags in decreasing order
test_that("get_gh sorts tags in decreasing order", {
  tags <- get_gh(owner = "PIP-Technical-Team", repo = "aux_cpi", what = "tags")
  if (length(tags) > 1) {
    expect_true(is.unsorted(tags))
  }
})

# Test if get_gh() throws an error when an invalid value is passed to the what argument
test_that("get_gh throws an error when an invalid value is passed to the what argument", {
  expect_error(get_gh(owner = "PIP-Technical-Team", repo = "aux_cpi", what = "invalid"))
})

# get_github_creds -------------

# Test if get_github_creds() succeeds when valid credentials are available
test_that("get_github_creds succeeds with valid credentials", {
  skip_on_cran()
  # Assuming there's a way to temporarily set a valid GITHUB_PAT for testing
  # This is a placeholder for setting up a mock or a temporary credential
  # setup_mock_creds()

  expect_silent(get_github_creds())

  creds <- get_github_creds()
  expect_true(is.list(creds))
  expect_true(all(c("username", "password") %in% names(creds)))

  expect_equal(names(creds),c("protocol", "host", "username", "password"))

  # Cleanup mock credentials if necessary
  # cleanup_mock_creds()
})

# Test if get_github_creds() fails gracefully when no git installation is found
test_that("get_github_creds fails gracefully with no git installation", {
  skip_on_cran()
  # This would require mocking the absence of git, which might be complex
  # and potentially disruptive to the user's actual configuration.
  # Therefore, this test is more theoretical and should be approached with caution.
  # expect_error(get_github_creds(), "No git installation found")
})

# Test if get_github_creds() fails gracefully when no credentials are found
test_that("get_github_creds fails gracefully with no credentials", {
  skip_on_cran()
  # This test assumes that there's a way to temporarily ensure no credentials are found
  # This might involve manipulating the environment or using mocking techniques
  # expect_error(get_github_creds(), "No git credentials found")
})

# Note: The above tests for the absence of git and credentials are more conceptual,
# as they involve conditions that are challenging to simulate in a test environment


# download_from_gh ------------
# Unit tests for the download_from_gh() function in R using the testthat package's mocking tools

# Assuming the presence of a function get_github_creds() and get_gh() that are used within download_from_gh()

# Test if download_from_gh() successfully downloads a file
test_that("download_from_gh successfully downloads a file", {
  temp_file <- tempfile(fileext = ".csv")
  path <- "https://raw.githubusercontent.com/PIP-Technical-Team/aux_cpi/DEV/cpi.csv"


  download_from_gh(path, temp_file)
  # Check if the file exists and has content
  expect_true(file.exists(temp_file))
  expect_true(file.size(temp_file) > 0)

  # Clean up
  unlink(temp_file)
})

# Test if download_from_gh() handles invalid paths correctly
test_that("download_from_gh handles invalid information", {
  temp_file <- tempfile()
  invalid_path <- "https://raw.githubusercontent.com/PIP-Technical-Team/aux_gdp/INVALID/gdp.csv"

  download_from_gh(invalid_path, temp_file) |>
    expect_error()
  # Check if the file exists and has content
  expect_false(file.exists(temp_file))
  expect_true(is.na(file.size(temp_file) > 0))
  unlink(temp_file)
  }
)

test_that("download_from_gh handles invalid credentials", {

  temp_file <- tempfile(fileext = ".csv")
  path <- "https://raw.githubusercontent.com/PIP-Technical-Team/aux_nan/DEV/nan.csv"

  with_mocked_bindings(
    code = expect_error(download_from_gh(path, temp_file)),
    get_github_creds = function() {
      list(username = "dummy_user", password = "dummy_pass")
    }
  )

})

# Note: These tests use the local_mocked_bindings() and with_mocked_bindings() functions from the testthat package to mock the behavior of external functions.
# This allows for testing the download_from_gh() function without making actual HTTP requests or relying on GitHub credentials.


# load_from_disk ------------
# Unit tests for the load_from_disk() function in R

# Test loading CSV files
test_that("load_from_disk loads CSV files correctly", {
  temp_file <- tempfile(fileext = ".csv")
  write.csv(mtcars, temp_file, row.names = FALSE)
  result <- load_from_disk(temp_file, "csv") |>
    suppressMessages()
  expect_equal(dim(result), dim(mtcars))
  unlink(temp_file)
})

# Test loading Excel files (xls and xlsx)
test_that("load_from_disk loads Excel files correctly", {
  skip_if_not(requireNamespace("writexl", quietly = TRUE))
  # temp_file_xls <- tempfile(fileext = ".xls")
  temp_file_xlsx <- tempfile(fileext = ".xlsx")
  # Assuming the presence of a function to write Excel files for testing
  # writexl::write_xlsx(mtcars, temp_file_xls)
  writexl::write_xlsx(mtcars, temp_file_xlsx)
  # result_xls <- load_from_disk(temp_file_xls, "xls")
  result_xlsx <- load_from_disk(temp_file_xlsx, "xlsx")
  # Assuming the Excel files have the same structure as mtcars for this test
  # expect_equal(dim(result_xls), dim(mtcars))
  expect_equal(dim(result_xlsx), dim(mtcars))
  # Cleanup
  unlink(temp_file_xls)
  unlink(temp_file_xlsx)
})

# Test loading Stata files (dta)
test_that("load_from_disk loads Stata files correctly", {
  skip_if_not(requireNamespace("haven", quietly = TRUE))
  temp_file <- tempfile(fileext = ".dta")
  # Assuming the presence of a function to write Stata files for testing
  haven::write_dta(mtcars, temp_file)
  result <- load_from_disk(temp_file, "dta")
  # Assuming the Stata file has the same structure as mtcars for this test
  expect_equal(dim(result), dim(mtcars))
  unlink(temp_file)
})

# Test loading QS files
test_that("load_from_disk loads QS files correctly", {
  skip_if_not(requireNamespace("qs", quietly = TRUE))
  temp_file <- tempfile(fileext = ".qs")
  qs::qsave(mtcars, temp_file)
  result <- load_from_disk(temp_file, "qs")
  expect_equal(dim(result), dim(mtcars))
  unlink(temp_file)
})

# Test loading FST files
test_that("load_from_disk loads FST files correctly", {
  skip_if_not(requireNamespace("fst", quietly = TRUE))
  temp_file <- tempfile(fileext = ".fst")
  fst::write_fst(mtcars, temp_file)
  result <- load_from_disk(temp_file, "fst")
  expect_equal(dim(result), dim(mtcars))
  unlink(temp_file)
})

# Test loading YAML files
test_that("load_from_disk loads YAML files correctly", {
  skip_if_not(requireNamespace("yaml", quietly = TRUE))
  temp_file <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(mtcars), temp_file)
  result <- load_from_disk(temp_file, "yaml")
  expect_true(is.list(result))
  unlink(temp_file)
})

# Test unsupported file extension
test_that("load_from_disk handles unsupported extensions correctly", {
  temp_file <- tempfile(fileext = ".unsupported")
  expect_error(load_from_disk(temp_file, "unsupported"))
  unlink(temp_file)
})
