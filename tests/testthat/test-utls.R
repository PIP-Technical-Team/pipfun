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

# test_that("convert_df_to_base64 works as expected", {
#   expect_equal(convert_df_to_base64(mtcars),
#                "bXBnLGN5bCxkaXNwLGhwLGRyYXQsd3QscXNlYyx2cyxhbSxnZWFyLGNhcmIKMjEsNiwxNjAsMTEwLDMuOSwyLjYyLDE2LjQ2LDAsMSw0LDQKMjEsNiwxNjAsMTEwLDMuOSwyLjg3NSwxNy4wMiwwLDEsNCw0CjIyLjgsNCwxMDgsOTMsMy44NSwyLjMyLDE4LjYxLDEsMSw0LDEKMjEuNCw2LDI1OCwxMTAsMy4wOCwzLjIxNSwxOS40NCwxLDAsMywxCjE4LjcsOCwzNjAsMTc1LDMuMTUsMy40NCwxNy4wMiwwLDAsMywyCjE4LjEsNiwyMjUsMTA1LDIuNzYsMy40NiwyMC4yMiwxLDAsMywxCjE0LjMsOCwzNjAsMjQ1LDMuMjEsMy41NywxNS44NCwwLDAsMyw0CjI0LjQsNCwxNDYuNyw2MiwzLjY5LDMuMTksMjAsMSwwLDQsMgoyMi44LDQsMTQwLjgsOTUsMy45MiwzLjE1LDIyLjksMSwwLDQsMgoxOS4yLDYsMTY3LjYsMTIzLDMuOTIsMy40NCwxOC4zLDEsMCw0LDQKMTcuOCw2LDE2Ny42LDEyMywzLjkyLDMuNDQsMTguOSwxLDAsNCw0CjE2LjQsOCwyNzUuOCwxODAsMy4wNyw0LjA3LDE3LjQsMCwwLDMsMwoxNy4zLDgsMjc1LjgsMTgwLDMuMDcsMy43MywxNy42LDAsMCwzLDMKMTUuMiw4LDI3NS44LDE4MCwzLjA3LDMuNzgsMTgsMCwwLDMsMwoxMC40LDgsNDcyLDIwNSwyLjkzLDUuMjUsMTcuOTgsMCwwLDMsNAoxMC40LDgsNDYwLDIxNSwzLDUuNDI0LDE3LjgyLDAsMCwzLDQKMTQuNyw4LDQ0MCwyMzAsMy4yMyw1LjM0NSwxNy40MiwwLDAsMyw0CjMyLjQsNCw3OC43LDY2LDQuMDgsMi4yLDE5LjQ3LDEsMSw0LDEKMzAuNCw0LDc1LjcsNTIsNC45MywxLjYxNSwxOC41MiwxLDEsNCwyCjMzLjksNCw3MS4xLDY1LDQuMjIsMS44MzUsMTkuOSwxLDEsNCwxCjIxLjUsNCwxMjAuMSw5NywzLjcsMi40NjUsMjAuMDEsMSwwLDMsMQoxNS41LDgsMzE4LDE1MCwyLjc2LDMuNTIsMTYuODcsMCwwLDMsMgoxNS4yLDgsMzA0LDE1MCwzLjE1LDMuNDM1LDE3LjMsMCwwLDMsMgoxMy4zLDgsMzUwLDI0NSwzLjczLDMuODQsMTUuNDEsMCwwLDMsNAoxOS4yLDgsNDAwLDE3NSwzLjA4LDMuODQ1LDE3LjA1LDAsMCwzLDIKMjcuMyw0LDc5LDY2LDQuMDgsMS45MzUsMTguOSwxLDEsNCwxCjI2LDQsMTIwLjMsOTEsNC40MywyLjE0LDE2LjcsMCwxLDUsMgozMC40LDQsOTUuMSwxMTMsMy43NywxLjUxMywxNi45LDEsMSw1LDIKMTUuOCw4LDM1MSwyNjQsNC4yMiwzLjE3LDE0LjUsMCwxLDUsNAoxOS43LDYsMTQ1LDE3NSwzLjYyLDIuNzcsMTUuNSwwLDEsNSw2CjE1LDgsMzAxLDMzNSwzLjU0LDMuNTcsMTQuNiwwLDEsNSw4CjIxLjQsNCwxMjEsMTA5LDQuMTEsMi43OCwxOC42LDEsMSw0LDI="
#   )
# })

test_that("convert_df_to_base64 returns an error if dataframe is not passed", {
  expect_error(convert_df_to_base64("x"))
})

