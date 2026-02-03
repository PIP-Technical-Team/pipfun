library(testthat)
library(mockery)
library(base64enc)

# Sample data frame for testing
df_sample <- data.frame(
  x = 1:5,
  y = letters[1:5],
  stringsAsFactors = FALSE
)

# Mock GitHub responses
fake_gh <- function(...){
  list(
    content = list(sha = "fake_sha", path = "fake_path.csv"),
    commit  = list(sha = "commit_sha")
  )
}

test_that("save_to_gh throws error if metadata missing 'sha' or 'path'", {
  stub(save_to_gh, 'gh::gh', fake_gh)

  metadata_no_sha <- list(path = "path/to/file.csv")
  metadata_no_path <- list(sha = "12345abcde")
  metadata_empty <- list()

  expect_error(save_to_gh(df = df_sample, repo = "aux_test", filename = "test", ext = "csv", metadata = metadata_no_sha))
  expect_error(save_to_gh(df = df_sample, repo = "aux_test", filename = "test", ext = "csv", metadata = metadata_no_path))
  expect_error(save_to_gh(df = df_sample, repo = "aux_test", filename = "test", ext = "csv", metadata = metadata_empty))
})

test_that("convert_df_to_base64 works correctly", {
  skip_on_ci()

  extensions <- c("csv", "json", "rds", "qs", "fst", "dta")

  for (ext in extensions) {
    encoded <- convert_df_to_base64(df_sample, ext)
    expect_true(is.character(encoded))
    expect_true(nchar(encoded) > 0)

    decoded <- base64enc::base64decode(encoded)

    if (ext == "csv") {
      read_df <- readr::read_csv(rawToChar(decoded), show_col_types = FALSE)
      expect_equal(df_sample, as.data.frame(read_df))
    } else if (ext == "json") {
      read_df <- jsonlite::fromJSON(rawToChar(decoded))
      expect_equal(df_sample, as.data.frame(read_df))
    } else if (ext == "rds") {
      expect_equal(df_sample, unserialize(decoded))
    } else if (ext == "qs") {
      expect_equal(df_sample, qs::qdeserialize(decoded))
    } else if (ext == "fst") {
      tmp <- tempfile(fileext = ".fst")
      on.exit(unlink(tmp), add = TRUE)
      writeBin(decoded, tmp)
      expect_equal(df_sample, as.data.frame(fst::read_fst(tmp)))
    } else if (ext == "dta") {
      tmp <- tempfile(fileext = ".dta")
      on.exit(unlink(tmp), add = TRUE)
      writeBin(decoded, tmp)
      expect_equal(df_sample, as.data.frame(haven::read_dta(tmp)), ignore_attr = TRUE)
    }
  }
})
