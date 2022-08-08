

test_that("Saves correctly if object is a list", {

  tdir <- fs::path_temp("pipfun-l")

  lx <- list(x = 1)
  measure <- "ltst"

  saved <-
    pip_sign_save(x = lx,
                measure = measure,
                msrdir = tdir,
                save_dta = TRUE)

  expect_true(saved)


  fs::path(tdir, "_vintage") |>
    fs::dir_exists() |>
    expect_true(label = "Vintage folder is created")



  fs::path(tdir, "_vintage") |>
    fs::dir_exists() |>
    expect_true(label = "Vintage folder is created")



  fs::path(tdir, measure, ext = "qs") |>
    fs::file_exists() |>
    expect_true(label = "qs file is created from list")


  fs::path(tdir, measure, ext = "rds") |>
    fs::file_exists() |>
    expect_true(label = "rds file is created from list")

  fs::path(tdir, measure, ext = "dta") |>
    fs::file_exists() |>
    expect_false(label = "No dta file should be saved")


  fs::path(tdir, glue("{measure}_datasignature.txt")) |>
    fs::file_exists() |>
    expect_true(label = "Data signature created")


  # Since the file has not changed, we should expect false if saved again.
  saved2 <-
    pip_sign_save(x = lx,
                  measure = measure,
                  msrdir = tdir,
                  save_dta = TRUE)

  expect_false(saved2)
  fs::dir_delete(tdir)

}
)

test_that("Saves correctly if object is a dataframe", {

  tdir <- fs::path_temp("pipfun-df")

  lx <- data.frame(x = 1)
  measure <- "dftest"

  saved <-
    pip_sign_save(x = lx,
                measure = measure,
                msrdir = tdir,
                save_dta = TRUE) |>
    suppressWarnings()

  expect_true(saved)


  fs::path(tdir, "_vintage") |>
    fs::dir_exists() |>
    expect_true(label = "Vintage folder is created")



  fs::path(tdir, "_vintage") |>
    fs::dir_exists() |>
    expect_true(label = "Vintage folder is created")



  fs::path(tdir, measure, ext = "qs") |>
    fs::file_exists() |>
    expect_true(label = "qs file is created from list")


  fs::path(tdir, measure, ext = "rds") |>
    fs::file_exists() |>
    expect_false(label = "IF dataframe, rds should not be created")

  fs::path(tdir, measure, ext = "dta") |>
    fs::file_exists() |>
    expect_true(label = "If object is data frame, dta should be saved")

  fs::path(tdir, measure, ext = "fst") |>
    fs::file_exists() |>
    expect_true(label = "if dataframe, fst should be created")


  fs::path(tdir, glue("{measure}_datasignature.txt")) |>
    fs::file_exists() |>
    expect_true(label = "Data signature created")


  # Since the file has not changed, we should expect false if saved again.
  saved2 <-
    pip_sign_save(x = lx,
                  measure = measure,
                  msrdir = tdir,
                  save_dta = TRUE)

  expect_false(saved2)

}
)
