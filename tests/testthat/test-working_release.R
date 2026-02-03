library(withr)

# Helper: create a temp main_dir for testing
create_temp_main_dir <- function() {
  tmp <- tempfile("piptest_")
  dir.create(tmp)
  tmp
}

test_that("set_pip_folders returns pip_folder_paths with expected names", {
  main_dir <- create_temp_main_dir()
  lr <- get_latest_pip_release()
  folders <- set_pip_folders(main_dir = main_dir, release = lr$release, identity = lr$identity)
  expect_s3_class(folders, "pip_folder_paths")
  expect_true(all(c("aux_data", "aux_metadata", "dlw_data", "pip_data", "pip_metadata", "pip_inventory") %in% names(folders)))
})

test_that("set_pip_folders creates directories on disk", {
  main_dir <- create_temp_main_dir()
  lr <- get_latest_pip_release()
  folders <- set_pip_folders(main_dir = main_dir, release = lr$release, identity = lr$identity)
  expect_true(dir.exists(folders$aux_data))
  expect_true(dir.exists(folders$pip_data))
})

# Test setup_working_release and .pipenv

test_that("setup_working_release sets up .pipenv with working_release, gls, folder_paths, and pip_aliases", {
  main_dir <- create_temp_main_dir()
  with_options(list(pipfun.main_dir = main_dir), {
    lr <- get_latest_pip_release()
    wr <- setup_working_release(release = lr$release, identity = lr$identity, verbose = FALSE)
    expect_type(wr, "list")
    expect_true(exists("wrk_release", envir = .pipenv, inherits = FALSE))
    expect_true(exists("gls", envir = .pipenv, inherits = FALSE))
    expect_true(exists("folder_paths", envir = .pipenv, inherits = FALSE))
    expect_true(exists("pip_aliases", envir = .pipenv, inherits = FALSE))
  })
})

# Test get_wrk_release

test_that("get_wrk_release assigns working_release to parent frame", {
  main_dir <- create_temp_main_dir()
  with_options(list(pipfun.main_dir = main_dir), {
    lr <- get_latest_pip_release()
    setup_working_release(release = lr$release, identity = lr$identity)
    get_wrk_release(name = "my_wr")
    expect_true(exists("my_wr"))
  })
})

test_that("get_wrk_release errors if working_release not set", {
  rm(list = ls(envir = .pipenv), envir = .pipenv)
  expect_error(get_wrk_release(verbose = FALSE), "Working release has not been set up")
})

# Test get_pip_folders

test_that("get_pip_folders assigns folder_paths to parent frame", {
  main_dir <- create_temp_main_dir()
  with_options(list(pipfun.main_dir = main_dir), {
    lr <- get_latest_pip_release()
    setup_working_release(release = lr$release, identity = lr$identity, verbose = FALSE)
    env <- new.env()
    with(env, get_pip_folders(name = "my_folders", verbose = FALSE))
    expect_true(exists("my_folders", envir = env))
    expect_s3_class(env$my_folders, "pip_folder_paths")
  })
})

test_that("get_pip_folders errors if folder_paths not set", {
  rm(list = ls(envir = .pipenv), envir = .pipenv)
  expect_error(get_pip_folders(verbose = FALSE), "PIP folder paths have not been set up")
})

# Test get_pip_aliases

test_that("get_pip_aliases assigns aliases to parent frame and returns mapping", {
  main_dir <- create_temp_main_dir()
  with_options(list(pipfun.main_dir = main_dir), {
    lr <- get_latest_pip_release()
    setup_working_release(release = lr$release, identity = lr$identity, verbose = FALSE)

    aliases <- get_from_pipenv("pip_aliases")
    expect_type(aliases, "character")
    expect_true(all(c("aux_data", "pip_data") %in% names(aliases)))

    env <- new.env()
    with(env, get_pip_aliases(name = "my_aliases", verbose = FALSE))
    expect_true(exists("my_aliases", envir = env))
    expect_type(env$my_aliases, "character")
  })
})

test_that("get_pip_aliases can return a single alias", {
  main_dir <- create_temp_main_dir()
  with_options(list(pipfun.main_dir = main_dir), {
    lr <- get_latest_pip_release()
    setup_working_release(release = lr$release, identity = lr$identity, verbose = FALSE)
    a <- get_pip_aliases("aux_data", verbose = FALSE)
    expect_type(a, "character")
    expect_length(a, 1)
  })
})

test_that("get_pip_aliases errors if pip_aliases not set", {
  rm(list = ls(envir = .pipenv), envir = .pipenv)
  expect_error(get_pip_aliases(verbose = FALSE), "PIP aliases have not been set up")
})