library(withr)

# Helper: create a temp main_dir for testing
create_temp_main_dir <- function() {
  tmp <- tempfile("piptest_")
  dir.create(tmp)
  tmp
}

# Test set_pip_boards

test_that("set_pip_boards returns pip_boards S3 object with expected names", {
  main_dir <- create_temp_main_dir()
  boards <- set_pip_boards(main_dir = main_dir, release = "20250801", identity = "TEST")
  expect_s3_class(boards, "pip_boards")
  expect_true(all(c("aux_data", "dlw_data", "dlw_inventory", "pip_data", "pip_metadata", "pip_inventory") %in% names(boards)))
})

test_that("print.pip_boards produces output and returns invisibly", {
  main_dir <- create_temp_main_dir()
  boards <- set_pip_boards(main_dir = main_dir, release = "20250801", identity = "TEST")
  expect_invisible(print(boards))
})

# Test setup_working_release and .pipenv

test_that("setup_working_release sets up .pipenv with working_release, gls, and pins_boards", {
  main_dir <- create_temp_main_dir()
  with_options(list(pipfun.main_dir = main_dir), {
    wr <- setup_working_release(release = "20250801", identity = "TEST", verbose = FALSE)
    expect_type(wr, "list")
    expect_true(exists("working_release", envir = .pipenv, inherits = FALSE))
    expect_true(exists("gls", envir = .pipenv, inherits = FALSE))
    expect_true(exists("pins_boards", envir = .pipenv, inherits = FALSE))
  })
})

# Test get_wrk_release

test_that("get_wrk_release assigns working_release to parent frame", {
  main_dir <- create_temp_main_dir()
  with_options(list(pipfun.main_dir = main_dir), {
    setup_working_release(release = "20250801", identity = "TEST", verbose = FALSE)
    env <- new.env()
    with(env, get_wrk_release(name = "my_wr", verbose = FALSE))
    expect_true(exists("my_wr", envir = env))
    expect_type(env$my_wr, "list")
  })
})

test_that("get_wrk_release errors if working_release not set", {
  rm(list = ls(envir = .pipenv), envir = .pipenv)
  expect_error(get_wrk_release(verbose = FALSE), "Working release has not been set up")
})

# Test get_pins_boards

test_that("get_pins_boards assigns pins_boards to parent frame", {
  main_dir <- create_temp_main_dir()
  with_options(list(pipfun.main_dir = main_dir), {
    setup_working_release(release = "20250801", identity = "TEST", verbose = FALSE)
    env <- new.env()
    with(env, get_pins_boards(name = "my_boards", verbose = FALSE))
    expect_true(exists("my_boards", envir = env))
    expect_s3_class(env$my_boards, "pip_boards")
  })
})

test_that("get_pins_boards errors if pins_boards not set", {
  rm(list = ls(envir = .pipenv), envir = .pipenv)
  expect_error(get_pins_boards(verbose = FALSE), "PIP pins boards have not been set up")
})