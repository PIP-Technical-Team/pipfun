library(testthat)
library(withr)

# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------
create_temp_main_dir <- function() {
  tmp <- tempfile("piptest_")
  dir.create(tmp, recursive = TRUE)
  tmp
}

clear_pipenv <- function() {
  if (exists(".pipenv", envir = globalenv())) {
    rm(list = ls(envir = .pipenv), envir = .pipenv)
  } else {
    return(NULL)
  }
}

restore_pipenv <- function() {
  # Re-run setup_working_release for the shared_main_dir
  setup_working_release(
    release = lr_shared$release,
    identity = lr_shared$identity,
    verbose  = FALSE,
    main_dir = shared_main_dir,
    alias_include_release = TRUE
  )
}

# ------------------------------------------------------------------
# Single shared working release for the whole test file
# (prevents re-registering the same short aliases for different folders
#  across multiple temp dirs in the same R session)
# ------------------------------------------------------------------
shared_main_dir <- create_temp_main_dir()
lr_shared <- get_latest_pip_release()

# --- run setup once for the file (replace local_test_setup) ---
clear_pipenv()
tryCatch(
  {
    setup_working_release(
      release  = lr_shared$release,
      identity = lr_shared$identity,
      verbose  = FALSE,
      main_dir = shared_main_dir,
      alias_include_release = TRUE
    )
  },
  error = function(e) {
    if (grepl("already registered for a different folder", conditionMessage(e))) {
      testthat::skip("Existing global stamp aliases detected; skipping working_release tests")
    }
    stop(e)
  }
)

# ------------------------------------------------------------------
# set_pip_folders (pure function; safe to test)
# ------------------------------------------------------------------
test_that("set_pip_folders returns expected structure and creates directories", {
  folders <- set_pip_folders(
    main_dir = shared_main_dir,
    release  = lr_shared$release,
    identity = lr_shared$identity
  )

  expect_s3_class(folders, "pip_folder_paths")

  expect_true(all(c(
    "stamp_root",
    "aux_data", "aux_metadata",
    "dlw_data", "dlw_inventory", "dlw_metadata",
    "pip_data", "pip_metadata",
    "pip_inventory", "pip_master_inventory"
  ) %in% names(folders)))

  expect_true(dir.exists(folders$aux_data))
  expect_true(dir.exists(folders$pip_data))
  expect_true(dir.exists(folders$pip_inventory))
})

# ------------------------------------------------------------------
# setup_working_release (state validation only)
# ------------------------------------------------------------------
test_that("setup_working_release populated .pipenv with expected entries", {
  expect_true(exists("wrk_release",  envir = .pipenv, inherits = FALSE))
  expect_true(exists("gls",          envir = .pipenv, inherits = FALSE))
  expect_true(exists("folder_paths", envir = .pipenv, inherits = FALSE))
  expect_true(exists("pip_aliases",  envir = .pipenv, inherits = FALSE))

  folders <- get_from_pipenv("folder_paths")
  aliases <- get_from_pipenv("pip_aliases")

  expect_s3_class(folders, "pip_folder_paths")
  expect_type(aliases, "character")
  expect_true(length(aliases) > 0)
  # alias names correspond to folder keys
  expect_true(all(names(aliases) %in% names(folders)))
})

# ------------------------------------------------------------------
# get_wrk_release
# ------------------------------------------------------------------


test_that("get_wrk_release errors if working release not set", {

  old_env <- as.list(.pipenv)
  clear_pipenv()

  withr::defer({
    list2env(old_env, envir = .pipenv)
  })

  expect_error(
    get_wrk_release(verbose = FALSE),
    "Working release"
  )
})

# ------------------------------------------------------------------
# get_pip_folders
# ------------------------------------------------------------------
test_that("get_pip_folders errors if folder_paths not set", {

  old_env <- as.list(.pipenv)
  clear_pipenv()

  withr::defer({
    list2env(old_env, envir = .pipenv)
  })

  expect_error(
    get_pip_folders(verbose = FALSE),
    "folder paths"
  )
})

# ------------------------------------------------------------------
# get_pip_aliases
# ------------------------------------------------------------------

test_that("get_pip_aliases errors if pip_aliases not set", {

  old_env <- as.list(.pipenv)
  clear_pipenv()

  withr::defer({
    list2env(old_env, envir = .pipenv)
  })

  expect_error(
    get_pip_aliases(verbose = FALSE),
    "aliases"
  )
})