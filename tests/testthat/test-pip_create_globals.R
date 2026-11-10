# init parameters
rqr_sect          <- c("release", "ppp_year", "ppp_rv", "ppp_av", "identity")
ddtt              <- format(Sys.Date(), "%Y%m%d")
vintage_ch        <- paste0(ddtt, "_2021_01_02_TEST")
vintage_lt        <- data.table::tstrsplit(vintage_ch, "_")
names(vintage_lt) <- rqr_sect

test_that("noncreating initialization returns an absent cache path", {
  root <- tempfile("pipfun-globals-absent-")
  cache <- fs::path(root, "pip_ingestion_pipeline/pc_data/cache/clean_survey_data")
  expect_false(fs::dir_exists(root))
  expect_false(fs::dir_exists(cache))

  expect_silent(glbs <- pip_create_globals(root_dir = root,
                                           vintage = NULL,
                                           create_dir = FALSE,
                                           verbose = FALSE))

  expect_type(glbs, "list")
  expect_s3_class(glbs$CACHE_SVY_DIR_PC, "fs_path")
  expect_length(glbs$CACHE_SVY_DIR_PC, 1L)
  expect_equal(glbs$CACHE_SVY_DIR_PC, cache)
  expect_null(glbs$cache_ppp)
  expect_false(fs::dir_exists(root))
  expect_false(fs::dir_exists(cache))
})

test_that("noncreating initialization returns an empty cache path", {
  root <- withr::local_tempdir()
  cache <- fs::path(root, "pip_ingestion_pipeline/pc_data/cache/clean_survey_data")
  fs::dir_create(cache)
  before <- fs::dir_ls(root, recurse = TRUE, all = TRUE)

  expect_silent(glbs <- pip_create_globals(root_dir = root,
                                           vintage = NULL,
                                           create_dir = FALSE,
                                           verbose = FALSE))

  expect_equal(glbs$CACHE_SVY_DIR_PC, cache)
  expect_equal(fs::dir_ls(root, recurse = TRUE, all = TRUE), before)
})

test_that("noncreating initialization keeps the latest existing cache", {
  root <- withr::local_tempdir()
  cache <- fs::path(root, "pip_ingestion_pipeline/pc_data/cache/clean_survey_data")
  fs::dir_create(fs::path(cache, c("2017", "2021")))
  before <- fs::dir_ls(root, recurse = TRUE, all = TRUE)

  expect_silent(glbs <- pip_create_globals(root_dir = root,
                                           vintage = NULL,
                                           create_dir = FALSE,
                                           verbose = FALSE))

  expect_equal(glbs$CACHE_SVY_DIR_PC, fs::path(cache, "2021"))
  expect_equal(fs::dir_ls(root, recurse = TRUE, all = TRUE), before)
})

test_that("pip_create_globals works as expected", {
  root <- fs::path_temp("pcg")
  vintage_lt <- c("latest", "prod")
  vintage_ch <- "20250101_1234_1_1_PROD"

  # Test with vintage list
  vd <- pip_create_globals(root_dir = root, create_dir = TRUE, vintage = vintage_lt)
  expect_true(fs::dir_exists(vd$OUT_DIR_PC))
  expect_true(fs::dir_exists(vd$OUT_SVY_DIR_PC))
  #expect_equal(vd$vintage_dir, vintage_ch)

  # Test with vintage character
  vintage_ch        <- paste0(ddtt, "_2017_01_02_TEST")
  vd <- pip_create_globals(root_dir = root, create_dir = TRUE, vintage = vintage_ch)
  expect_equal(vd$vintage_dir, vintage_ch)

  # Test without vintage
  vd <- pip_create_globals(root_dir = root, create_dir = TRUE)
  expect_null(vd$vintage_dir)
})


test_that("not applicable in CI", {
  #skip_on_ci()

  root <- fs::path_temp("pcg2")

  out_dir <-  fs::path_temp("pipfun-cg")

  vd <- pip_create_globals(root_dir = root,
                           create_dir = TRUE,
                           vintage = c("new", "test"),
                           out_dir = out_dir)

  expect_equal(vd$vintage_dir, vintage_ch)
  expect_true(fs::dir_exists(vd$OUT_EST_DIR_PC))

})


test_that("wrong inputs trigger error", {

  expect_error(pip_create_globals(root_dir = root,
                                  create_dir = TRUE,
                                  vintage = c("a", "b", "c")
                                  )
               )

  expect_error(pip_create_globals(root_dir = root,
                                  create_dir = TRUE,
                                  vintage = c("latest", "b")
                                  )
               )
  expect_error(pip_create_globals(root_dir = root,
                                  create_dir = TRUE,
                                  vintage = c("fiejf", "int")
                                  )
               )

})

