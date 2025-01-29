# init parameters
rqr_sect          <- c("release", "ppp_year", "ppp_rv", "ppp_av", "identity")
ddtt              <- format(Sys.Date(), "%Y%m%d")
vintage_ch        <- paste0(ddtt, "_2017_01_02_TEST")
vintage_lt        <- data.table::tstrsplit(vintage_ch, "_")
names(vintage_lt) <- rqr_sect

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
  # skip_on_ci()

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

