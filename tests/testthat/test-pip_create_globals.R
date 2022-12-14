# init parameters
rqr_sect          <- c("release", "ppp_year", "ppp_rv", "ppp_av", "identity")
ddtt              <- format(Sys.Date(), "%Y%m%d")
vintage_ch        <- paste0(ddtt, "_2017_01_02_TEST")
vintage_lt        <- data.table::tstrsplit(vintage_ch, "_")
names(vintage_lt) <- rqr_sect

test_that("working as expected", {
  # convert list

  root <- fs::path_temp("pcg")

  vd <- pip_create_globals(root_dir = root,
                           create_dir = TRUE,
                           vintage = vintage_lt)

  expect_equal(vd$vintage_dir, vintage_ch)

  # convert character vector
  expect_equal(pip_create_globals(root_dir = root,
                                  create_dir = TRUE,
                                  vintage = vintage_ch)$vintage_dir, vintage_ch)

  # works with data frames
  vintage_DT <- data.table::as.data.table(vintage_lt)
  vintage_DF <- as.data.frame(vintage_lt)

  expect_equal(pip_create_globals(root_dir = root,
                                  create_dir = TRUE,
                                  vintage = vintage_DT)$vintage_dir, vintage_ch)
  expect_equal(pip_create_globals(root_dir = root,
                                  create_dir = TRUE,
                                  vintage = vintage_DF)$vintage_dir, vintage_ch)


  vd <- pip_create_globals(root_dir = root,
                           create_dir = TRUE)
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

