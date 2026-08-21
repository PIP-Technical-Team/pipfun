test_that("log_save_checkpoint saves a named stage checkpoint", {
  root <- withr::local_tempdir()
  stamp::st_init(root, alias = "checkpoint_test")

  log_init("pipdata_log", overwrite = TRUE)
  log_add(
    event = "info",
    message = "checkpoint test",
    name = "pipdata_log"
  )

  result <- log_save_checkpoint(
    name = "pipdata_log",
    stage = "dlw",
    alias = "checkpoint_test"
  )

  expect_true(!is.null(result))
  expect_true(
    fs::file_exists(
      fs::path(root, "pipdata_log_checkpoint_dlw.qs2")
    )
  )
})

test_that("log_save_checkpoint preserves metadata for each stage", {
  root <- withr::local_tempdir()
  stamp::st_init(root, alias = "checkpoint_metadata")

  log_init("pipdata_log", overwrite = TRUE)
  log_add(
    event = "info",
    message = "checkpoint metadata test",
    name = "pipdata_log"
  )

  log_save_checkpoint(
    name = "pipdata_log",
    stage = "dlw",
    alias = "checkpoint_metadata"
  )
  log_save_checkpoint(
    name = "pipdata_log",
    stage = "pipeline",
    alias = "checkpoint_metadata"
  )

  dlw_info <- stamp::st_info(
    fs::path(root, "pipdata_log_checkpoint_dlw.qs2"),
    alias = "checkpoint_metadata"
  )
  pipeline_info <- stamp::st_info(
    fs::path(root, "pipdata_log_checkpoint_pipeline.qs2"),
    alias = "checkpoint_metadata"
  )

  expect_equal(dlw_info$sidecar$stage, "dlw")
  expect_equal(pipeline_info$sidecar$stage, "pipeline")
  expect_type(dlw_info$sidecar$checkpoint_time, "character")
  expect_type(pipeline_info$sidecar$checkpoint_time, "character")
})

test_that("log_save_checkpoint validates stage", {
  expect_error(
    log_save_checkpoint(stage = "invalid", alias = "checkpoint_test"),
    "'arg' should be one of"
  )
})
