test_that("log_save_checkpoint saves a named stage checkpoint", {
  root <- withr::local_tempdir()
  alias <- paste0("checkpoint_test_", digest::digest(tempfile()))
  stamp::st_init(root, alias = alias)

  log_init("pipdata_log", overwrite = TRUE)
  log_add(
    event = "info",
    message = "checkpoint test",
    name = "pipdata_log"
  )

  result <- log_save_checkpoint(
    name = "pipdata_log",
    stage = "dlw",
    alias = alias
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
  alias <- paste0("checkpoint_metadata_", digest::digest(tempfile()))
  stamp::st_init(root, alias = alias)

  log_init("pipdata_log", overwrite = TRUE)
  log_add(
    event = "info",
    message = "checkpoint metadata test",
    name = "pipdata_log"
  )

  log_save_checkpoint(
    name = "pipdata_log",
    stage = "dlw",
    alias = alias
  )
  log_save_checkpoint(
    name = "pipdata_log",
    stage = "pipeline",
    alias = alias
  )

  dlw_info <- stamp::st_info(
    fs::path(root, "pipdata_log_checkpoint_dlw.qs2"),
    alias = alias
  )
  pipeline_info <- stamp::st_info(
    fs::path(root, "pipdata_log_checkpoint_pipeline.qs2"),
    alias = alias
  )

  expect_equal(dlw_info$sidecar$stage, "dlw")
  expect_equal(pipeline_info$sidecar$stage, "pipeline")
  expect_type(dlw_info$sidecar$checkpoint_time, "character")
  expect_type(pipeline_info$sidecar$checkpoint_time, "character")
})

test_that("log_save_checkpoint validates stage", {
  alias <- paste0("checkpoint_stage_", digest::digest(tempfile()))
  expect_error(
    log_save_checkpoint(stage = "invalid", alias = alias),
    "must be exactly one of"
  )
})

test_that("log_save_checkpoint rejects path-like log names", {
  alias <- paste0("checkpoint_path_", digest::digest(tempfile()))

  expect_error(
    log_save_checkpoint(name = "../outside", alias = alias),
    "must be a log name"
  )
  expect_error(
    log_save_checkpoint(name = "C:/outside", alias = alias),
    "must be a log name"
  )
  expect_error(
    log_save_checkpoint(name = "C:outside", alias = alias),
    "must be a log name"
  )
})

test_that("log_save_checkpoint rejects stage abbreviations", {
  alias <- paste0("checkpoint_abbrev_", digest::digest(tempfile()))

  expect_error(
    log_save_checkpoint(stage = "d", alias = alias),
    "must be exactly one of"
  )
  expect_error(
    log_save_checkpoint(stage = "p", alias = alias),
    "must be exactly one of"
  )
})

test_that("log_save_checkpoint falls back to the default log name", {
  alias <- paste0("checkpoint_default_", digest::digest(tempfile()))
  withr::local_options(pipfun.log.default = NULL)
  stamp::st_init(withr::local_tempdir(), alias = alias)
  log_init("default", overwrite = TRUE)
  log_add("info", "default checkpoint", name = "default")

  expect_no_error(
    log_save_checkpoint(stage = "dlw", alias = alias)
  )
})

test_that("log_save_checkpoint uses the registered stamp default alias", {
  root <- withr::local_tempdir()
  stamp::st_init(root)
  log_init("default_alias_log", overwrite = TRUE)
  log_add("info", "default alias checkpoint", name = "default_alias_log")

  result <- log_save_checkpoint(name = "default_alias_log", stage = "dlw")

  expect_true(fs::file_exists(result$path))
  expect_equal(stamp::st_info(result$path)$sidecar$stage, "dlw")
})

test_that("log_save_checkpoint merges custom metadata and rejects reserved ids", {
  root <- withr::local_tempdir()
  alias <- paste0("checkpoint_forward_", digest::digest(tempfile()))
  stamp::st_init(root, alias = alias)
  log_init("forwarding_log", overwrite = TRUE)
  log_add("info", "forwarding checkpoint", name = "forwarding_log")

  log_save_checkpoint(
    name = "forwarding_log",
    stage = "dlw",
    alias = alias,
    metadata = list(run_id = "run-1"),
    code = quote(forwarding_log)
  )
  info <- stamp::st_info(
    fs::path(root, "forwarding_log_checkpoint_dlw.qs2"),
    alias = alias
  )

  expect_equal(info$sidecar$run_id, "run-1")
  expect_equal(info$sidecar$stage, "dlw")
  expect_true(is.character(info$sidecar$code_hash))
  expect_length(info$sidecar$code_hash, 1L)
  expect_error(
    log_save_checkpoint(
      name = "forwarding_log",
      stage = "dlw",
      alias = alias,
      id = "other"
    ),
    "Reserved checkpoint argument"
  )
  expect_error(
    log_save_checkpoint(
      name = "forwarding_log",
      stage = "dlw",
      alias = alias,
      metadata = list(stage = "other")
    ),
    "cannot be overridden"
  )
})

test_that("repeated unchanged checkpoints refresh checkpoint metadata", {
  root <- withr::local_tempdir()
  alias <- paste0("checkpoint_repeat_", digest::digest(tempfile()))
  stamp::st_init(root, alias = alias)
  log_init("repeated_log", overwrite = TRUE)
  log_add("info", "repeated checkpoint", name = "repeated_log")

  path <- fs::path(root, "repeated_log_checkpoint_dlw.qs2")
  first_result <- log_save_checkpoint(name = "repeated_log", alias = alias)
  Sys.sleep(0.1)
  second_result <- log_save_checkpoint(name = "repeated_log", alias = alias)
  versions <- stamp::st_versions(path, alias = alias)

  expect_false(isTRUE(second_result$skipped))
  expect_equal(nrow(versions), 2L)
  expect_false(identical(first_result$version_id, second_result$version_id))
})
