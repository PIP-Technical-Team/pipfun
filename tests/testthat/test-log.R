
withr::local_options(list(pipfun.log_ini.ow = TRUE))

# log_init ------------
test_that("log_init creates an empty log", {
  log_init("testlog", overwrite = TRUE)
  log <- rlang::env_get(.piplogenv, "testlog")
  expect_s3_class(log, "piplog")
  expect_equal(nrow(log), 0)
})

test_that("log_init fails if log already exists", {
  log_init("testlog", overwrite = TRUE)
  log_init("testlog") |>
    expect_error()
})


# log_add ------------
test_that("log_add appends a new entry", {
  log_init("testlog", overwrite = TRUE)
  log_add(event = "info", message = "Test message", name = "testlog")
  log <- rlang::env_get(.piplogenv, "testlog")
  expect_equal(nrow(log), 1)
  expect_equal(log$message[[1]], "Test message")
  expect_equal(log$event[[1]], "info")
})

test_that("log_add captures arguments when args is NULL", {
  log_init("test_capture", overwrite = TRUE)

  # Simulate a caller function
  simulate_caller <- function(a = 1, b = "text") {
    log_add("info", "capturing args",
            name = "test_capture")
  }

  simulate_caller()

  log <- log_get("test_capture")
  last <- log[.N]

  expect_type(last$args[[1]], "list")
  expect_named(last$args[[1]], c("a", "b"))
  expect_equal(last$args[[1]]$a, 1)
  expect_equal(last$args[[1]]$b, "text")
})


test_that("log_add uses provided args if not NULL", {
  log_init("test_explicit", overwrite = TRUE)

  log_add("info", "manual args", name = "test_explicit", args = list(foo = 123))

  log <- log_get("test_explicit")
  last <- log[.N]

  expect_equal(last$args[[1]]$foo, 123)
})


test_that("log_add captures output if provided", {
  log_init("test_output", overwrite = TRUE)

  result <- sum(1:5)
  log_add("info", "with result", name = "test_output", output = result)

  log <- log_get("test_output")
  last <- log[.N]

  expect_equal(last$output[[1]], result)
})


test_that("log_add fallback to sys.call when .trace is NULL", {
  log_init("test_trace", overwrite = TRUE)

  log_add("info", "trace test", name = "test_trace")

  log <- log_get("test_trace")
  last <- log[.N]

  expect_true(inherits(last$trace[[1]], "call"))
})


### capturing logmeta -----------
test_that("log_add() captures logmeta passed to log_info()", {
  log_init("dotstest", overwrite = TRUE)

  log_info("Testing dots", name = "dotstest", logmeta = list(a = 1:3, b = "hello"))
  log <- log_get("dotstest")
  logmeta <- log$logmeta[[1]]

  expect_true("a" %in% names(logmeta))
  expect_equal(logmeta$b, "hello")
})

test_that("log_add() captures logmeta passed directly", {
  log_init("directdots", overwrite = TRUE)

  log_add("info", "Direct dots test", name = "directdots",
          logmeta = list(x = 99, y = "yay"))

  log <- log_get("directdots")
  logmeta <- log$logmeta[[1]]

  expect_equal(logmeta$x, 99)
  expect_equal(logmeta$y, "yay")
})

test_that("log_add() respects args override", {
  log_init("explicitargs", overwrite = TRUE)

  log_add("info", "Explicit args test", name = "explicitargs",
          args = list(z = "onlythis"))

  log <- log_get("explicitargs")
  args <- log$args[[1]]

  expect_equal(names(args), "z")
  expect_equal(args$z, "onlythis")
})

test_that("log_add() merges all captured arguments and logmeta correctly", {
  log_init("mergeargs", overwrite = TRUE)

  test_fun <- function(a = 1, b = 2, ...) {
    log_info("Merged", name = "mergeargs", logmeta = list(...))
  }

  test_fun(x = "hello", y = TRUE)

  log <- log_get("mergeargs")
  args <- log$args[[1]]
  meta <- log$logmeta[[1]] # THis is another list because of list(...)

  expect_true(all(c("a", "b", "x", "y") %in% names(args)))
  expect_equal(args$x, "hello")
  expect_true(args$y)
  expect_equal(meta$x, "hello")
  expect_true(meta$y)

})




# helpers ------------
test_that("log_info() captures arguments from parent function", {
  log_init("loginfo_test", overwrite = TRUE)

  my_function <- function(a = 1, b = 2, ...) {
    log_info("Info from my_function", name = "loginfo_test")
  }

  my_function(x = 42)

  log <- log_get("loginfo_test")
  args <- log$args[[1]]

  expect_true("a" %in% names(args))
  expect_true("b" %in% names(args))
  expect_true("x" %in% names(args))
  expect_equal(args$x, 42)
})

test_that("log_warn() captures arguments and logs warning", {
  log_init("logwarn_test", overwrite = TRUE)

  my_warn <- function(y = 10, ...) {
    log_warn("Warning issued", name = "logwarn_test")
  }

  my_warn(extra = TRUE)
  log <- log_get("logwarn_test")
  args <- log$args[[1]]

  expect_equal(log$event[[1]], "warning")
  expect_true("y" %in% names(args))
  expect_true("extra" %in% names(args))
})

test_that("log_error() captures environment and logs error", {
  log_init("logerror_test", overwrite = TRUE)

  my_error <- function(code = "A") {
    log_error("Error encountered", name = "logerror_test")
  }

  my_error()
  log <- log_get("logerror_test")
  args <- log$args[[1]]

  expect_equal(log$event[[1]], "error")
  expect_equal(args$code, "A")
})


## print --------------
test_that("print.piplog produces output without error", {
  skip() # we need to test when print.piplog is finished.
  msg <- "Printing test"
  log_init("testlog", overwrite = TRUE)
  log_info(msg, name = "testlog")
  log <- rlang::env_get(.piplogenv, "testlog")
  expect_output(print(log), msg, fixed = FALSE)
})

test_that("log_exists() works as expected", {
  log_init("existtest", overwrite = TRUE)
  expect_true(log_exists("existtest"))

  log_reset("existtest")
  expect_false(log_exists("existtest"))
})



# Save and load --------
test_that("log_save and log_load work with stamp alias", {
  root <- fs::path(tempdir(), "pipfun_test_alias")
  fs::dir_create(root)
  stamp::st_init(root, alias = "A")

  id <- fs::path(root, "demo_log")         # artifact path (extension added by functions)

  # prepare and save log
  log_init("demo", overwrite = TRUE)
  log_add("info", "smoke test", name = "demo")
  log_save(name = "demo", id = id, alias = "A", format = "qs2")

  # versions exist under the correct extension and alias
  vr <- stamp::st_versions(fs::path_ext_set(id, "qs2"), alias = "A")
  expect_true(nrow(vr) > 0)

  # load into a new name
  log_reset("demo")
  log_load(id = id, name = "demo_loaded", alias = "A", format = "qs2")
  expect_true(rlang::env_has(.piplogenv, "demo_loaded"))
  expect_s3_class(rlang::env_get(.piplogenv, "demo_loaded"), "piplog")

  # list available versions via log_load(..., version = "available")
  avail <- log_load(id = id, version = "available", alias = "A", format = "qs2")
  expect_true(NROW(avail) > 0)
})

test_that("log_load respects overwrite flag", {
  root <- fs::path(tempdir(), "pipfun_test_alias_overwrite")
  fs::dir_create(root)
  stamp::st_init(root, alias = "B")

  id <- fs::path(root, "demo2")
  log_init("demo2", overwrite = TRUE)
  log_add("info", "for overwrite", name = "demo2")
  log_save(name = "demo2", id = id, alias = "B", format = "qs2")

  # create an in-memory log with the target name and ensure load errors without overwrite
  log_init("already_here", overwrite = TRUE)
  expect_error(log_load(id = id, name = "already_here", alias = "B", overwrite = FALSE))
})


# log_has_errors ----------------------------------------------------------

test_that("log_has_errors() returns correct logical or filtered log", {
  log_init("testlog", overwrite = TRUE)
  expect_false(log_has_errors("testlog"))

  log_error("something bad happened", name = "testlog")
  expect_true(log_has_errors("testlog"))

  log <- log_has_errors("testlog", show = TRUE)
  expect_s3_class(log, "piplog")
  expect_equal(log$event, "error")
})


## log get ------
test_that("log_get restores class if dropped", {
  log_init("reclass_test", overwrite = TRUE)
  log_info("Test", name = "reclass_test")

  # simulate class drop
  log <- rlang::env_get(.piplogenv, "reclass_test")
  setattr(log, "class", "data.table")
  rlang::env_poke(.piplogenv, "reclass_test", log)

  restored <- log_get("reclass_test")
  expect_s3_class(restored, "piplog")
})
