test_that("typed logging preserves structured metadata in tryCatch handlers", {
  log_init("capture_spike", overwrite = TRUE)

  capture_in_handler <- function(country, year) {
    tryCatch(
      stop("synthetic failure"),
      error = function(e) {
        log_error(
          conditionMessage(e),
          name = "capture_spike",
          logmeta = list(
            error = "dlw_acquisition_inf",
            country = country,
            year = year,
            condition_msg = conditionMessage(e)
          )
        )
      }
    )
  }

  capture_in_handler("CHN", 2020L)
  entry <- log_get("capture_spike")[.N]

  expect_equal(entry$logmeta[[1L]]$error, "dlw_acquisition_inf")
  expect_equal(entry$logmeta[[1L]]$country, "CHN")
  expect_equal(entry$logmeta[[1L]]$year, 2020L)
  expect_equal(entry$logmeta[[1L]]$condition_msg, "synthetic failure")
  expect_true(is.list(entry$args[[1L]]))
})

test_that("typed logging preserves structured metadata in lapply callbacks", {
  log_init("capture_lapply_spike", overwrite = TRUE)

  capture_in_callback <- function(survey) {
    tryCatch(
      stop("synthetic callback failure"),
      error = function(e) {
        log_error(
          conditionMessage(e),
          name = "capture_lapply_spike",
          logmeta = list(
            error = "dlw_validation_inf",
            survey = survey,
            condition_msg = conditionMessage(e)
          )
        )
      }
    )
  }

  invisible(lapply(c("CHN_2020", "IND_2021"), capture_in_callback))
  log <- log_get("capture_lapply_spike")

  expect_equal(nrow(log), 2L)
  expect_setequal(
    vapply(log$logmeta, `[[`, character(1), "survey"),
    c("CHN_2020", "IND_2021")
  )
  expect_true(all(
    vapply(log$logmeta, `[[`, character(1), "condition_msg") ==
      "synthetic callback failure"
  ))
})
