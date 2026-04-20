#' Save PIP  data
#'
#' Save PIP  data with data signature.
#'
#' @param x data.frame Data frame to be signed and saved.
#' @param measure character: Measure to be used. e.g., "cpi" or "ppp".
#' @param msrdir character: Directory where the data and data signature will be
#'   saved.
#' @param force logical: If TRUE data will be overwritten.
#' @param save_dta logical: If TRUE a Stata (.dta) version of the dataset is
#'   also saved.
#' @inheritParams pip_create_globals
#' @return logical
#' @export
pip_sign_save <- function(x,
                          measure,
                          msrdir,
                          force = FALSE,
                          save_dta = FALSE,
                          verbose = getOption("pipfun.verbose")) {

  #   ____________________________________________________________________________
  #   Files and directories                                                   ####

  wholedir <- fs::path(msrdir, "_vintage")

  if (!fs::dir_exists(wholedir)) {
    fs::dir_create(wholedir, recurse = TRUE)
  }

  # Signature file path in production
  ds_production_path <- fs::path(msrdir, paste0(measure, "_datasignature.txt"))

  if (fs::file_exists(ds_production_path)) {
    ds_production <- readr::read_lines(ds_production_path)[[1]]
  } else {
    ds_production <- "0000"
    ms_status <- "new"
  }

  #   __________________________________________________________________
  #   Check signature                                                  ####

  ds_dlw <- digest::digest(x, algo = "xxhash64")

  if (ds_dlw != ds_production) {
    ms_status <- "changed"
  } else {
    ms_status <- "unchanged"
  }

  if (force) {
    ms_status <- "forced"
  }

  #   ____________________________________________________________________________
  #   If signature changes or force = TRUE                                     ####

  if (ms_status %in% c("forced", "changed", "new")) {

    time <- format(Sys.time(), "%Y%m%d%H%M%S")
    attr(x, "datetime") <- time

    var_class <- purrr::map(x, class)

    # Save main file
    if (is.data.frame(x) && !("list" %in% unique(var_class))) {
      if (save_dta) {
        haven::write_dta(
          data = x,
          path = fs::path(msrdir, measure, ext = "dta")
        )
      }
    }

    qs2::qs_save(
      x = x,
      file = fs::path(msrdir, measure, ext = "qs")
    )

    # Save vintages
    if (is.data.frame(x) && !("list" %in% unique(var_class))) {
      if (save_dta) {
        haven::write_dta(
          data = x,
          path = fs::path(msrdir, "_vintage/", paste0(measure, "_", time), ext = "dta")
        )
      }
    }

    qs2::qs_save(
      x = x,
      file = fs::path(
        msrdir,
        "_vintage/",
        paste0(measure, "_", time),
        ext = "qs2"
      )
    )

    # Write new signature
    readr::write_lines(
      x = ds_dlw,
      file = ds_production_path
    )

    # Message
    fillintext <- fcase(ms_status == "new", "was not found",
                        ms_status == "forced", "has been changed forcefully",
                        ms_status == "changed", "has changed",
                        default = "")

    if (verbose) {
      cli::cli_alert_warning(
        "Data signature {fillintext}. {.file {measure}.qs} has been updated"
      )
    }

    return(invisible(TRUE))

  } else {
    if (verbose) {
      cli::cli_alert_info("Data signature is up to date. {cli::col_blue('No update performed')}")
    }
    return(invisible(FALSE))
  }
}
