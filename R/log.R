#' Initialize a new log
#'
#' Creates a new named log as a list to store log entries. If the log already exists,
#' it will be reset (unless `overwrite = FALSE`).
#'
#' @param name Name of the log to create (default: "default").
#' @param overwrite Whether to overwrite an existing log with the same name.
#'
#' @return Invisibly returns the initialized log name.
#' @export
log_init <- function(name = "default", overwrite = TRUE) {
  if (exists(name, envir = .piplogenv) && !overwrite) {
    cli::cli_alert_warning("Log {.field {name}} already exists. Use `overwrite = TRUE` to reset.")
    return(invisible(NULL))
  }

  log <- data.table::data.table(
    time     = as.POSIXct(character()),
    package  = character(),
    function = character(),
    event    = character(),
    message  = character(),
    args     = list(),
    output   = list(),
    trace    = list()
  )

  class(log) <- c("piplog", class(log))
  assign(name, log, envir = .piplogenv)
  invisible(name)
}
