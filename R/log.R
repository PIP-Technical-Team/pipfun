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
    fun      = character(),
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


#' Add a log entry
#'
#' Adds a structured entry to a named log, including timestamp, caller info,
#' event type, message, and optional trace and data.
#'
#' @param event Type of event (e.g., "error", "info", "warning").
#' @param message A descriptive message.
#' @param name Name of the log to write to (default: "default").
#' @param output Optional output to capture.
#' @param .trace Optional trace object or call stack.
#' @param .env Environment from which to capture arguments (default: parent.frame()).
#'
#' @return Invisibly returns the updated log.
#' @export
log_add <- function(event,
                    message,
                    name   = "default",
                    output = NULL,
                    .trace = NULL,
                    .env   = parent.frame()) {

  if (!exists(name, envir = .piplogenv)) {
    log_init(name)
  }

  log <- get(name, envir = .piplogenv)

  call_stack <- sys.calls()
  fun        <- deparse(call_stack[[length(call_stack) - 1]])

  pkg <- tryCatch(utils::packageName(topenv(.env)), error = function(e) NA_character_)

  args <- tryCatch(as.list(.env), error = function(e) list())

  new_row <- data.table(
    time     = Sys.time(),
    package  = pkg,
    fun      = fun,
    event    = event,
    message  = message,
    args     = list(args),
    output   = list(output),
    trace    = list(.trace)
  )

  log <- rbindlist(list(log, new_row), fill = TRUE, use.names = TRUE)
  class(log) <- c("piplog", class(log))

  assign(name, log, envir = .piplogenv)
  invisible(log)
}

