#' Initialize a new log
#'
#' Creates a new named log as a list to store log entries. If the log already
#' exists, it will be reset (unless `overwrite = FALSE`).
#'
#' @param name Name of the log to create (default: "default").
#' @param overwrite Whether to overwrite an existing log with the same name.
#'
#' @return Invisibly returns the initialized log name.
#' \dontrun{
#' log_init("testlog")
#' # This basically checks whther it already exists
#' log_init("testlog", overwrite = FALSE)
#' }
#' @export
log_init <- function(name = getOption("pipfun.log.default"),
                     overwrite = getOption("pipfun.log_init.ow")) {

  if (rlang::env_has(.piplogenv, name)) {
    if (!isTRUE(overwrite)) {
      cli::cli_abort("Log {.field {name}} already exists.
                     Use {.code overwrite = TRUE} to replace it.")
    }
  }

  log <- data.table(
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
  rlang::env_poke(.piplogenv, name, log)

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
#' @param .env Environment from which to capture arguments (default:
#'   parent.frame()).
#'
#' @return Invisibly returns the updated log.
#' @export
log_add <- function(event,
                    message,
                    name   = getOption("pipfun.log.default"),
                    args   = NULL,
                    output = NULL,
                    .trace = NULL,
                    .env   = parent.frame()) {

  if (!rlang::env_has(.piplogenv, name)) {
    log_init(name)
  }

  log <- rlang::env_get(.piplogenv, name)

  call_stack <- sys.calls()
  calling_fn <- if (length(call_stack) > 1) {
    deparse(call_stack[[length(call_stack) - 1]])
  } else {
    "unknown"
  }

  calling_pkg <- rlang::env_name(.env)

  new_row <- data.table(
    time     = Sys.time(),
    package  = calling_pkg,
    fun      = calling_fn,
    event    = event,
    message  = as.character(message),
    args     = list(args),
    output   = list(output),
    trace    = list(if (!is.null(.trace)) .trace else sys.call(-1))
  )

  log <- rbindlist(list(log, new_row),
                   use.names = TRUE,
                   fill = TRUE)
  setattr(log, "class", c("piplog", class(log)))
  rlang::env_poke(.piplogenv, name, log)

  invisible(TRUE)
}



#' Save a log to disk
#'
#' Saves a log stored in `.piplogenv` to a `.qs` file for persistence.
#'
#' @param name Name of the log in memory (default:
#'   `getOption("pipfun.log.default")`).
#' @param path File path to save the log to. If missing, defaults to
#'   `{name}.qs`.
#' @param compress Whether to compress the file (default: TRUE).
#'
#' @return Invisible `TRUE` if successful.
#' @export
log_save <- function(name     = getOption("pipfun.log.default", "default"),
                     path     = NULL,
                     compress = TRUE) {

  if (!requireNamespace("qs", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg qs} is required to save logs.")
  }

  if (!exists(name, envir = .piplogenv)) {
    cli::cli_abort("Log {.field {name}} does not exist in memory.")
  }

  log <- get(name, envir = .piplogenv)

  if (!inherits(log, "piplog")) {
    cli::cli_abort("Object {.field {name}} is not a valid piplog.")
  }

  if (is.null(path)) {
    path <- fs::path(name, ext = "qs")
  }
  if (fs::path_ext(path) != "qs") {
    path <- fs::path(path, ext = "qs")
  }

  qs::qsave(log, file = path, preset = if (compress) "high" else "fast")
  cli::cli_alert_success("Log {.field {name}} saved to {.path {path}}")
  invisible(TRUE)
}


#' Load a log from a .qs file
#'
#' Loads a previously saved log into `.piplogenv`, optionally under a different
#' name.
#'
#' @param path Path to the `.qs` file to load.
#' @param name Name to assign to the log in memory (default: inferred from
#'   filename).
#' @param overwrite Whether to overwrite an existing log of the same name
#'   (default: FALSE).
#'
#' @return Invisibly returns the name of the loaded log.
#' @export
log_load <- function(path,
                     name      = NULL,
                     overwrite = FALSE) {

  if (!requireNamespace("qs", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg qs} is required to load logs.")
  }

  if (!fs::file_exists(path)) {
    cli::cli_abort("File {.file {path}} does not exist.")
  }

  log <- qs::qread(path)

  if (!inherits(log, "piplog")) {
    cli::cli_abort("File does not contain a valid {.cls piplog} object.")
  }

  if (is.null(name)) {
    name <- path |>
      fs::path_ext_remove() |>
      fs::path_file()
  }

  if (rlang::env_has(.piplogenv, name) && !overwrite) {
    cli::cli_abort("A log named {.field {name}} already exists in memory. Use {.code overwrite = TRUE} to replace it.")
  }

  rlang::env_poke(.piplogenv, name, log)
  cli::cli_alert_success("Log {.field {name}} loaded from {.file {path}}")
  invisible(name)
}

#' Reset or delete a log from memory
#'
#' Clears a log from the internal environment. Use this to start over or free
#' memory.
#'
#' @param name Name of the log to remove (default:
#'   `getOption("pipfun.log.default")`).
#'
#' @return Invisibly returns TRUE if the log was removed.
#' @export
log_reset <- function(name = getOption("pipfun.log.default", "default")) {
  if (!rlang::env_has(.piplogenv, name)) {
    cli::cli_alert_info("Log {.field {name}} is not present.")
    return(invisible(FALSE))
  }

  rlang::env_unbind(.piplogenv, name)
  cli::cli_alert_success("Log {.field {name}} has been reset.")
  invisible(TRUE)
}


#' Filter log entries
#'
#' @param name Name of the log (default: `pipfun.log.default`)
#' @param event Type of event to filter ("info", "warning", "error", etc.)
#' @param fun Optional: function name(s) to filter
#' @param after Optional: filter entries after this datetime
#' @param before Optional: filter entries before this datetime
#'
#' @return A filtered `piplog` object.
#' @export
log_filter <- function(name    = getOption("pipfun.log.default"),
                       event   = NULL,
                       fun     = NULL,
                       after   = NULL,
                       before  = NULL) {

  log <- name |>
    log_get() |>
    copy()

  setDT(log)

  # not elegant but works
  e <- event
  f <- fun

  if (!is.null(event))  {
    log <- log[event %in% e]
  }
  if (!is.null(fun)) {
    log <- log[fun %in% f]
  }
  if (!is.null(after))  {
    log <- log[time >= as.POSIXct(after)]
  }
  if (!is.null(before)) {
    log <- log[time <= as.POSIXct(before)]
  }
  setattr(log, "class", c("piplog", class(log)))
  return(log)
}


#' Get a particular log entries
#'
#' @param name Name of the log (default: `pipfun.log.default`)
#'
#' @return A raw `piplog` object.
#' @export
log_get <- function(name    = getOption("pipfun.log.default")) {
  if (!rlang::env_has(.piplogenv, name)) {
    cli::cli_abort("Log {.field {name}} does not exist.")
  }

  log <- rlang::env_get(.piplogenv, name)

  if (!inherits(log, "piplog")) {
    cli::cli_abort(c(x = "Object {.field {name}} is not a valid piplog.",
                     i = "{.field {name}}'s class is {class(log)}"))
  }
  log
}
