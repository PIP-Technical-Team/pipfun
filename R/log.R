#' Add a log entry
#'
#' Adds a structured entry to a named log, capturing metadata such as timestamp,
#' calling function, event type, message, arguments used, optional output, and
#' more.
#'
#' This function automatically captures all arguments from the calling function,
#' including `...`. You can also manually add custom metadata using the
#' `logmeta` argument.
#'
#' @param event Type of event (e.g. `"error"`, `"info"`, `"warning"`).
#' @param message Description of the log entry.
#' @param name Name of the log (default: `options("pipfun.log.default")`).
#' @param args Optional list of captured arguments (default: auto-captured).
#' @param logmeta Optional named list of metadata to attach (merged with args).
#' @param output Optional result or return value.
#' @param .trace Optional call stack or trace override.
#' @param .env Internal use. Calling environment (default:
#'   `rlang::caller_env()`).
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @examples
#' log_init("demo_log", overwrite = TRUE)
#'
#' # Automatically captures arguments from the caller:
#' my_fun <- function(x, y = 1, ...) {
#'   result <- x + y
#'   log_info("Ran my_fun", name = "demo_log", output = result)
#'   return(result)
#' }
#' my_fun(3, z = 9)
#'
#' # Add custom metadata manually:
#' log_info("Logging manually", name = "demo_log",
#'          logmeta = list(stage = "processing", user = "analyst"))
#' @export
log_add <- function(event,
                    message,
                    name    = getOption("pipfun.log.default"),
                    args    = NULL,
                    logmeta = NULL,
                    output  = NULL,
                    .trace  = NULL,
                    .env    = rlang::caller_env()) {




  # --- Auto-capture args from caller if not supplied ---
  # Get the current call stack
  call_stack <- sys.calls() # (list of all active calls)
  # Get the current environment stack
  env_stack  <- sys.frames() # (list of all active environments)
  # Number of calls in the stack
  n          <- length(call_stack)
  # Start from the immediate caller (one before this function)
  target_idx <- n - 1
  # Move up the stack until we find a function that does NOT start with 'log_'
  while (target_idx > 0 && grepl("^log_", deparse(call_stack[[target_idx]])[1])) {
    target_idx <- target_idx - 1
  }
  # The environment of the true calling function
  target_env <- env_stack[[target_idx]]

  # The function object of the true calling function (may error if not found)
  # call_stack[[target_idx]] is the call (as a language object) to the target
  # function. call_stack[[target_idx]][[1]] extracts the function name or object
  # being called. eval(..., envir = target_env) evaluates that function name in
  # the environment where it was called, so we get the actual function object
  # (not just its name as a symbol). We need it as symbol so I can access its
  # arguments... or formals.
  target_fun <- tryCatch(eval(call_stack[[target_idx]][[1]],
                              envir = target_env),
                         error = function(e) NULL)

  # --- Argument capture ---
  if (is.null(args)) {
    # If we found a valid function, get its formal argument names
    if (!is.null(target_fun) && is.function(target_fun)) {
      arg_names <- names(formals(target_fun)) # all argument names
      arg_names <- arg_names[arg_names != "..."] # exclude ...
      # Get the values of those arguments from the target environment
      args <- mget(arg_names,
                   envir = target_env,
                   ifnotfound = vector("list", length(arg_names)))
      # If ... is present, try to capture its values as well
      if ("..." %in% names(formals(target_fun))) {
        dots <- tryCatch(evalq(list(...), envir = target_env),
                         error = function(e) NULL)
        if (!is.null(dots)) args <- c(args, dots)
      }
    } else {
      # If we can't find a valid function, just use an empty list
      args <- list()
    }
  }

  # Retrieve the log object from the logging environment
  log <- rlang::env_get(.piplogenv, name)

  # --- Extract calling function name (from target_idx) ---
  # Use the same target_idx as above to get the function name as a string
  calling_fn <- if (target_idx > 0) {
    cf <- deparse(call_stack[[target_idx]]) |> # deparse the call
      trimws() |>                            # trim whitespace
      paste(collapse = " ")                 # collapse multi-line calls
    invisible(cf)                            # return as invisible (for assignment)
  } else {
    "unknown"
  }

  new_row <- data.table(
    time    = Sys.time(),
    package = rlang::env_name(.env),
    fun     = calling_fn,
    event   = tolower(event),
    message = as.character(message),
    args    = list(args),
    logmeta = list(logmeta),
    output  = list(output),
    trace   = list(if (!is.null(.trace)) .trace else sys.call(-1))
  )

  log <- rbindlist(list(log, new_row),
                   use.names = TRUE,
                   fill = TRUE)
  setattr(log, "class", c("piplog", class(log)))
  rlang::env_poke(.piplogenv, name, log)

  # future = list(level = "debug", module = "foo")

  invisible(TRUE)
}


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


#' Save a log to disk
#'
#' Saves a log stored in `.piplogenv` to a `.qs` file for persistence.
#'
#' @param name Name of the log in memory (default:
#'   `getOption("pipfun.log.default")`).
#' @param path `r lifecycle::badge("deprecated")` `path` is no longer supported.
#'   Use `board` argument now. If value passed to `path` is not a pins board, it
#'   will through an error.
#' @param board pins board
#' @param pin_name name of pin that will be used to load the log. By default it is the same as `name`.
#' @inheritDotParams pins::pin_write title description metadata tags
#'
#'
#' @return Invisible `TRUE` if successful.
#' @export
log_save <- function(name     = getOption("pipfun.log.default", "default"),
                     board    = NULL,
                     pin_name = name,
                     path     =  deprecated(),
                     ...) {

  if (lifecycle::is_present(path)) {
    lifecycle::deprecate_warn(
      when = "0.3.7",
      what = "log_save(path)",
      with = "log_save(board)",
      details = "all the logs will be saved as pins, so you need to use a pins board rather than a directory path"
    )
    board <- path
  }

  if (!inherits(board, "pins_board")) {
    cli::cli_abort("{.arg board} must be a pins_board class object")
  }


  if (!exists(name, envir = .piplogenv)) {
    cli::cli_abort("Log {.field {name}} does not exist in memory.")
  }

  log <- get(name, envir = .piplogenv)

  if (!inherits(log, "piplog")) {
    cli::cli_abort("Object {.field {name}} is not a valid piplog.")
  }

  pins::pin_write(board     = board,
                  x         = log,
                  name      = pin_name,
                  type      = "qs",
                  versioned = TRUE,
                  ...)

  cli::cli_alert_success("Log {.field {name}} saved to {.path {path}}")
  invisible(TRUE)
}


#' Load a log from a .qs file
#'
#' Loads a previously saved log into `.piplogenv`, optionally under a different
#' name.
#'
#' @param board pins board
#' @param pin_name name of pin that will be used to load the log. By default it
#'   is the same as `name`.
#' @inheritParams pins::pin_read
#' @param name `r lifecycle::badge("deprecated")` `name` has been superseded by
#'   `pin_name`. It is nor inferred from filename any more.
#' @param path `r lifecycle::badge("deprecated")` `path` is no longer supported.
#'   Use `board` argument now. If value passed to `path` is not a pins board, it
#'   will through an error.
#' @inheritDotParams pins::pin_read
#'
#' @param overwrite logical: whether to override the log in `.piplogenv` with
#'   the same `pin_name`. Default is FALSE.
#'
#' @return Invisibly returns the name of the loaded log.
#' @export
log_load <- function(board,
                     pin_name  = name,
                     version   = NULL,
                     hash      = NULL,
                     path      = deprecated(),
                     name      = deprecated(),
                     overwrite = FALSE,
                     ...) {

  if (lifecycle::is_present(path)) {
    lifecycle::deprecate_warn(
      when = "0.3.7",
      what = "log_save(path)",
      with = "log_save(board)",
      details = "all the logs will be saved as pins, so you need to use a pins board rather than a directory path"
    )
    board <- path
  }
  if (lifecycle::is_present(name)) {
    lifecycle::deprecate_warn(
      when = "0.3.7",
      what = "log_save(name)",
      with = "log_save(pin_name)"
    )
    pin_name <- name
  }

  if (!inherits(board, "pins_board")) {
    cli::cli_abort("{.arg board} must be a pins_board class object")
  }

  log <- pins::pin_read(board = board,
                        name = pin_name,
                        version = version,
                        hash = hash,
                        ...)

  if (!inherits(log, "piplog")) {
    cli::cli_abort("File does not contain a valid {.cls piplog} object.")
  }


  if (rlang::env_has(.piplogenv, pin_name) && !overwrite) {
    cli::cli_abort("A log named {.field {name}} already exists in memory. Use {.code overwrite = TRUE} to replace it.")
  }

  rlang::env_poke(.piplogenv, pin_name, log)
  cli::cli_alert_success("Log {.field {pin_name}} loaded from {.file {path}}")
  invisible(pin_name)
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
    # Restore class silently if it's just been dropped by DT ops
    if (is.data.table(log)) {
      setattr(log, "class", unique(c("piplog", class(log))))
    } else {
      cli::cli_abort(c(x = "Object {.field {name}} is not a valid piplog.",
                       i = "{.field {name}}'s class is {class(log)}"))
    }
  }
  invisible(log)
}
