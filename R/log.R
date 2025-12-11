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

  # 1. Get the current call and environment stacks
  call_stack <- sys.calls()      # All active calls (as language objects)
  env_stack  <- sys.frames()     # All active environments
  n          <- length(call_stack) # Number of calls in the stack

  # 2. Find the index of the true calling function (skip log_* wrappers)
  target_idx <- n - 1            # Start from the immediate caller
  while (target_idx > 0 && grepl("^log_", deparse(call_stack[[target_idx]])[1])) {
    target_idx <- target_idx - 1 # Move up until not a log_* function
  }

  # 3. Get the environment of the true calling function, or fallback
  target_env <- if (target_idx > 0) {
    env_stack[[target_idx]]
  } else {
    parent.frame()
  }

  # 4. Try to get the function object of the true caller (for argument capture)
  target_fun <- if (target_idx > 0) {
    tryCatch(eval(call_stack[[target_idx]][[1]],  envir = target_env),
             error = function(e) NULL)
  } else {
    NULL
  }

  # 5. Capture arguments from the caller if not provided
  if (is.null(args)) {
    # If .env was explicitly provided (not the default), use it for argument capture
    if (!identical(.env, rlang::caller_env())) {
      # Try to get all objects in .env except hidden ones
      env_names <- ls(envir = .env, all.names = TRUE)
      # Remove hidden/internal variables (starting with ".")
      env_names <- env_names[!grepl("^\\.", env_names)]
      # Get their values from .env
      args <- mget(env_names, envir = .env, ifnotfound = vector("list", length(env_names)))
      # Short comment: Use .env directly for argument capture if provided
    } else if (!is.null(target_fun) && is.function(target_fun)) {
      # Get all formal argument names except ...
      arg_names <- names(formals(target_fun))
      arg_names <- arg_names[arg_names != "..."]
      # Get their values from the caller's environment
      args <- mget(arg_names,
                   envir = target_env,
                   ifnotfound = vector("list", length(arg_names)))

      # If ... is present, capture those as well
      if ("..." %in% names(formals(target_fun))) {
        dots <- tryCatch(evalq(list(...), envir = target_env), error = function(e) NULL)
        if (!is.null(dots)) args <- c(args, dots)
      }
      # Short comment: Use call stack for argument capture if .env is default
    } else {
      # If no valid function, just use an empty list
      args <- list()
    }
  }

  # 6. Retrieve the log object from the logging environment
  log <- rlang::env_get(.piplogenv, name)

  # 7. Extract the calling function name as a string (for log entry)
  calling_fn <- if (target_idx > 0) {
    cf <- deparse(call_stack[[target_idx]]) |> trimws() |> paste(collapse = " ")
    invisible(cf)
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
#' Saves a log stored in `.piplogenv` to disk using {stamp}, with metadata
#' and versioning support.
#'
#' @param name Name of the log in memory (default:
#'   `getOption("pipfun.log.default")`).
#' @param dir Directory where the log should be saved.
#' @param id File identifier (without extension). Defaults to `name`.
#' @param format File format (default: "qs2").
#' @param metadata Optional named list of metadata to attach.
#' @param code Optional code object whose hash will be stored.
#' @param ... Forwarded to `stamp::st_save()`.
#'
#' @return Invisibly, the result returned by `stamp::st_save()`.
#' @export
log_save <- function(
    name     = getOption("pipfun.log.default", "default"),
    dir,
    id       = name,
    format   = "qs2",
    metadata = list(),
    code     = NULL,
    ...
) {

  # ---- Validate directory ----
  if (missing(dir) || !fs::dir_exists(dir)) {
    cli::cli_abort("Provided directory path does not exist: {.path {dir}}")
  }

  # ---- Validate log ----
  if (!rlang::env_has(.piplogenv, name)) {
    cli::cli_abort("Log {.field {name}} does not exist in memory.")
  }

  log <- rlang::env_get(.piplogenv, name)

  # Restore class if dropped by serialization
  if (is.data.table(log)) {
    setattr(log, "class", unique(c("piplog", class(log))))
  }

  # Final validation
  if (!inherits(log, "piplog")) {
    cli::cli_abort("File does not contain a valid {.cls piplog} object.")
  }

  # ---- Build stamp path ----
  file <- fs::path(dir, id, ext = format)
  sp   <- stamp::st_path(file, format = format)

  # ---- Save with stamp ----
  out <- stamp::st_save(
    x        = log,
    file     = sp,
    metadata = c(
      list(
        class    = "piplog",
        log_name = name,
        saved_at = Sys.time()
      ),
      metadata
    ),
    code   = code,
    format = format,
    ...
  )

  invisible(out)
}


#' Load a log from disk
#'
#' Loads a previously saved piplog from disk using {stamp}, optionally under a
#' different name.
#'
#' @param dir Directory where the log is stored.
#' @param id File identifier (without extension). Defaults to `name`.
#' @param name Name to assign to the log in memory (default: `id`).
#' @param version Optional version identifier passed to `stamp::st_load()`.
#'   Use `"available"` to list available versions.
#' @param format File format (default: "qs2").
#' @param overwrite Logical: whether to overwrite an existing log in
#'   `.piplogenv`. Default is FALSE.
#' @param verbose Logical: whether to announce loading progress.
#'
#' @return Invisibly returns the name of the loaded log.
#' @export
log_load <- function(
    dir,
    id,
    name     = id,
    version  = NULL,
    format   = "qs2",
    overwrite = FALSE,
    verbose   = TRUE
) {

  # ---- Validate directory ----
  if (missing(dir) || !fs::dir_exists(dir)) {
    cli::cli_abort("Artifact folder {.path {dir}} does not exist.")
  }

  # ---- Build path ----
  file <- fs::path(dir, id, ext = format)

  # ---- List available versions ----
  if (identical(version, "available")) {
    vr <- stamp::st_versions(file)
    if (nrow(vr) == 0) {
      cli::cli_abort("No versions found in {.path {file}}.")
    }
    vr[, vintage := (.I - 1) * -1]
    return(vr[])
  }

  # ---- Load log ----
  ver <- if (is.null(version)) "latest" else version

  if (verbose) {
    cli::cli_alert_info(
      "Loading {.path {file}} (version = {.strong {ver}})"
    )
  }


  log <- stamp::st_load(file, version = version)

  # ---- Validate object ----
  # Restore class if dropped by serialization
  if (is.data.table(log)) {
    setattr(log, "class", unique(c("piplog", class(log))))
  }

  # Final validation
  if (!inherits(log, "piplog")) {
    cli::cli_abort("File does not contain a valid {.cls piplog} object.")
  }


  # ---- Handle overwrite ----
  if (rlang::env_has(.piplogenv, name) && !isTRUE(overwrite)) {
    cli::cli_abort(
      "A log named {.field {name}} already exists in memory.
       Use {.code overwrite = TRUE} to replace it."
    )
  }

  rlang::env_poke(.piplogenv, name, log)

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
