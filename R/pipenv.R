# Getter function: Returns the entire .pipenv environment
#' Get the entire .pipenv environment
#'
#' @return The .pipenv environment
#' @export
#'
#' @examples
#' env <- get_pipenv()
get_pipenv <- function() {
  .pipenv
}

# Getter for a specific key from .pipenv
#' Get a value from .pipenv
#'
#' @param key A character string representing the key
#'
#' @return The value associated with the key in .pipenv
#' @export
#'
#' @examples
#' set_in_pipenv("example_key", 42)
#' get_from_pipenv("example_key") # returns 42
get_from_pipenv <- function(key) {
  rlang::env_get(.pipenv, key, default = NULL) # Returns NULL if key doesn't exist
}

# Setter function: Assign a value in .pipenv
#' Set a value in .pipenv
#'
#' @param key A character string representing the key
#' @param value The value to store in .pipenv
#'
#' @return The assigned value (invisibly)
#' @export
#'
#' @examples
#' set_in_pipenv("example_key", 42)
set_in_pipenv <- function(key, value) {
  rlang::env_poke(.pipenv, key, value)
  invisible(value)  # Return value invisibly to avoid clutter in console
}
