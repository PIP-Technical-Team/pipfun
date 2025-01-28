# Getter function
#' Title
#'
#' @return
#' @export
#'
#' @examples
get_pipenv <- function() {
  .pipenv
}

#' Get from .pipenv
#'
#' @param key
#'
#' @return
#' @export
#'
#' @examples
get_from_pipenv <- function(key) {
  get(key, envir = .pipenv)
}

# Setter function
#' Title
#'
#' @param key
#' @param value
#'
#' @return
#' @export
#'
#' @examples
set_in_pipenv <- function(key, value) {
  assign(key, value, envir = .pipenv)
}
