#' check if package is loaded
#'
#' @inheritParams check_pkg_active
#'
#' @return invisible logical
#' @keywords internal
is_package_loaded <- function(pkg) {
  is_it <- pkg %in% loadedNamespaces()
  invisible(is_it)
}

#' check if package is loaded
#'
#' @inheritParams check_pkg_active
#'
#' @return  invisible logical
#' @keywords internal
is_package_attached <- function(pkg) {
  is_it <- paste("package", pkg, sep = ":") %in% search()
  invisible(is_it)
}

#' Check if a package is loaded or attached
#'
#' @param pkg character: Name of the package
#'
#' @return invisible(TRUE)
#' @export
#'
#' @examples
#' \dontrun{
#' check_pkg_active
#' }
check_pkg_active <- function(pkg) {
  if (!is_package_loaded(pkg) && !is_package_attached(pkg)) {
    cli::cli_abort("The {.pkg {pkg}} package is not loaded or attached.
                   Please load or attach it with {.run library({pkg})} or
                   {.code devtools::load_all()}.")
  } else {
    cli::cli_inform("The {.pkg {pkg}} package is active.")
  }
  invisible(TRUE)
}

