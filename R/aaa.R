.pipenv <-  new.env(parent = emptyenv())

#' Logging environment
#'
#' This environment stores all active log files used internally by the package.
#' Each log is stored as a named object within this environment.
#'
#' @keywords internal
.piplogenv <- new.env(parent = emptyenv())
