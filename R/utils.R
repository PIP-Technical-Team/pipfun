#' Get all arguments of calling function
#'
#' Calling function is [parent.frame]
#'
#' @return arguments of calling function as a list
#' @export
#'
#' @examples
#' foo <- function(x, y = 2, ...) {
#'   all_args()
#' }
#' foo(x = 1, y = 3)
#' foo(x = 1, z = 123)
#' foo(x = 1, z = "valid")
#' foo(x = 1, a = TRUE)
all_args <- function() {
  # Capture the call of the calling function
  call <- match.call(definition = sys.function(-1),
                     call = sys.call(-1),
                     expand.dots = FALSE)

  # Convert the call to a list of arguments
  call_args <- as.list(call)[-1]  # Remove function name

  # Evaluate each argument in the parent frame to resolve variable references
  call_args <- lapply(call_args, eval, envir = parent.frame())

  # Evaluate `...` separately and merge into the list
  if ("..." %in% names(call_args)) {
    dots <- eval(call_args[["..."]], envir = parent.frame())
    call_args <- c(call_args[setdiff(names(call_args), "...")], dots)
  }

  return(call_args)
}


#' get most recent version of PPP
#'
#' THis data is extracted from the `aux_ppp` repo in branch DEV_V2
#'
#' @return data frame with all PPP years and versions available
#' @export
#'
#' @examples
#' get_ppp_versions()
get_ppp_versions <- function() {

  get_file_from_gh(repo   = "aux_ppp",
                   branch = "DEV_v2",
                   file_path = "ppp_vintage.csv")

}

#' @rdname get_ppp_versions
#' @inheritParams new_pip_release
#' @return data frame with most recent versions for `ppps` selected
#' @export
get_latest_ppp_versions <- function(ppps = getOption("pipfun.ppps")) {

  pppm <- get_ppp_versions()

  pppmf <- pppm[ppp_year %in% ppps]
  if (nrow(pppmf) == 0)
    cli::cli_abort(c("{as.character(ppps)} {?is/are} not PPP
                       year{?s} available",
                     i = "Years available: {unique(pppm$ppp_year)}"))

  pppmf <- pppmf[,
                 .SD[which.max(ppp_av)],
                 by = .(ppp_year, ppp_rv)
  ][,
    .SD[which.max(ppp_rv)],
    by = ppp_year
  ]
  pppmf
}


#' Constructor of secret.
#'
#' Right from the Advanced R Book
#'
#' @param x any vector
#'
#' @returns x with class "secrete"
#' @export
#'
#' @examples
#' y <- new_secrete("284092sdhfjsld")
#' y
new_secret <- function(x = double()) {
  structure(x, class = "secret")
}


#' S# method for secret
#'
#' @param x secret vector
#' @param ... other arguments
#'
#' @returns x invisible
#' @export
print.secret <- function(x, ...) {
  print(strrep("x", (nchar(x) + 1)))
  invisible(x)
}



##' Print method for pip_boards S3 class
##'
##' Displays a summary of PIP pins boards in a beautiful cli output.
##' Shows the common directory path, each board's name, its relative path, and cache size.
##'
##' @param x A `pip_boards` object, as returned by `set_pip_boards()`.
##' @param ... Additional arguments (ignored).
##'
##' @return Invisibly returns the input `pip_boards` object.
##'
##' @examples
##' boards <- set_pip_boards()
##' print(boards)
##'
##' @export
print.pip_boards <- function(x, ...) {
  stopifnot(inherits(x, "pip_boards"))
  # Extract all paths
  paths <- vapply(x, function(board) as.character(board$path), character(1))
  # Find common path prefix
  split_paths <- strsplit(paths, "[/\\]")
  min_len <- min(vapply(split_paths, length, integer(1)))
  common <- character()
  for (i in seq_len(min_len)) {
    elems <- vapply(split_paths, function(p) p[i], character(1))
    if (length(unique(elems)) == 1) {
      common <- c(common, elems[1])
    } else break
  }
  common_path <- paste(common, collapse = "/")
  # Prepare board info
  board_info <- lapply(names(x), function(nm) {
    board <- x[[nm]]
    rel_path <- sub(paste0("^", common_path), "", as.character(board$path))
    rel_path <- sub("^[/\\]", "", rel_path)
    cache_size <- tryCatch({
      cache <- board$cache
      if (is.character(cache) && !is.na(cache) && dir.exists(cache)) {
        size <- sum(file.info(list.files(cache, full.names = TRUE, recursive = TRUE))$size)
        format(size, big.mark = ",", scientific = FALSE)
      } else {
        "0"
      }
    }, error = function(e) "0")
    list(name = nm, rel_path = rel_path, cache_size = cache_size)
  })
  # cli output
  cli::cli_h1("PIP Pins Boards")
  cli::cli_text("Common path: {.path {common_path}}\n")
  cli::cli_div(theme = list(span.board = list(color = "cyan", font_weight = "bold")))
  cli::cli_ul()
  for (info in board_info) {
    cli::cli_li()
    cli::cli_text("{.board {info$name}}: {.path {info$rel_path}} | Cache size: {.field {info$cache_size}} bytes")
  }
  cli::cli_end()
  invisible(x)
}


