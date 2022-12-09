#' Merge auxiliary PIP tables
#'
#' @param tables character: name of auxiliary tables available in
#'   `pipload::pip_load_aux()`
#' @param ppp_year numeric: PPP round year
#' @inheritParams pipload::pip_load_aux
#' @param ... Other arguments passed on to [base::merge()]. Yet, it actually
#'   uses the merge S3 method for data.table
#'
#' @return data.table
#' @export
#'
#' @examples
#' pip_merge_aux()
pip_merge_aux <- function(tables            = c("cpi", "ppp"),
                          ppp_year          = 2017,
                          branch            = c("DEV", "PROD", "main"),
                          root_dir          = Sys.getenv("PIP_ROOT_DIR"),
                          maindir           = pip_create_globals(root_dir)$PIP_DATA_DIR,
                          ...) {

  branch <- match.arg(branch)


#   ____________________________________________________
#   on.exit                                         ####
  on.exit({

  })

#   ____________________________________________________
#   Defenses                                        ####
  if (length(tables) < 2) {
    cli::cli_abort("Parameter {.field tables} must be two or more elements.")
  }

#   ____________________________________________________
#   Early returns                                   ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________
#   Computations                                     ####

  # load Id
  lid <- aux_ids(tables = tables)

  # load aux tables
  laux <- purrr::map(tables,
                     pipload::pip_load_aux,
                     branch            = branch,
                     root_dir          = root_dir,
                     maindir           = maindir,
                     verbose           = FALSE)
  names(laux) <- tables

  purrr::accumulate(lid, uni_int, .init = NULL)




#   ____________________________________________________
#   Return                                           ####
  return(TRUE)

}


#' list of ID variables in auxiliary tables
#'
#' @inheritParams pip_merge_aux
#'
#' @return list of ids per table
aux_ids <- function(tables = NULL) {

  l <- list()

  l[["cpi"]] <- c("country_code",
                  "survey_year",
                  "cpi_year",
                  "data_level",
                  "survey_acronym")

  l[["pfw"]] <- c("country_code",
                  "year",
                  "reporting_year",
                  "surveyid_year",
                  "survey_year",
                  "welfare_type",
                  "survey_acronym")

  l[["ppp"]]  <- c("country_code", "data_level")

  l[["gdm"]] <- c("country_code",
                  "data_level",
                  "survey_year",
                  "welfare_type")


  if (!is.null(tables)) {
    miss_tbl <- tables[!tables %in% names(l)]
    if (length(miss_tbl)) {
      cli::cli_abort("{.field {miss_tbl}} {?is/are} not part of PIP auxiliary table")
    }

    l <- l[tables]
  }



#   ____________________________________________________
#   Return                                           ####
  return(l)

}


uni_int <- function(x, y, z) {

#   ____________________________________________________
#   on.exit                                         ####
  on.exit({

  })

#   ____________________________________________________
#   Defenses                                        ####
  stopifnot( exprs = {

    }
  )

#   ____________________________________________________
#   Early returns                                   ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________
#   Computations                                     ####
  u <- c(x, y) |>
    unique() |>
    intersect(z)


#   ____________________________________________________
#   Return                                           ####
  return(u)

}
