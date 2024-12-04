#' Return the rows of the table if they exist in master file
#'
#' @param country_code Country Code
#' @param year Year
#' @param poverty_line Poverty Lines
#' @param con Connection object where master_file exists
#'
#' @return Dataframe
#' @export
#'
return_if_exists <- function(country_code, year, poverty_line, con) {
  all_args_data <- all_args(country_code, year, poverty_line)

  duckdb::duckdb_register(con, "all_args_data", all_args_data, overwrite = TRUE)

  result <- DBI::dbGetQuery(con, glue::glue("
      SELECT *
      FROM master_file mf
      INNER JOIN all_args_data ad
      ON mf.country_code = ad.country_code
      AND mf.reporting_year = ad.reporting_year
      AND mf.poverty_line = ad.poverty_line
      "))
  #calculate the arguments that are remaining and return for the pip call
  return(result)
}

#' Create a dataframe with all possible combinations of `country_code`, `reporting_year` and `poverty_line`
#'
#' @param country_code Code of countries to be expanded
#' @param reporting_year Reported year(s)
#' @param poverty_line Poverty Line(s)
#'
#' @return A dataframe
#'
all_args <- function(country_code, reporting_year, poverty_line) {
  expand.grid(country_code = country_code, reporting_year = reporting_year, poverty_line = poverty_line)
}
