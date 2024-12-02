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
  return(result)
}

all_args <- function(country_code, reporting_year, poverty_line) {
  expand.grid(country_code = country_code, reporting_year = reporting_year, poverty_line = poverty_line)
}
