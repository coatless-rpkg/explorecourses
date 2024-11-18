#' Generate academic year string
#'
#' @param start_year Starting year as YYYY
#' @return Academic year string in YYYYYYYY format
#' @examples
#' generate_academic_year(2023)  # Returns "20232024"
#' @export
generate_academic_year <- function(start_year) {
  start_year <- as.integer(start_year)
  sprintf("%04d%04d", start_year, start_year + 1)
}

#' Validate academic year format
#'
#' @param year Academic year in format YYYYYYYY or NULL
#' @return Validated year string or error
#' @keywords internal
validate_academic_year <- function(year) {
  if (is.null(year)) {
    return("")  # Current academic year
  }

  if (!grepl("^\\d{8}$", year)) {
    cli::cli_abort(
      c("Invalid academic year format",
        "x" = "Year must be in format YYYYYYYY (e.g., '20232024')",
        "i" = "Received: {.val {year}}"),
      value = year
    )
  }

  start_year <- as.integer(substr(year, 1, 4))
  end_year <- as.integer(substr(year, 5, 8))

  if (end_year != start_year + 1) {
    cli::cli_abort(
      c("Invalid academic year range",
        "x" = "End year must be one year after start year",
        "i" = "Start year: {.val {start_year}}",
        "i" = "End year: {.val {end_year}}")
    )
  }

  if (start_year < 1913) {  # Courses data starts in 1913
    cli::cli_abort(
      c("Invalid start year",
        "x" = "Start year cannot be before 1913 (first year of course data)",
        "i" = "Received: {.val {start_year}}")
    )
  }

  year
}
