#' Generate academic year string from start year
#'
#' Converts a single starting year into a Stanford academic year format by combining
#' it with the following year. Takes a year (`YYYY`) and returns the full academic
#' year string (`YYYYYYYY`).
#'
#' @param start_year Numeric or character. Starting year as `YYYY` (e.g., `2023`).
#'   If provided as character, will be converted to integer.
#'
#' @return
#' Character string. Eight-digit academic year in format `YYYYYYYY` where
#' the first four digits are the start year and the last four digits are
#' the start year plus one.
#'
#' @seealso
#'
#' - [validate_academic_year()] for validating academic year strings
#' - [fetch_department_courses()] for using academic years in course queries
#'
#' @details
#' The function:
#'
#' 1. Converts the input year to an integer
#' 2. Creates an 8-digit string by combining the start year with (start year + 1)
#' 3. Ensures proper zero-padding of years using `sprintf()`
#'
#' For example:
#'
#' - 2023 -> "20232024"
#' - 2024 -> "20242025"
#'
#' @examples
#' generate_academic_year(2023)  # Returns "20232024"
#' generate_academic_year(2024)  # Returns "20242025"
#' generate_academic_year("2025")  # Returns "20252026"
#'
#' @export
generate_academic_year <- function(start_year) {
  start_year <- as.integer(start_year)
  sprintf("%04d%04d", start_year, start_year + 1)
}

#' Validate Stanford academic year format
#'
#' Validates that an academic year string meets Stanford's format requirements and
#' constraints. Checks both the string format and the logical relationship between
#' start and end years. Also enforces historical data boundaries.
#'
#' @param year Character string or NULL. Academic year to validate:
#'   - If `character`: Must be in format `YYYYYYYY` (e.g., `"20232024"`)
#'   - If `NULL`: Represents current academic year
#'
#' @return
#' Character string. Returns:
#'
#' - Empty string (`""`) if input is `NULL`
#' - Original year string if validation passes
#'
#'
#' @seealso
#'
#' - [generate_academic_year()] for creating valid academic year strings
#' - [fetch_department_courses()] for using validated years in queries
#'
#' @details
#' The function performs several validation checks:
#'
#' 1. Format validation:
#'    - Must be exactly 8 digits (YYYYYYYY)
#'    - Must contain only numeric characters
#'
#' 2. Logical validation:
#'    - End year must be exactly one year after start year
#'    - Start year must not be before 1913 (earliest course data)
#'
#' 3. Special handling:
#'    - NULL input returns "" (representing current academic year)
#'
#' The function provides detailed error messages when validation fails,
#' indicating the specific validation rule that was violated.
#'
#' @section Error Messages:
#'
#' The function may throw errors for:
#'
#' - Invalid format (not 8 digits)
#' - Invalid year progression (end year != start year + 1)
#' - Start year before 1913
#'
#' Each error includes:
#'
#' - Description of the problem
#' - Expected format/rules
#' - Received invalid value
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_academic_year("20232024")  # Returns "20232024"
#' validate_academic_year(NULL)        # Returns ""
#' validate_academic_year("2023")      # Error: Invalid format
#' validate_academic_year("20232025")  # Error: Invalid year progression
#' validate_academic_year("19121913")  # Error: Start year too early
#' }
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
