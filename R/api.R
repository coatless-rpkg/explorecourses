#' Make API request to ExploreCourses endpoint
#'
#' @param url API endpoint URL
#' @return Raw response content or error
#' @keywords internal
#' @include constants.R
make_api_request <- function(url) {
  tryCatch({
    req <- httr2::request(url) |>
      httr2::req_perform()

    if (httr2::resp_status(req) != 200) {
      cli::cli_abort(c(
        "API request failed",
        "i" = "Status code: {httr2::resp_status(req)}",
        "i" = "Endpoint: {url}"
      ))
    }

    xml2::read_xml(httr2::resp_body_string(req))
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to make API request",
      "x" = "{conditionMessage(e)}",
      "i" = "Endpoint: {url}"
    ))
  })
}

#' Fetch raw department data from API
#'
#' @return Raw XML content
#' @keywords internal
fetch_departments_raw <- function() {
  make_api_request(DEPARTMENTS_ENDPOINT)
}

#' Fetch raw course data for a department from API
#'
#' @param name Department code
#' @param year Academic year in format YYYYYYYY or NULL for current year
#' @return Raw XML content
#' @keywords internal
fetch_department_courses_raw <- function(name, year = NULL) {
  year <- validate_academic_year(year)
  url <- glue::glue(COURSE_ENDPOINT, year = year, name = name)
  make_api_request(url)
}
