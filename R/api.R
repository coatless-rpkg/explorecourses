#' Make HTTP request to Stanford ExploreCourses API endpoint
#'
#' Makes an HTTP GET request to a Stanford ExploreCourses API endpoint and returns
#' the response as an XML document. Handles response validation and error cases.
#'
#' @param url Character string. Complete URL for the ExploreCourses API endpoint.
#'   Expected to be one of:
#'   - Departments endpoint: `DEPARTMENTS_ENDPOINT`
#'   - Course search endpoint: `COURSE_ENDPOINT` with parameters
#'
#' @return
#' An [xml2::xml_document-class] object containing the parsed API response.
#' The structure depends on the specific endpoint called.
#'
#' @seealso
#'
#' - [httr2::request()] for the underlying HTTP request functionality
#' - [xml2::read_xml()] for XML parsing
#'
#' @details
#' The function performs these steps:
#'
#' 1. Makes HTTP GET request using `httr2`
#' 2. Validates HTTP response status (must be 200)
#' 3. Converts response body to XML document
#'
#' Error handling covers:
#'
#' - Network/connection failures
#' - Non-200 HTTP status codes
#' - XML parsing errors
#'
#' All errors are converted to standardized error messages with
#' endpoint information and original error details.
#'
#' @section Rate Limiting:
#' The ExploreCourses API does not currently require authentication or impose
#' rate limits, but considerate usage is recommended.
#'
#' @section Response Format:
#' The API returns XML data in the ExploreCourses schema version 20140630.
#' The exact structure varies by endpoint:
#'
#' - Departments: List of schools containing departments
#' - Courses: List of courses with sections and schedules
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Get departments list
#' deps_xml <- make_api_request(DEPARTMENTS_ENDPOINT)
#'
#' # Get courses for CS department
#' courses_url <- sprintf(COURSE_ENDPOINT, year = "20232024", name = "CS")
#' cs_xml <- make_api_request(courses_url)
#' }
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

#' Fetch raw department list from Stanford ExploreCourses API
#'
#' Makes a direct API call to retrieve the complete list of Stanford departments
#' and their associated schools. Returns the raw XML response without any
#' processing or transformation.
#'
#' @details
#' This function is a thin wrapper around [make_api_request()] specifically for
#' the departments endpoint. It retrieves the current academic year's department
#' structure including:
#'
#' - Schools
#' - Departments within each school
#' - Department codes and full names
#'
#' The department list is relatively static, changing only when Stanford's
#' organizational structure changes.
#'
#' @return
#' An [xml2::xml_document-class] object containing the raw departments XML.
#'
#' The XML structure includes:
#'
#' - Root `schools` element
#' - Multiple `school` elements, each containing:
#'   - `@name` attribute: School name
#'   - Multiple `department` elements, each containing:
#'     - `@name` attribute: Department code
#'     - `@longname` attribute: Full department name
#'
#' @seealso
#'
#' - [make_api_request()] for the underlying API request functionality
#' - [process_departments_xml()] for processing this XML data
#' - [fetch_departments()] for the high-level interface
#'
#' @section API Endpoint:
#' Uses the `DEPARTMENTS_ENDPOINT` constant, which points to:
#' `https://explorecourses.stanford.edu/?view=xml-20140630`
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' xml_doc <- fetch_departments_raw()
#' schools <- xml2::xml_find_all(xml_doc, "//school")
#' }
fetch_departments_raw <- function() {
  make_api_request(DEPARTMENTS_ENDPOINT)
}

#' Fetch raw course data for a department from Stanford ExploreCourses API
#'
#' Makes a direct API call to retrieve all courses for a specific department and
#' academic year from Stanford's ExploreCourses system. Returns the raw XML
#' response without processing.
#'
#' @param name Character string. Department code (e.g., `"CS"` for Computer Science).
#'   Must be a valid Stanford department code.
#' @param year Character string or `NULL`. Academic year in format `YYYYYYYY`
#'   (e.g., `"20232024"`). If `NULL`, defaults to current academic year.
#'   Will be validated using [validate_academic_year()].
#'
#' @return
#' An [xml2::xml_document-class] object containing the raw courses XML.
#' The XML structure includes:
#'
#'   - Root `courses` element containing multiple `course` elements
#'   - Each `course` element contains:
#'     - Basic course information (ID, title, description)
#'     - Units information
#'     - Section data
#'     - Schedule information
#'     - Instructor details
#'
#' @seealso
#'
#' - [validate_academic_year()] for year format validation
#' - [make_api_request()] for the underlying API request
#' - [process_courses_xml()] for processing this XML data
#' - [fetch_department_courses()] for the high-level interface
#'
#' @details
#' This function:
#'
#' 1. Validates the academic year format
#' 2. Constructs the API URL using `COURSE_ENDPOINT` template
#' 3. Makes the API request for course data
#'
#' The API returns all active courses for the specified department,
#' including detailed information about:
#'
#' - Course metadata (title, description, units)
#' - Sections and components
#' - Meeting schedules
#' - Instructor information
#'
#' @section API Endpoint:
#'
#' Uses the `COURSE_ENDPOINT` template, which expands to:
#'
#' ```
#' https://explorecourses.stanford.edu/search?view=xml-20140630
#'   &academicYear={year}
#'   &q={name}
#'   &filter-departmentcode-{name}=on
#'   &filter-coursestatus-Active=on
#' ```
#'
#' @section Filters:
#'
#' The API request automatically includes filters for:
#'
#' - Department code match
#' - Active courses only
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Get current year CS courses
#' cs_xml <- fetch_department_courses_raw("CS")
#'
#' # Get specific year Math courses
#' math_xml <- fetch_department_courses_raw("MATH", "20232024")
#' }
fetch_department_courses_raw <- function(name, year = NULL) {
  year <- validate_academic_year(year)
  url <- glue::glue(COURSE_ENDPOINT, year = year, name = name)
  make_api_request(url)
}
