#' Fetch department list from Stanford ExploreCourses
#'
#' Retrieves a list of all departments from Stanford ExploreCourses, either from
#' cache if available or from the API. The function handles caching automatically
#' when a cache directory is specified.
#'
#' @param cache_dir Character string. Optional path to cache directory. If provided,
#'   the function will:
#'   - Try to read existing department data from cache
#'   - Cache fresh API data when cache miss occurs
#'   If `NULL`, always fetches fresh data from the API.
#'
#' @return
#' A tibble (data frame) containing department information with columns:
#'
#' - `name`: Department code (e.g., `"CS"`)
#' - `longname`: Full department name (e.g., `"Computer Science"`)
#' - `school`: School name (e.g., `"School of Engineering"`)
#'
#' @details
#' The function follows this process:
#'
#' 1. If `cache_dir` provided:
#'    - Attempts to read cached department data
#'    - On cache miss, proceeds to API fetch
#' 2. API fetch (when needed):
#'    - Requests department list from ExploreCourses API
#'    - Caches response if `cache_dir` provided
#' 3. Data processing:
#'    - Parses XML response into structured data frame
#'    - Organizes departments by school
#'
#' @section Error Handling:
#'
#' - If API request fails or returns invalid data. Includes specific error message
#' and any relevant details.
#' - If cache operations fail when `cache_dir` is provided. Includes cache path
#' and specific error message.
#'
#' @seealso
#'
#' - [fetch_department_courses()] for fetching courses for a specific department
#' - [process_departments_xml()] for details on XML processing
#' - [read_xml_cache()] and [write_xml_cache()] for cache operations
#'
#' @examples
#' \dontrun{
#' # Fetch fresh data from API (no caching)
#' departments <- fetch_departments()
#'
#' # Fetch with caching enabled
#' departments <- fetch_departments(cache_dir = "course_cache")
#'
#' # Example data processing
#' # Find all engineering departments
#' eng_deps <- subset(departments, school == "School of Engineering")
#'
#' # Get department codes
#' dept_codes <- departments$name
#' }
#'
#' @export
fetch_departments <- function(cache_dir = NULL) {
  tryCatch({
    # Try to read from cache first
    xml_content <- read_xml_cache(
      name = "departments",
      cache_dir = cache_dir
    )

    # If cache miss, fetch from API
    if (is.null(xml_content)) {
      xml_content <- fetch_departments_raw()

      # Cache the fresh data if cache_dir is specified
      if (!is.null(cache_dir)) {
        write_xml_cache(
          xml_doc = xml_content,
          name = "departments",
          cache_dir = cache_dir
        )
      }
    }

    # Process the XML content
    process_departments_xml(xml_content)

  }, error = function(e) {
    if (inherits(e, "explorecourses_error")) {
      stop(e)
    }

    # For unexpected errors
    cli::cli_abort(
      c("Failed to fetch departments",
        "x" = "{conditionMessage(e)}",
        if (!is.null(cache_dir)) c("i" = "Cache directory: {.file {cache_dir}}")
       )
    )
  })
}

#' Fetch courses for a specific department
#'
#' Retrieves all courses for a specified department from Stanford ExploreCourses,
#' either from cache if available or from the API. Supports fetching courses for
#' specific academic years and handles caching automatically when a cache directory
#' is specified.
#'
#' @param name Character string. Department code (e.g., `"CS"`). Must be a single,
#'   non-empty string. If multiple codes provided, only the first is used.
#' @param year Character string. Optional academic year in `YYYYYYYY` format (e.g.,
#'   `"20232024"`). If `NULL`, fetches data for the current academic year.
#' @param cache_dir Character string. Optional path to cache directory. If provided,
#'   the function will:
#'   - Try to read existing course data from cache
#'   - Cache fresh API data when cache miss occurs
#'   If `NULL`, always fetches fresh data from the API.
#'
#' @return
#' A tibble (data frame) containing course information with columns:
#'
#' - `objectID`: Unique course identifier
#' - `year`: Academic year
#' - `subject`: Department code
#' - `code`: Course number
#' - `title`: Course title
#' - `description`: Course description
#' - `units_min`: Minimum units
#' - `units_max`: Maximum units
#' - `department`: Department code of the course
#' - `term`: Academic term (e.g., "Autumn", "Winter")
#' - `term_id`: Numeric term identifier
#' - `section_number`: Section identifier
#' - `component`: Course component type (e.g., "LEC", "DIS", "LAB")
#' - `class_id`: Unique identifier for the section
#' - `current_class_size`: Current enrollment
#' - `max_class_size`: Maximum enrollment capacity
#' - Schedule information:
#'   - `days`: Days of the week (e.g., "Mon, Wed, Fri")
#'   - `start_time`: Start time of the class
#'   - `end_time`: End time of the class
#'   - `location`: Class location/room
#' - Instructor information:
#'   - `instructors`: Combined instructor information with roles
#'   - `instructor_names`: Names of instructors
#'   - `instructor_sunets`: SUNet IDs of instructors
#'   - `instructor_roles`: Roles of instructors
#'
#' Returns `NULL` if no courses found.
#'
#' @details
#' The function follows this process:
#'
#' 1. Input validation:
#'    - Ensures single department code
#'    - Validates code format and content
#' 2. Data retrieval:
#'    - Checks cache if `cache_dir` provided
#'    - Fetches from API on cache miss
#'    - Caches new data if applicable
#' 3. Data processing:
#'    - Parses XML into structured format
#'    - Validates course data exists
#'
#' @section Warning handlers:
#'
#' Warning message will be shown if:
#'
#' - Multiple department codes provided
#' - No courses found for department
#'
#' @section Error Handling:
#'
#' - Invalid department code (empty or non-string)
#' - API fetch failures
#' - Cache operation failures
#' - XML processing errors
#'
#' @seealso
#'
#' - [fetch_departments()] for getting list of valid department codes
#' - [fetch_all_courses()] for fetching courses from multiple departments
#' - [validate_academic_year()] for year format details
#'
#' @examples
#' \dontrun{
#' # Fetch current year's CS courses
#' cs_courses <- fetch_department_courses("CS")
#'
#' # Fetch specific academic year
#' cs_courses_2023 <- fetch_department_courses(
#'   name = "CS",
#'   year = "20232024"
#' )
#'
#' # Fetch with caching enabled
#' cs_courses <- fetch_department_courses(
#'   name = "CS",
#'   cache_dir = "course_cache"
#' )
#'
#' # Handle empty results
#' courses <- fetch_department_courses("DEPT")
#' if (!is.null(courses)) {
#'   # Process courses
#' }
#'
#' # Find all courses taught by a specific instructor
#' cs_courses %>%
#'   dplyr::filter(grepl("Smith", instructor_names))
#'
#' # Get all lecture sections
#' lectures <- cs_courses %>%
#'   dplyr::filter(component == "LEC")
#' }
#'
#' @export
fetch_department_courses <- function(name, year = NULL, cache_dir = NULL) {
  # Input validation
  if (length(name) > 1) {
    cli::cli_warn(c(
      "Multiple department codes provided",
      "i" = "Using first code: {.val {name[1]}}",
      "i" = "Ignoring: {.val {paste(name[-1], collapse = ', ')}}"
    ))
    name <- name[1]
  }

  if (is.na(name) || !is.character(name) || nchar(name) == 0) {
    cli::cli_abort(
      c("Invalid department code",
        "x" = "Department code must be a non-empty string")
    )
  }

  tryCatch({
    # Check cache first
    xml_content <- read_xml_cache(name, year, cache_dir)

    if (is.null(xml_content)) {
      # Fetch from API if not in cache
      xml_content <- fetch_department_courses_raw(name, year)
      # Cache the response
      write_xml_cache(xml_content, name, year, cache_dir)
    }

    # Process data
    courses <- process_courses_xml(xml_content, name)

    if (is.null(courses) || nrow(courses) == 0) {
      cli::cli_warn(c(
        "No courses found",
        "i" = "Department: {.val {name}}",
        "i" = "Year: {.val {year %||% 'current'}}"
      ))
      return(NULL)
    }

    courses
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to process department data",
      "i" = "Department: {.val {name}}",
      "i" = "Year: {.val {year %||% 'current'}}",
      "x" = "Error: {conditionMessage(e)}"
    ))
  })
}

#' Fetch and process courses for multiple departments in parallel
#'
#' Retrieves courses from multiple departments concurrently using parallel processing.
#' If no departments are specified, fetches courses from all available departments.
#' Provides progress updates during processing and handles errors gracefully.
#'
#' @param departments Character vector. Department codes to fetch (e.g., `c("CS", "MATH")`).
#'   If `NULL`, fetches from all available departments.
#' @param year Character string. Optional academic year in `YYYYYYYY` format (e.g.,
#'   `"20232024"`). If `NULL`, fetches data for the current academic year.
#' @param cache_dir Character string. Optional path to cache directory. If provided,
#'   enables caching for all department fetches.
#'
#' @return
#' A tibble (data frame) containing course information with columns:
#'
#' - `objectID`: Unique course identifier
#' - `year`: Academic year
#' - `subject`: Department code
#' - `code`: Course number
#' - `title`: Course title
#' - `description`: Course description
#' - `units_min`: Minimum units
#' - `units_max`: Maximum units
#' - `department`: Department code of the course
#' - `term`: Academic term (e.g., "Autumn", "Winter")
#' - `term_id`: Numeric term identifier
#' - `section_number`: Section identifier
#' - `component`: Course component type (e.g., "LEC", "DIS", "LAB")
#' - `class_id`: Unique identifier for the section
#' - `current_class_size`: Current enrollment
#' - `max_class_size`: Maximum enrollment capacity
#' - Schedule information:
#'   - `days`: Days of the week (e.g., "Mon, Wed, Fri")
#'   - `start_time`: Start time of the class
#'   - `end_time`: End time of the class
#'   - `location`: Class location/room
#' - Instructor information:
#'   - `instructors`: Combined instructor information with roles
#'   - `instructor_names`: Names of instructors
#'   - `instructor_sunets`: SUNet IDs of instructors
#'   - `instructor_roles`: Roles of instructors
#'
#' Returns `NULL` if no courses found for any department.
#'
#' @details
#' The function follows this process:
#'
#' 1. Input preparation:
#'    - Validates year format if provided
#'    - Fetches all departments if none specified
#'    - Cleans department list (removes duplicates and invalid codes)
#' 2. Parallel processing:
#'    - Uses `furrr` for parallel execution
#'    - Shows progress updates via `progressr`
#'    - Safely handles errors per department
#' 3. Results handling:
#'    - Reports any department-specific errors
#'    - Combines successful results
#'    - Returns `NULL` if no valid results
#'
#' Progress updates show:
#'
#' - Current department being processed
#' - Extraction of results
#' - Final merging step
#'
#' @section Error handling:
#' This function will trigger errors for:
#'
#' - Department-specific processing errors
#' - No courses found across all departments
#' - Year validation issues
#'
#' @seealso
#'
#' - [fetch_department_courses()] for single department fetching
#' - [fetch_departments()] for getting department codes
#' - [validate_academic_year()] for year format details
#'
#' @examples
#' \dontrun{
#' # Fetch all departments
#' all_courses <- fetch_all_courses()
#'
#' # Fetch specific departments
#' stem_courses <- fetch_all_courses(
#'   departments = c("CS", "MATH", "PHYSICS")
#' )
#'
#' # Fetch with year and caching
#' courses_2023 <- fetch_all_courses(
#'   year = "20232024",
#'   cache_dir = "course_cache"
#' )
#'
#' # Process results with dplyr
#' library(dplyr)
#'
#' # Count courses per department
#' dept_summary <- all_courses %>%
#'   group_by(department) %>%
#'   summarise(
#'     n_courses = n_distinct(objectID),
#'     n_sections = n()
#'   )
#'
#' # Find large classes
#' large_classes <- all_courses %>%
#'   filter(current_class_size >= 100) %>%
#'   select(department, code, title, current_class_size)
#' }
#'
#' @export
fetch_all_courses <- function(departments = NULL, year = NULL, cache_dir = NULL) {
  # Input validation
  if (!is.null(year)) {
    year <- validate_academic_year(year)
  }

  if (is.null(departments)) {
    departments <- fetch_departments(cache_dir)$name
  }

  # Ensure departments is a character vector and remove duplicates
  departments <- unique(as.character(departments))

  # Remove any NA or invalid department codes
  departments <- departments[!is.na(departments) & nchar(departments) > 0]

  # Set up progress reporting
  p <- progressr::progressor(steps = length(departments))

  # Process departments in parallel using furrr
  results <- furrr::future_map(
    departments,
    purrr::safely(function(dept) {
      if (!is.null(p)) p(sprintf("Processing %s", dept))
      fetch_department_courses(dept, year, cache_dir)
    }),
    .options = furrr::furrr_options(seed = TRUE)
  )

  p("Extracting viable results...")

  # Extract successful results and errors
  successes <- purrr::map(results, "result")
  errors <- purrr::map(results, "error")

  # Report any errors
  error_indices <- which(!purrr::map_lgl(errors, is.null))
  if (length(error_indices) > 0) {
    for (i in error_indices) {
      error_obj <- errors[[i]]
      if (inherits(error_obj, "error")) {
        cli::cli_warn(
          c("Error processing department",
            "x" = "{conditionMessage(error_obj)}",
            "i" = "Department: {.val {departments[i]}}")
        )
      } else {
        cli::cli_warn(
          c("Error processing department",
            "x" = "Unknown error occurred",
            "i" = "Department: {.val {departments[i]}}")
        )
      }
    }
  }

  p("Merging results...")

  # Keep only successful results
  results <- purrr::compact(successes)

  # Combine results
  if (length(results) > 0) {
    final_df <- dplyr::bind_rows(results)
    return(final_df)
  } else {
    cli::cli_warn(
      c("No course data retrieved",
        "i" = "Year: {.val {year %||% 'current'}}")
    )
    return(NULL)
  }
}
