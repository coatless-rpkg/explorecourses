#' Fetch department list from Stanford ExploreCourses
#'
#' @param cache_dir Base cache directory
#' @return A data frame containing department information
#' @export
#' @include constants.R
fetch_departments <- function(cache_dir = NULL) {
  req <- httr2::request(DEPARTMENTS_ENDPOINT) |>
    httr2::req_perform()

  xml_data <- req |>
    httr2::resp_body_string() |>
    xml2::read_xml()

  schools <- xml2::xml_find_all(xml_data, "//school")

  departments <- purrr::map_dfr(schools, function(school) {
    school_name <- xml2::xml_attr(school, "name")
    deps <- xml2::xml_find_all(school, ".//department")

    purrr::map_dfr(deps, function(dep) {
      tibble::tibble(
        name = xml2::xml_attr(dep, "name"),
        longname = xml2::xml_attr(dep, "longname"),
        school = school_name
      )
    })
  })

  if (!is.null(cache_dir)) {
    write_json_cache(departments, cache_dir, "departments")
  }

  departments
}


#' Fetch courses for a specific department with progress reporting
#'
#' @param name Department code
#' @param cache_dir Base cache directory
#' @param p Progress handler
#' @return Data frame of parsed course information
#' @export
fetch_department_courses <- function(name, cache_dir = NULL, p = NULL) {
  if (!is.null(p)) p(message = sprintf("Fetching %s", name))

  url <- glue::glue(COURSE_ENDPOINT, name = name)

  req <- httr2::request(url) |>
    httr2::req_perform()

  content <- httr2::resp_body_string(req)

  if (!is.null(cache_dir)) {
    write_xml_cache(content, cache_dir, name)
  }

  # Parse XML directly to data frame
  courses <- parse_courses(content)
  courses$department <- name

  courses
}

#' Process a single department
#'
#' @param name Department code
#' @param cache_dir Base cache directory
#' @param p Progress handler
#' @return Data frame of parsed course information
#' @keywords internal
process_department <- function(name, cache_dir = NULL, p = NULL) {
  if (!is.null(p)) p(message = sprintf("Fetching %s", name))

  url <- glue::glue(COURSE_ENDPOINT, name = name)

  # Try to fetch and parse the data
  tryCatch({
    req <- httr2::request(url) |>
      httr2::req_perform()

    content <- httr2::resp_body_string(req)

    if (!is.null(cache_dir)) {
      write_xml_cache(content, cache_dir, name)
    }

    # Parse XML directly to data frame
    courses <- parse_courses(content)
    courses$department <- name

    courses
  }, error = function(e) {
    warning(sprintf("Error processing department %s: %s", name, e$message))
    NULL
  })
}

#' Parse course XML into a data frame
#'
#' @param xml_content XML content from fetch_department_courses
#' @return A list of data frames containing course information
#' @export
parse_courses <- function(xml_content) {
  xml_data <- xml2::read_xml(xml_content)
  courses <- xml2::xml_find_all(xml_data, "//course")

  course_data <- purrr::map_dfr(courses, function(course) {
    # Basic course info
    basic_info <- tibble::tibble(
      objectID = xml2::xml_text(xml2::xml_find_first(course, ".//courseId")),
      year = xml2::xml_text(xml2::xml_find_first(course, ".//year")),
      subject = xml2::xml_text(xml2::xml_find_first(course, ".//subject")),
      code = xml2::xml_text(xml2::xml_find_first(course, ".//code")),
      title = xml2::xml_text(xml2::xml_find_first(course, ".//title")),
      description = xml2::xml_text(xml2::xml_find_first(course, ".//description")),
      units_min = as.numeric(xml2::xml_text(xml2::xml_find_first(course, ".//unitsMin"))),
      units_max = as.numeric(xml2::xml_text(xml2::xml_find_first(course, ".//unitsMax")))
    )

    # Get sections
    sections <- xml2::xml_find_all(course, ".//section")
    section_data <- purrr::map_dfr(sections, function(section) {
      section_info <- tibble::tibble(
        objectID = basic_info$objectID,
        term = xml2::xml_text(xml2::xml_find_first(section, ".//term")),
        term_id = xml2::xml_text(xml2::xml_find_first(section, ".//termId")),
        section_number = xml2::xml_text(xml2::xml_find_first(section, ".//sectionNumber")),
        component = xml2::xml_text(xml2::xml_find_first(section, ".//component")),
        class_id = xml2::xml_text(xml2::xml_find_first(section, ".//classId")),
        current_size = as.numeric(xml2::xml_text(xml2::xml_find_first(section, ".//currentClassSize"))),
        max_size = as.numeric(xml2::xml_text(xml2::xml_find_first(section, ".//maxClassSize")))
      )

      # Get schedules
      schedules <- xml2::xml_find_all(section, ".//schedule")
      schedule_data <- purrr::map_dfr(schedules, function(schedule) {
        tibble::tibble(
          section_id = section_info$class_id,
          days = xml2::xml_text(xml2::xml_find_first(schedule, ".//days")),
          start_time = xml2::xml_text(xml2::xml_find_first(schedule, ".//startTime")),
          end_time = xml2::xml_text(xml2::xml_find_first(schedule, ".//endTime")),
          location = xml2::xml_text(xml2::xml_find_first(schedule, ".//location"))
        )
      })

      # Join schedules to section
      if (nrow(schedule_data) > 0) {
        section_info <- dplyr::left_join(
          section_info,
          schedule_data,
          by = c("class_id" = "section_id")
        )
      }

      section_info
    })

    # Join sections to basic info
    if (nrow(section_data) > 0) {
      basic_info <- dplyr::left_join(
        basic_info,
        section_data,
        by = "objectID"
      )
    }

    basic_info
  })

  course_data
}

#' Fetch courses for a specific department with progress reporting
#'
#' @param name Department code
#' @param cache_dir Base cache directory
#' @param p Progress handler
#' @return Data frame of parsed course information
#' @export
fetch_department_courses <- function(name, cache_dir = NULL, p = NULL) {
  process_department(name, cache_dir, p)
}

#' Fetch and process courses for multiple departments in parallel
#'
#' @param departments Character vector of department codes
#' @param cache_dir Base cache directory
#' @return A data frame containing course information
#' @export
fetch_all_courses <- function(departments = NULL, cache_dir = NULL) {
  if (is.null(departments)) {
    departments <- fetch_departments(cache_dir)$name
  }

  p <- progressr::progressor(steps = length(departments))

  results <- future.apply::future_lapply(
    departments,
    fetch_department_courses,
    cache_dir = cache_dir,
    p = p,
    future.seed = TRUE
  )

  # Combine results
  dplyr::bind_rows(results)
}
