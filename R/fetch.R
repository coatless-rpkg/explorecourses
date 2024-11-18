#' Fetch department list from Stanford ExploreCourses
#'
#' @param cache_dir Directory to cache results
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
    fs::dir_create(cache_dir)
    jsonlite::write_json(
      departments,
      fs::path(cache_dir, "departments.json"),
      pretty = TRUE
    )
  }

  departments
}

#' Fetch courses for a specific department
#'
#' @param name Department code
#' @param cache_dir Directory to cache results
#' @return XML content of courses
#' @export
fetch_department_courses <- function(name, cache_dir = NULL) {
  url <- glue::glue(COURSE_ENDPOINT, name = name)

  req <- httr2::request(url) |>
    httr2::req_perform()

  content <- httr2::resp_body_string(req)

  if (!is.null(cache_dir)) {
    fs::dir_create(cache_dir)

    xml_path <- fs::path(cache_dir, paste0(name, ".xml"))
    readr::write_file(content, xml_path)
  }


  courses <- parse_courses(content)
  courses$department <- name

  courses
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

#' Fetch and process courses for multiple departments
#'
#' @param departments Character vector of department codes
#' @param cache_dir Directory to cache results
#' @return A list of data frames containing course information
#' @export
fetch_all_courses <- function(departments = NULL, cache_dir = NULL) {
  if (is.null(departments)) {
    departments <- fetch_departments(cache_dir)$name
  }

  purrr::map_dfr(departments, function(dept) {
    message("Fetching department: ", dept)
    courses <- fetch_department_courses(dept, cache_dir)
    courses
  })
}
