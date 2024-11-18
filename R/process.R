#' Process departments XML data into a data frame
#'
#' @param xml_doc XML document (xml2 doc object)
#' @return Data frame of departments
#' @keywords internal
process_departments_xml <- function(xml_doc) {
  tryCatch({
    schools <- xml2::xml_find_all(xml_doc, "//school")

    if (length(schools) == 0) {
      cli::cli_abort("No schools found in XML data")
    }

    departments <- purrr::map_dfr(schools, function(school) {
      school_name <- xml2::xml_attr(school, "name")
      deps <- xml2::xml_find_all(school, ".//department")

      if (length(deps) == 0) {
        return(NULL)
      }

      purrr::map_dfr(deps, function(dep) {
        tibble::tibble(
          name = xml2::xml_attr(dep, "name"),
          longname = xml2::xml_attr(dep, "longname"),
          school = school_name
        )
      })
    })

    if (nrow(departments) == 0) {
      cli::cli_abort("No departments found in XML data")
    }

    departments
  }, error = function(e) {
    if (inherits(e, "explorecourses_error")) {
      stop(e)
    }
    cli::cli_abort(
      c("Failed to process departments XML",
        "x" = "{conditionMessage(e)}")
    )
  })
}

#' Process courses XML data into a data frame
#'
#' @param xml_doc XML document (xml2 doc object)
#' @param department Department code
#' @return Data frame of courses with detailed schedule and instructor information
#' @keywords internal
process_courses_xml <- function(xml_doc, department) {
  # Store department in the function environment for error handler access
  department_code <- department

  tryCatch({
    courses <- xml2::xml_find_all(xml_doc, "//course")

    if (length(courses) == 0) {
      cli::cli_warn(c(
        "No courses found in XML data",
        "i" = "Department: {.val {department}}"
      ))
      return(NULL)
    }

    course_data <- purrr::map_dfr(courses, function(course) {
      # Basic course info
      basic_info <- extract_basic_course_info(course)

      # Get sections with schedule and instructor info
      section_data <- extract_section_data(course, basic_info$objectID)

      # Join sections to basic info if available
      if (!is.null(section_data) && nrow(section_data) > 0) {
        basic_info <- dplyr::left_join(
          basic_info,
          section_data,
          by = "objectID"
        )
      }

      basic_info
    })

    # Add department information
    course_data$department <- department
    course_data

  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to process courses XML",
      "i" = "Department: {.val {department}}",
      "x" = "Error: {conditionMessage(e)}"
    ))
  })
}


#' Extract basic course information
#'
#' @param course XML node for a course
#' @return Tibble with basic course information
#' @keywords internal
extract_basic_course_info <- function(course) {
  tryCatch({
    tibble::tibble(
      objectID = xml2::xml_text(xml2::xml_find_first(course, ".//courseId")),
      year = xml2::xml_text(xml2::xml_find_first(course, ".//year")),
      subject = xml2::xml_text(xml2::xml_find_first(course, ".//subject")),
      code = xml2::xml_text(xml2::xml_find_first(course, ".//code")),
      title = xml2::xml_text(xml2::xml_find_first(course, ".//title")),
      description = xml2::xml_text(xml2::xml_find_first(course, ".//description")),
      units_min = as.numeric(xml2::xml_text(xml2::xml_find_first(course, ".//unitsMin"))),
      units_max = as.numeric(xml2::xml_text(xml2::xml_find_first(course, ".//unitsMax")))
    )
  }, error = function(e) {
    cli::cli_abort(
      c("Failed to extract basic course info",
        "x" = "{conditionMessage(e)}")
    )
  })
}

#' Extract section data including schedules
#'
#' @param course XML node for a course
#' @param course_id Course ID for joining
#' @return Tibble with section and schedule data
#' @keywords internal
extract_section_data <- function(course, course_id) {
  tryCatch({
    sections <- xml2::xml_find_all(course, ".//section")

    if (length(sections) == 0) {
      return(NULL)
    }

    purrr::map_dfr(sections, function(section) {
      section_info <- extract_section_info(section, course_id)
      schedule_data <- extract_schedule_data(section)

      if (!is.null(schedule_data) && nrow(schedule_data) > 0) {
        section_info <- dplyr::left_join(
          section_info,
          schedule_data,
          by = c("class_id" = "section_id")
        )
      }

      section_info
    })
  }, error = function(e) {
    cli::cli_abort(
      c("Failed to extract section data",
        "x" = "{conditionMessage(e)}")
    )
  })
}

#' Extract section information
#'
#' @param section XML node for a section
#' @param course_id Course ID for joining
#' @return Tibble with section information
#' @keywords internal
extract_section_info <- function(section, course_id) {
  tryCatch({
    tibble::tibble(
      objectID = course_id,
      term = xml2::xml_text(xml2::xml_find_first(section, ".//term")),
      term_id = xml2::xml_text(xml2::xml_find_first(section, ".//termId")),
      section_number = xml2::xml_text(xml2::xml_find_first(section, ".//sectionNumber")),
      component = xml2::xml_text(xml2::xml_find_first(section, ".//component")),
      class_id = xml2::xml_text(xml2::xml_find_first(section, ".//classId")),
      current_class_size = as.numeric(xml2::xml_text(xml2::xml_find_first(section, ".//currentClassSize"))),
      max_class_size = as.numeric(xml2::xml_text(xml2::xml_find_first(section, ".//maxClassSize")))
    )
  }, error = function(e) {
    cli::cli_abort(
      c("Failed to extract section info",
        "x" = "{conditionMessage(e)}")
    )
  })
}

#' Extract schedule data from a section including instructors and their roles
#'
#' @param section XML node for a section
#' @return Tibble with schedule and instructor information
#' @keywords internal
extract_schedule_data <- function(section) {
  tryCatch({
    schedules <- xml2::xml_find_all(section, ".//schedule")

    if (length(schedules) == 0) {
      return(NULL)
    }

    purrr::map_dfr(schedules, function(schedule) {
      # Get instructors for this schedule
      instructors <- xml2::xml_find_all(schedule, ".//instructor")

      schedule_base <- tibble::tibble(
        section_id = xml2::xml_text(xml2::xml_find_first(section, ".//classId")),
        days = xml2::xml_text(xml2::xml_find_first(schedule, ".//days")),
        start_time = xml2::xml_text(xml2::xml_find_first(schedule, ".//startTime")),
        end_time = xml2::xml_text(xml2::xml_find_first(schedule, ".//endTime")),
        location = xml2::xml_text(xml2::xml_find_first(schedule, ".//location"))
      )

      if (length(instructors) > 0) {
        # Extract instructor information including roles
        instructor_info <- purrr::map_dfr(instructors, function(instructor) {
          tibble::tibble(
            instructor_name = xml2::xml_text(xml2::xml_find_first(instructor, ".//name")),
            instructor_sunet = xml2::xml_text(xml2::xml_find_first(instructor, ".//sunet")),
            instructor_role = xml2::xml_text(xml2::xml_find_first(instructor, ".//role"))
          )
        })

        # Create combined strings with format "name (role)"
        instructors_with_roles <- paste0(
          instructor_info$instructor_name,
          " (",
          instructor_info$instructor_role,
          ")"
        )

        # Combine information into semicolon-separated strings
        schedule_base$instructors <- paste(instructors_with_roles, collapse = "; ")
        schedule_base$instructor_names <- paste(instructor_info$instructor_name, collapse = "; ")
        schedule_base$instructor_sunets <- paste(instructor_info$instructor_sunet, collapse = "; ")
        schedule_base$instructor_roles <- paste(instructor_info$instructor_role, collapse = "; ")
      } else {
        # Add empty instructor fields if no instructors found
        schedule_base$instructors <- NA_character_
        schedule_base$instructor_names <- NA_character_
        schedule_base$instructor_sunets <- NA_character_
        schedule_base$instructor_roles <- NA_character_
      }

      schedule_base
    })
  }, error = function(e) {
    cli::cli_abort(
      c("Failed to extract schedule data",
        "x" = "{conditionMessage(e)}")
    )
  })
}
