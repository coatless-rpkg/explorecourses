#' Process Stanford departments XML data into a data frame
#'
#' Parses XML data containing Stanford University department information into a structured
#' data frame. The function processes hierarchical XML data where departments are nested
#' within schools, extracting department codes, full names, and their associated schools.
#'
#' @param xml_doc An `xml2` document object containing Stanford departments data.
#'   Expected to have a structure with `school` nodes containing `department` nodes.
#'
#' @return
#' A [tibble][tibble::tibble-package] with three columns:
#'
#' - `name`: Character. Department code/abbreviation (e.g., "CS")
#' - `longname`: Character. Full department name (e.g., "Computer Science")
#' - `school`: Character. Name of the school containing the department
#'
#' @seealso
#' - [xml2::xml_find_all()] for the XML parsing functionality
#' - [fetch_departments()] for the public interface to this functionality
#'
#' @details
#' The function performs the following steps:
#'
#' 1. Locates all school nodes in the XML using XPath
#' 2. For each school, extracts its name and finds all department nodes
#' 3. For each department, extracts:
#'    - Department code (`name`)
#'    - Full department name (`longname`)
#'    - Associated school name (`school`)
#' 4. Combines all departments into a single data frame
#'
#' The function includes error handling for:
#'
#' - Missing school data
#' - Missing department data
#' - XML parsing errors
#'
#' @section Error handling:
#' If no schools or departments are found in the XML, an error is thrown.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' xml_data <- xml2::read_xml("departments.xml")
#' departments_df <- process_departments_xml(xml_data)
#' }
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

#' Process Stanford course XML data into a data frame
#'
#' Parses XML data containing Stanford University course information into a structured
#' data frame. The function processes detailed course data including basic course
#' information, section details, schedules, and instructor information.
#'
#' @details
#'
#' The function processes course data in several stages:
#'
#' 1. Locates all course nodes in the XML using XPath
#' 2. For each course:
#'    - Extracts basic course information (ID, title, units, etc.)
#'    - Extracts section data including schedules and instructors
#'    - Joins section data with basic course information
#' 3. Adds department code to all courses
#'
#' Course sections may include:
#'
#' - Term information
#' - Class components (e.g., lecture, discussion)
#' - Schedule details (days, times, locations)
#' - Instructor information
#' - Enrollment data
#'
#' @param xml_doc An `xml2` document object containing Stanford course data.
#'   Expected to have a structure with `course` nodes containing section and
#'   schedule information.
#' @param department Character string. Department code (e.g., `"CS"`) used to
#'   identify the department for all courses in the XML.
#'
#' @return A [tibble][tibble::tibble-package] containing course information with columns:
#' - `objectID`: Character. Unique course identifier
#' - `year`: Character. Academic year
#' - `subject`: Character. Subject code
#' - `code`: Character. Course number
#' - `title`: Character. Course title
#' - `description`: Character. Course description
#' - `units_min`: Numeric. Minimum units
#' - `units_max`: Numeric. Maximum units
#' - Additional columns for section, schedule, and instructor information when available
#' - `department`: Character. Department code
#'
#' @return
#' `NULL` if no courses are found (with a warning)
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' xml_data <- xml2::read_xml("cs_courses.xml")
#' cs_courses <- process_courses_xml(xml_data, "CS")
#' }
#'
#' @seealso
#' - [extract_basic_course_info()] for basic course data extraction
#' - [extract_section_data()] for section and schedule processing
#' - [fetch_department_courses()] for the public interface to this functionality
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


#' Extract basic course information from XML node
#'
#' Extracts fundamental course information from a Stanford course XML node into a
#' structured tibble. This function handles the core course attributes that are
#' common to all courses, independent of sections or schedules.
#'
#' @param course An `xml2` node object representing a single course. Expected to
#'   contain child nodes for:
#'   - `courseId`
#'   - `year`
#'   - `subject`
#'   - `code`
#'   - `title`
#'   - `description`
#'   - `unitsMin`
#'   - `unitsMax`
#'
#' @return
#' A [tibble][tibble::tibble-package] with one row containing:
#'
#' - `objectID`: Character. Unique course identifier from `courseId`
#' - `year`: Character. Academic year
#' - `subject`: Character. Subject code (e.g., "CS")
#' - `code`: Character. Course number (e.g., "106A")
#' - `title`: Character. Full course title
#' - `description`: Character. Course description text
#' - `units_min`: Numeric. Minimum units for the course
#' - `units_max`: Numeric. Maximum units for the course
#'
#' @seealso
#' - [xml2::xml_find_first()] for the XML node selection
#' - [xml2::xml_text()] for text extraction
#' - [process_courses_xml()] for the parent function using this extraction
#'
#' @details
#' The function extracts the following course attributes using XPath:
#' - Course ID (unique identifier)
#' - Academic year
#' - Subject code
#' - Course code (number)
#' - Course title
#' - Course description
#' - Unit range (minimum and maximum)
#'
#' All text fields are extracted using `xml_find_first()` to get the first matching
#' node, with unit values converted to numeric format.
#'
#' @section Error Handling:
#' The function uses `tryCatch` to handle potential XML parsing errors. If any
#' required node is missing or cannot be parsed, it throws an error with details
#' about the failure.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' course_node <- xml2::xml_find_first(xml_doc, "//course")
#' basic_info <- extract_basic_course_info(course_node)
#' }
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

#' Extract section and schedule data from a course XML node
#'
#' Processes section-level information from a Stanford course XML node, including
#' both section details and associated schedule information. This function handles
#' the extraction and combination of section metadata with its corresponding
#' schedule data.
#'
#' @param course An `xml2` node object representing a single course. Expected to
#'   contain child `section` nodes, each potentially containing schedule
#'   information.
#' @param course_id Character string. The course identifier used to link section
#'   data back to the parent course.
#'
#' @return
#'
#' A [tibble][tibble::tibble-package] containing section and schedule
#' information, or `NULL` if no sections are found. The tibble includes:
#'
#' From section information:
#'
#' - `objectID`: Character. Course identifier (from `course_id`)
#' - `term`: Character. Academic term
#' - `term_id`: Character. Term identifier
#' - `section_number`: Character. Section number
#' - `component`: Character. Section component (e.g., "LEC", "DIS")
#' - `class_id`: Character. Unique class identifier
#' - `current_class_size`: Numeric. Current enrollment
#' - `max_class_size`: Numeric. Maximum enrollment
#'
#' When schedule data exists, additional columns include:
#'
#' - Schedule timing information
#' - Location data
#' - Instructor information
#'
#' @details
#' The function performs the following steps:
#'
#' 1. Locates all section nodes within the course
#' 2. For each section:
#'    - Extracts basic section information using `extract_section_info()`
#'    - Extracts schedule data using `extract_schedule_data()`
#'    - Joins section and schedule information if schedule data exists
#' 3. Combines all section data into a single tibble
#'
#' The function returns `NULL` if no sections are found, allowing for courses
#' that may not have active sections.
#' @seealso
#'
#' - [extract_section_info()] for section information extraction
#' - [extract_schedule_data()] for schedule data extraction
#' - [process_courses_xml()] for the parent function using this extraction
#'
#'
#' @section Data Joining:
#'
#' Section and schedule data are joined using the class identifier, with
#' `class_id` from section data matching `section_id` from schedule data.
#'
#' @section Error Handling:
#'
#' If section data extraction fails, the function throws an error with details.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' course_node <- xml2::xml_find_first(xml_doc, "//course")
#' course_id <- "222796"
#' section_data <- extract_section_data(course_node, course_id)
#' }
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

#' Extract basic section information from XML node
#'
#' Extracts fundamental section-level information from a Stanford course section XML node
#' into a structured tibble. This function processes the core attributes of a course
#' section, such as term information, component type, and enrollment data.
#'
#' @param section An `xml2` node object representing a single course section.
#'   Expected to contain child nodes for:
#'   - `term`
#'   - `termId`
#'   - `sectionNumber`
#'   - `component`
#'   - `classId`
#'   - `currentClassSize`
#'   - `maxClassSize`
#' @param course_id Character string. The parent course identifier used to link
#'   section data back to the course.
#'
#' @return
#' A [tibble][tibble::tibble-package] with one row containing:
#'
#' - `objectID`: Character. Course identifier (from `course_id`)
#' - `term`: Character. Academic term (e.g., "Autumn", "Winter")
#' - `term_id`: Character. Unique term identifier
#' - `section_number`: Character. Section number within the course
#' - `component`: Character. Section type (e.g., "LEC", "DIS", "LAB")
#' - `class_id`: Character. Unique identifier for this section
#' - `current_class_size`: Numeric. Current number of enrolled students
#' - `max_class_size`: Numeric. Maximum enrollment capacity
#'
#' @seealso
#'
#' - [xml2::xml_find_first()] for XML node selection
#' - [xml2::xml_text()] for text extraction
#' - [extract_section_data()] for the parent function using this extraction
#'
#' @details
#' The function extracts the following section attributes using XPath:
#' - Term details (term name and ID)
#' - Section identification (section number, class ID)
#' - Component type (e.g., lecture, discussion)
#' - Enrollment information (current and maximum class sizes)
#'
#' All text fields are extracted using `xml_find_first()` to get the first matching
#' node. Enrollment numbers are converted to numeric format.
#'
#' @section Error Handling:
#' The function assumes all required nodes are present in the XML. Missing nodes
#' will trigger an error through the tryCatch block.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' section_node <- xml2::xml_find_first(course_node, ".//section")
#' course_id <- "CS106A-2023-2024"
#' section_info <- extract_section_info(section_node, course_id)
#' }
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

#' Extract schedule and instructor data from a section XML node
#'
#' Processes schedule and instructor information from a Stanford course section XML node.
#' This function extracts meeting times, locations, and detailed instructor information,
#' combining multiple instructors' data into semicolon-separated lists.
#'
#' @param section An `xml2` node object representing a course section. Expected to
#'   contain child nodes for:
#'   - `schedule` nodes, each containing:
#'     - `days`
#'     - `startTime`
#'     - `endTime`
#'     - `location`
#'     - Optional `instructor` nodes, each containing:
#'       - `name`
#'       - `sunet`
#'       - `role`
#'
#' @return
#'
#' A [tibble][tibble::tibble-package] containing schedule information, or
#' `NULL` if no schedules are found. The tibble includes:
#'
#' Basic schedule information:
#'
#' - `section_id`: Character. Section identifier (from classId)
#' - `days`: Character. Days of the week (e.g., "MonWedFri")
#' - `start_time`: Character. Start time
#' - `end_time`: Character. End time
#' - `location`: Character. Meeting location
#'
#' Instructor information (NA if no instructors):
#'
#' - `instructors`: Character. Combined strings in "name (role)" format
#' - `instructor_names`: Character. Semicolon-separated list of names
#' - `instructor_sunets`: Character. Semicolon-separated list of SUNet IDs
#' - `instructor_roles`: Character. Semicolon-separated list of roles
#'
#' @seealso
#'
#' - [xml2::xml_find_all()] for XML node selection
#' - [xml2::xml_text()] for text extraction
#' - [extract_section_data()] for the parent function using this data
#'
#' @details
#'
#' The function performs the following steps:
#'
#' 1. Locates all schedule nodes within the section
#' 2. For each schedule:
#'    - Extracts basic schedule information (days, times, location)
#'    - Processes instructor information if present
#'    - Combines multiple instructors' data into consolidated strings
#'
#' Instructor information is formatted in several ways:
#'
#' - Combined format: "name (role)"
#' - Separate fields: names, SUNet IDs, and roles in semicolon-separated lists
#'
#' If no instructors are found, instructor fields are set to `NA`.
#'
#' @section Error handling:
#' If schedule data extraction fails, the function throws an error with details.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' section_node <- xml2::xml_find_first(course_node, ".//section")
#' schedule_data <- extract_schedule_data(section_node)
#' }
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
