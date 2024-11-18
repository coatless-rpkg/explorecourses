#' Fetch department list from Stanford ExploreCourses
#'
#' @param cache_dir Base cache directory (optional)
#' @return A data frame containing department information
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
#' @param name Department code (single string)
#' @param year Academic year in format YYYYYYYY (e.g., "20232024") or NULL for current year
#' @param cache_dir Base cache directory (optional)
#' @return Data frame of parsed course information
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
#' @param departments Character vector of department codes
#' @param year Academic year in format YYYYYYYY (e.g., "20232024") or NULL for current year
#' @param cache_dir Base cache directory (optional)
#' @return A data frame containing course information
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
