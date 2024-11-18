DEFAULT_CACHE_DIR <- "explorecourses_cache"

#' Initialize cache directory for ExploreCourses data
#'
#' Creates and initializes a directory structure for caching ExploreCourses XML data.
#' This function ensures a consistent location for storing cached course and department
#' data between sessions. If no cache directory is specified, it creates a default
#' directory named `'explorecourses_cache'` in the current working directory.
#'
#' @param cache_dir Character string. Optional path to the desired cache directory.
#'   If `NULL` (default), uses `'{working_directory}/explorecourses_cache'`.
#'
#' @return
#' Character string containing the path to the initialized cache directory.
#'
#' @details
#' The function performs the following operations:
#'
#' * If `cache_dir` is `NULL`, constructs default path using `DEFAULT_CACHE_DIR`
#' * Creates the directory if it doesn't exist (including parent directories)
#' * Ensures the directory is writable
#' * Returns the full path to the created/existing directory
#'
#' The function uses `fs::dir_create()` which is safe for concurrent access
#' and handles nested directory creation automatically.
#'
#' @section Error handling:
#'
#' If directory creation fails due to permissions or other filesystem errors.
#' The error will include the attempted path and specific error message.
#'
#' @seealso
#'
#' - [write_xml_cache()] for writing data to the cache
#' - [read_xml_cache()] for reading data from the cache
#' - [clear_cache()] for removing cached data
#'
#' @examples
#' \dontrun{
#' # Use default cache location
#' cache_dir <- init_cache_dir()
#'
#' # Specify custom cache location
#' cache_dir <- init_cache_dir("~/my_course_cache")
#' }
#'
#' @keywords internal
init_cache_dir <- function(cache_dir = NULL) {
  cache_dir <- cache_dir %||% file.path(getwd(), DEFAULT_CACHE_DIR)
  tryCatch({
    fs::dir_create(cache_dir)
    cache_dir
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to create cache directory",
      "x" = "{conditionMessage(e)}",
      "i" = "Path: {.file {cache_dir}}"
    ))
  })
}

#' Write XML data to cache
#'
#' Writes XML course data to the cache directory. The function handles the creation
#' of appropriately named cache files based on department code and optional academic
#' year. Cache files are stored as XML documents with the extension `.xml`.
#'
#' @param xml_doc An `xml2` document object containing course or department data
#'   to be cached.
#' @param name Character string. Base filename for the cache file, typically a
#'   department code (e.g., "CS" for Computer Science).
#' @param year Character string. Optional academic year in YYYYYYYY format (e.g.,
#'   "20232024"). When provided, appends "_YYYYYYYY" to the filename.
#' @param cache_dir Character string. Optional path to the cache directory. If
#'   `NULL`, uses the default cache location.
#'
#' @return
#' Invisibly returns the full path to the created cache file as a character
#' string.
#'
#' @details
#' The function constructs the cache filename using the following pattern:
#' `{name}{_year}.xml`. For example:
#'
#' - Without year: "CS.xml"
#' - With year: "CS_20232024.xml"
#'
#' The function will:
#'
#' - Initialize/create the cache directory if needed
#' - Construct the appropriate filename with optional year suffix
#' - Write the XML document to the cache location
#' - Handle any errors during the write process
#'
#' @section Error Handling:
#'
#' If the XML document cannot be written to the cache location. The error will
#' include the department name, attempted file path, and specific error message.
#'
#' @seealso
#' * [init_cache_dir()] for cache directory initialization
#' * [read_xml_cache()] for reading cached XML data
#' * [clear_cache()] for removing cached files
#'
#' @examples
#' \dontrun{
#' # Cache current year's CS department data
#' xml_doc <- fetch_department_courses_raw("CS")
#' write_xml_cache(xml_doc, "CS")
#'
#' # Cache specific academic year
#' xml_doc <- fetch_department_courses_raw("CS", "20232024")
#' write_xml_cache(xml_doc, "CS", "20232024")
#' }
#'
#' @keywords internal
write_xml_cache <- function(xml_doc, name, year = NULL, cache_dir = NULL) {
  cache_dir <- init_cache_dir(cache_dir)

  suffix <- if (!is.null(year)) paste0("_", year) else ""
  xml_path <- fs::path(cache_dir, paste0(name, suffix, ".xml"))

  tryCatch({
    xml2::write_xml(xml_doc, xml_path)
    invisible(xml_path)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to write cache",
      "x" = "{conditionMessage(e)}",
      "i" = "Department: {.val {name}}",
      "i" = "Path: {.file {xml_path}}"
    ))
  })
}


#' Read XML data from cache
#'
#' Retrieves cached XML course data for a specific department and optional academic
#' year. Returns `NULL` if the requested cache file does not exist, allowing for
#' graceful fallback to fresh API requests.
#'
#' @param name Character string. Department code to read from cache (e.g., "CS" for
#'   Computer Science).
#' @param year Character string. Optional academic year in YYYYYYYY format (e.g.,
#'   "20232024"). When provided, looks for a file with "_YYYYYYYY" suffix.
#' @param cache_dir Character string. Optional path to the cache directory. If
#'   `NULL`, uses the default cache location.
#'
#' @return
#'
#' Returns one of:
#'
#' - An `xml2` document object containing the cached data if found
#' - `NULL` if the cache file does not exist
#'
#' @details
#'
#' The function looks for cache files using the following pattern:
#' `{name}{_year}.xml`. For example:
#'
#' - Without year: "CS.xml"
#' - With year: "CS_20232024.xml"
#'
#' The function will:
#'
#' - Initialize/verify the cache directory
#' - Construct the appropriate filename with optional year suffix
#' - Check if the cache file exists
#' - Read and parse the XML file if found
#' - Return `NULL` if file not found
#'
#' @section Error handling:
#'
#' If the XML file exists but cannot be read or parsed. The error will include
#' the department name, file path, and specific error message.
#'
#' @seealso
#'
#' - [write_xml_cache()] for writing data to cache
#' - [clear_cache()] for removing cached files
#' - [cache_exists()] for checking cache status
#'
#' @examples
#' \dontrun{
#' # Read current year's CS department data
#' xml_doc <- read_xml_cache("CS")
#'
#' # Read specific academic year
#' xml_doc <- read_xml_cache("CS", "20232024")
#'
#' # Handle missing cache gracefully
#' xml_doc <- read_xml_cache("CS")
#' if (is.null(xml_doc)) {
#'   # Cache miss - fetch from API instead
#'   xml_doc <- fetch_department_courses_raw("CS")
#' }
#' }
#'
#' @export
read_xml_cache <- function(name, year = NULL, cache_dir = NULL) {
  cache_dir <- init_cache_dir(cache_dir)

  suffix <- if (!is.null(year)) paste0("_", year) else ""
  xml_path <- fs::path(cache_dir, paste0(name, suffix, ".xml"))

  if (!fs::file_exists(xml_path)) {
    return(NULL)
  }

  tryCatch({
    xml2::read_xml(xml_path)
  }, error = function(e) {
    cli::cli_abort(c(
      "Error reading cache XML",
      "x" = "{conditionMessage(e)}",
      "i" = "Department: {.val {name}}",
      "i" = "Path: {.file {xml_path}}"
    ))
  })
}

#' Clear cache for specific data or entirely
#'
#' Removes cached XML files from the cache directory. Can selectively clear files
#' for a specific department, academic year, or combination of both. If no filters
#' are specified, clears all cached XML files.
#'
#' @param name Character string. Optional department code (e.g., "CS") to clear
#'   specific department cache files.
#' @param year Character string. Optional academic year in YYYYYYYY format (e.g.,
#'   "20232024") to clear cache files for a specific year.
#' @param cache_dir Character string. Optional path to the cache directory. If
#'   `NULL`, uses the default cache location.
#'
#' @return Invisibly returns `TRUE` if the operation was successful. Throws an
#'   error if the operation fails.
#'
#' @details
#' The function supports three clearing modes based on parameter combinations:
#'
#' 1. Department-specific: When `name` is provided
#'    - Clears: `"{name}.xml"` or `"{name}_{year}.xml"` if year provided
#' 2. Year-specific: When only `year` is provided
#'    - Clears: All files matching `"*_{year}.xml"`
#' 3. Complete clear: When neither `name` nor `year` provided
#'    - Clears: All `"*.xml"` files in cache directory
#'
#' The function will:
#'
#' - Initialize/verify the cache directory
#' - Determine which files to remove based on parameters
#' - Remove matching files
#' - Display success message with number of files removed
#'
#' @section Error Handling:
#'
#' If files cannot be deleted or the cache directory cannot be accessed. The error
#' will include the specific error message and cache directory path.
#'
#' @seealso
#'
#' - [write_xml_cache()] for writing data to cache
#' - [read_xml_cache()] for reading cached data
#' - [cache_exists()] for checking cache status
#'
#' @examples
#' \dontrun{
#' # Clear all cache files
#' clear_cache()
#'
#' # Clear specific department's cache
#' clear_cache(name = "CS")
#'
#' # Clear all departments for specific year
#' clear_cache(year = "20232024")
#'
#' # Clear specific department and year
#' clear_cache(name = "CS", year = "20232024")
#'
#' # Clear cache from custom location
#' clear_cache(cache_dir = "~/my_course_cache")
#' }
#'
#' @export
clear_cache <- function(name = NULL, year = NULL, cache_dir = NULL) {
  cache_dir <- init_cache_dir(cache_dir)

  tryCatch({
    if (!is.null(name)) {
      # Clear specific department
      suffix <- if (!is.null(year)) paste0("_", year) else ""
      pattern <- paste0(name, suffix, ".xml")
      files <- fs::dir_ls(cache_dir, glob = pattern)
    } else if (!is.null(year)) {
      # Clear specific year
      pattern <- paste0("*_", year, ".xml")
      files <- fs::dir_ls(cache_dir, glob = pattern)
    } else {
      # Clear all files
      files <- fs::dir_ls(cache_dir, glob = "*.xml")
    }

    fs::file_delete(files)
    cli::cli_alert_success(c(
      "Cache cleared successfully",
      "i" = "Files removed: {length(files)}"
    ))
    invisible(TRUE)
  }, error = function(e) {
    cli::cli_abort(c(
      "Error clearing cache",
      "x" = "{conditionMessage(e)}",
      "i" = "Directory: {.file {cache_dir}}"
    ))
  })
}

#' Check if cache exists for specific data
#'
#' Checks for the existence of cached XML files in the cache directory. Can check
#' for files for a specific department, academic year, or combination of both. If
#' no filters are specified, checks if any cache files exist.
#'
#' @param name Character string. Optional department code (e.g., `"CS"`) to check for
#'   specific department cache files.
#' @param year Character string. Optional academic year in `YYYYYYYY` format (e.g.,
#'   `"20232024"`) to check for cache files from a specific year.
#' @param cache_dir Character string. Optional path to the cache directory. If
#'   `NULL`, uses the default cache location.
#'
#' @return
#' A logical value:
#'
#' - `TRUE` if matching cache files exist
#' - `FALSE` if no matching files found or if check fails
#'
#' @details
#' The function supports three checking modes based on parameter combinations:
#'
#' 1. Department-specific: When `name` is provided
#'    - Checks for: `"{name}.xml"` or `"{name}_{year}.xml"` if year provided
#' 2. Year-specific: When only `year` is provided
#'    - Checks for: Any files matching `"*_{year}.xml"`
#' 3. General check: When neither `name` nor `year` provided
#'    - Checks for: Any `"*.xml"` files in cache directory
#'
#' On error (e.g., permission issues), the function:
#'
#' - Issues a warning with error details
#' - Returns `FALSE` to allow graceful error handling
#'
#' @seealso
#'
#' - [read_xml_cache()] for reading cached data
#' - [write_xml_cache()] for writing data to cache
#' - [clear_cache()] for removing cached files
#'
#' @examples
#' \dontrun{
#' # Check if any cache exists
#' if (cache_exists()) {
#'   # Process cached data
#' }
#'
#' # Check specific department cache
#' if (cache_exists(name = "CS")) {
#'   cached_data <- read_xml_cache("CS")
#' }
#'
#' # Check specific year
#' if (cache_exists(year = "20232024")) {
#'   # Process cached data for year
#' }
#'
#' # Check specific department and year
#' if (cache_exists(name = "CS", year = "20232024")) {
#'   cached_data <- read_xml_cache("CS", "20232024")
#' }
#' }
#'
#' @export
cache_exists <- function(name = NULL, year = NULL, cache_dir = NULL) {
  cache_dir <- init_cache_dir(cache_dir)

  tryCatch({
    if (!is.null(name)) {
      # Check specific department
      suffix <- if (!is.null(year)) paste0("_", year) else ""
      pattern <- paste0(name, suffix, ".xml")
      any(fs::file_exists(fs::dir_ls(cache_dir, glob = pattern)))
    } else if (!is.null(year)) {
      # Check specific year
      pattern <- paste0("*_", year, ".xml")
      any(fs::file_exists(fs::dir_ls(cache_dir, glob = pattern)))
    } else {
      # Check if directory has any files
      length(fs::dir_ls(cache_dir, glob = "*.xml")) > 0
    }
  }, error = function(e) {
    cli::cli_warn(
      c("Error checking cache",
        "x" = "{conditionMessage(e)}",
        "i" = "Directory: {.file {cache_dir}}")
    )
    invisible(FALSE)
  })
}

#' List cached files
#'
#' Lists all cached XML files in the cache directory, optionally filtered by
#' academic year. Returns a character vector of filenames without extensions,
#' with additional metadata attached as attributes.
#'
#' @param year Character string. Optional academic year in `YYYYYYYY` format (e.g.,
#'   `"20232024"`) to list only cache files from a specific year.
#' @param cache_dir Character string. Optional path to the cache directory. If
#'   `NULL`, uses the default cache location.
#'
#' @return
#' A character vector with class `explorecourses_cache_list` containing:
#'
#' - File names without the .xml extension
#' - Additional attributes:
#'   - `total`: Number of files found
#'   - `path`: Full path to cache directory
#'   - `year`: Academic year filter (if provided)
#'
#' Returns empty character vector (`character(0)`) if no files found.
#'
#' @details
#' The function supports two listing modes:
#'
#' 1. Year-specific: When `year` is provided
#'    - Lists: All files matching `"*_{year}.xml"`
#'    - Validates year format before searching
#' 2. Complete listing: When no `year` provided
#'    - Lists: All `"*.xml"` files in cache directory
#'
#' File names in the returned vector:
#'
#' - Have the `.xml` extension removed
#' - Retain any year suffixes (e.g., `"CS_20232024"`)
#' - Are returned in the order found by the filesystem
#'
#' @section Error Handling:
#'
#' If the cache directory cannot be accessed or listed. The error will include
#' the specific error message and cache directory path.
#'
#' @seealso
#'
#' - [cache_exists()] for checking specific cache files
#' - [clear_cache()] for removing cached files
#' - [read_xml_cache()] for reading cached data
#'
#' @examples
#' \dontrun{
#' # List all cached files
#' files <- list_cache()
#' print(files)  # Uses special print method for explorecourses_cache_list
#'
#' # Get total number of cached files
#' total <- attr(files, "total")
#'
#' # List files for specific year
#' files_2023 <- list_cache(year = "20232024")
#'
#' # List files in custom cache directory
#' files <- list_cache(cache_dir = "~/my_course_cache")
#'
#' # Check if any files were found
#' if (length(files) > 0) {
#'   # Process files
#' }
#' }
#'
#' @export
list_cache <- function(year = NULL, cache_dir = NULL) {
  tryCatch({
    cache_dir <- init_cache_dir(cache_dir)

    if (!is.null(year)) {
      # Validate year format
      validate_academic_year(year)
      pattern <- paste0("*_", year, ".xml")
    } else {
      pattern <- "*.xml"
    }

    files <- fs::dir_ls(cache_dir, glob = pattern)

    if (length(files) == 0) {
      cli::cli_warn(
        c("No cached files found",
          if (!is.null(year)) c("i" = "Year: {.val {year}}"),
          "i" = "Directory: {.file {cache_dir}}"),
        path = cache_dir,
        year = year
      )
      return(character(0))
    }

    # Process filenames
    filenames <- basename(files)
    result <- gsub("\\.xml$", "", filenames)

    # Add informative attributes
    structure(
      result,
      total = length(result),
      path = cache_dir,
      year = year,
      class = c("explorecourses_cache_list", "list")
    )

  }, error = function(e) {
    cli::cli_abort(
      c("Error listing cache",
        "x" = "{conditionMessage(e)}",
        "i" = "Directory: {.file {cache_dir}}")
    )
  })
}

#' Print method for cache list
#'
#' A custom print method for objects of class `explorecourses_cache_list`. Displays
#' the cache contents in a formatted, color-coded layout with summary information
#' and files arranged in columns.
#'
#' @param x An object of class `explorecourses_cache_list`, typically returned by
#'   [list_cache()].
#' @param ... Additional arguments passed to print (currently unused).
#'
#' @return
#' Invisibly returns the input object `x`.
#'
#' @details
#' The print method provides a formatted display including:
#'
#' - Header with "Cache contents:" in cyan
#' - Summary information in blue:
#'   - Total number of cached files
#'   - Academic year (if filtered)
#'   - Cache directory path
#' - Files listed in columns (5 files per row)
#' - Special handling for empty cache (warning message)
#'
#' Output formatting:
#'
#' - Uses color for emphasis (cyan and blue)
#' - Properly pluralizes file counts
#' - Aligns files in 25-character width columns
#' - Uses filesystem path formatting for directory paths
#'
#' @note
#' This is a method for the generic [print()] function and is automatically called
#' when printing objects of class `explorecourses_cache_list`. It should not typically
#' be called directly.
#'
#' @seealso
#'
#' - [list_cache()] which returns objects of class `explorecourses_cache_list`
#'
#' @examples
#' \dontrun{
#' # Normal usage through print generic
#' cache_files <- list_cache()
#' print(cache_files)  # Automatically calls this method
#'
#' # Or simply
#' cache_files  # Auto-printing also uses this method
#'
#' # Example with year filter
#' year_files <- list_cache(year = "20232024")
#' print(year_files)
#' }
#'
#' @export
print.explorecourses_cache_list <- function(x, ...) {
  cli::cli_div(theme = list(
    span.cache = list(color = "cyan"),
    span.emph = list(color = "blue")
  ))

  cli::cli_text("{.cache Cache contents:}")
  if (length(x) == 0) {
    cli::cli_alert_warning("No cached files found")
    return(invisible(x))
  }

  # Get attributes
  total <- attr(x, "total")
  path <- attr(x, "path")
  year <- attr(x, "year")

  # Print summary
  cli::cli_text("")
  cli::cli_text("Found {.emph {total}} cached file{?s}")
  if (!is.null(year)) {
    cli::cli_text("Year: {.emph {year}}")
  }
  cli::cli_text("Directory: {.file {path}}")
  cli::cli_text("")

  # Print files in organized format
  files_per_row <- 5
  rows <- split(x, ceiling(seq_along(x) / files_per_row))

  for (row in rows) {
    cli::cli_text(paste(format(row, width = 25), collapse = ""))
  }

  invisible(x)
}
