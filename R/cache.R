DEFAULT_CACHE_DIR <- "explorecourses_cache"

#' Initialize cache directory
#'
#' Creates the necessary directory structure for caching data.
#' If no cache directory is specified, creates 'explorecourses_cache'
#' in the current working directory.
#'
#' @param cache_dir Base cache directory (optional)
#' @return Character string of cache directory path
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
#' @param xml_doc XML document (xml2 doc object)
#' @param name Base filename (e.g., department code)
#' @param year Optional academic year for filename suffix
#' @param cache_dir Base cache directory (optional)
#' @return Invisibly returns the cache path
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
#' @param name Department code
#' @param year Academic year (optional)
#' @param cache_dir Base cache directory (optional)
#' @return XML content as string or NULL if cache not found
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
#' @param name Department code (optional)
#' @param year Academic year (optional)
#' @param cache_dir Base cache directory (optional)
#' @return Invisibly returns TRUE if successful, FALSE otherwise
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
#' @param name Department code (optional)
#' @param year Academic year (optional)
#' @param cache_dir Base cache directory (optional)
#' @return Logical indicating if cache exists
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
#' @param year Academic year (optional)
#' @param cache_dir Base cache directory (optional)
#' @return Character vector of cached file names (without extensions)
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
#' @param x Object of class explorecourses_cache_list
#' @param ... Additional arguments passed to print
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
