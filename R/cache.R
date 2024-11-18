#' Initialize cache directories
#'
#' @param cache_dir Base cache directory
#' @return List of cache paths
#' @keywords internal
init_cache_dirs <- function(cache_dir) {
  if (is.null(cache_dir)) {
    return(NULL)
  }

  paths <- list(
    base = cache_dir,
    json = file.path(cache_dir, "json"),
    xml = file.path(cache_dir, "xml")
  )

  fs::dir_create(paths$json)
  fs::dir_create(paths$xml)

  paths
}

#' Write XML data to cache
#'
#' @param content XML content
#' @param cache_dir Base cache directory
#' @param dept Department code
#' @keywords internal
write_xml_cache <- function(content, cache_dir, dept) {
  if (is.null(cache_dir)) return(NULL)

  paths <- init_cache_dirs(cache_dir)
  xml_path <- fs::path(paths$xml, paste0(dept, ".xml"))
  readr::write_file(content, xml_path)
}

#' Write JSON data to cache
#'
#' @param data Data frame or list to cache
#' @param cache_dir Base cache directory
#' @param filename Filename without extension
#' @keywords internal
write_json_cache <- function(data, cache_dir, filename) {
  if (is.null(cache_dir)) return(NULL)

  paths <- init_cache_dirs(cache_dir)
  jsonlite::write_json(
    data,
    fs::path(paths$json, paste0(filename, ".json")),
    pretty = TRUE
  )
}

#' Read course data from cache
#'
#' @param cache_dir Base cache directory
#' @param dept Department code (optional)
#' @return Data frame of course data
#' @export
read_cache <- function(cache_dir, dept = NULL) {
  paths <- init_cache_dirs(cache_dir)

  if (is.null(dept)) {
    files <- fs::dir_ls(paths$json, glob = "*.json")
    results <- lapply(files, jsonlite::read_json, simplifyVector = TRUE)
    dplyr::bind_rows(results)
  } else {
    file <- fs::path(paths$json, paste0(dept, ".json"))
    if (fs::file_exists(file)) {
      jsonlite::read_json(file, simplifyVector = TRUE)
    } else {
      stop("Cache file not found for department: ", dept)
    }
  }
}
