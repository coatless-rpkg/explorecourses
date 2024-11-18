#' Write course data to cache
#'
#' @param data Course data frame
#' @param cache_dir Directory to cache results
#' @param dept Department code
#' @export
write_cache <- function(data, cache_dir, dept) {
  fs::dir_create(cache_dir)
  jsonlite::write_json(
    data,
    fs::path(cache_dir, paste0(dept, ".json")),
    pretty = TRUE
  )
}

#' Read course data from cache
#'
#' @param cache_dir Directory containing cached files
#' @param dept Department code (optional)
#' @return Data frame of course data
#' @export
read_cache <- function(cache_dir, dept = NULL) {
  if (is.null(dept)) {
    files <- fs::dir_ls(cache_dir, glob = "*.json")
    purrr::map_dfr(files, jsonlite::read_json, simplifyVector = TRUE)
  } else {
    file <- fs::path(cache_dir, paste0(dept, ".json"))
    if (fs::file_exists(file)) {
      jsonlite::read_json(file, simplifyVector = TRUE)
    } else {
      stop("Cache file not found for department: ", dept)
    }
  }
}
