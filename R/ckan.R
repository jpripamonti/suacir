.CKAN_API_URL <- "https://data.buenosaires.gob.ar/api/3/action/package_show"
.CKAN_PACKAGE <- "sistema-unico-atencion-ciudadana"

# Session-level in-memory cache
.ckan_cache <- new.env(parent = emptyenv())

#' @importFrom httr2 request req_error req_perform resp_status resp_body_json
#' @importFrom rlang abort
#' @keywords internal
NULL

# Internal: parse a CKAN package_show JSON response into a year -> URLs list.
# Exported only to allow unit testing without network access.
.parse_ckan_response <- function(body) {
  if (!isTRUE(body$success)) {
    rlang::abort("CKAN API returned an error response.")
  }

  resources <- body$result$resources
  result    <- list()

  for (r in resources) {
    if (toupper(r$format) != "CSV") next
    year_match <- regmatches(r$name, regexpr("\\d{4}", r$name))
    if (length(year_match) == 0L) next
    yr         <- year_match[[1L]]
    result[[yr]] <- c(result[[yr]], r$url)
  }

  # Keep only complete years (2011-2025); exclude the current incomplete year
  valid  <- as.character(2011:2025)
  result <- result[names(result) %in% valid]
  result[order(as.integer(names(result)))]
}

#' Fetch SUACI resource metadata from the CKAN API
#'
#' Queries the Buenos Aires Data portal CKAN API to retrieve the list of
#' available CSV resources for the SUACI dataset. Results are cached in
#' memory for the duration of the R session.
#'
#' @param refresh Logical. If `TRUE`, bypasses the cache and re-fetches from
#'   the API. Default is `FALSE`.
#'
#' @return A named list where each name is a year (as character, e.g.
#'   `"2023"`) and each value is a character vector of one or more download
#'   URLs for that year.
#'
#' @export
#' @examples
#' \dontrun{
#' res <- suaci_ckan_resources()
#' names(res)       # available years
#' res[["2023"]]    # download URLs for 2023
#' }
suaci_ckan_resources <- function(refresh = FALSE) {
  if (!isTRUE(refresh) && exists("resources", envir = .ckan_cache)) {
    return(get("resources", envir = .ckan_cache))
  }

  url  <- paste0(.CKAN_API_URL, "?id=", .CKAN_PACKAGE)
  resp <- httr2::request(url) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200L) {
    rlang::abort(
      paste0(
        "CKAN API request failed (HTTP ", httr2::resp_status(resp), "). ",
        "Check your internet connection."
      )
    )
  }

  body   <- httr2::resp_body_json(resp)
  result <- .parse_ckan_response(body)

  assign("resources", result, envir = .ckan_cache)
  result
}
