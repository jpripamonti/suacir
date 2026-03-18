#' @importFrom httr2 request req_perform
#' @importFrom dplyr bind_rows
#' @importFrom rlang abort
#' @keywords internal
NULL

#' Download SUACI data for a single year
#'
#' Downloads the CSV file(s) for the requested year from the Buenos Aires Data
#' portal and returns a unified tibble. Years 2020 and later have their data
#' split across two files on the portal; both are downloaded and combined
#' automatically.
#'
#' @param year Integer. The year to download. Must be one of the values
#'   returned by [suaci_years()].
#'
#' @return A [tibble][tibble::tibble] with the following columns:
#' \describe{
#'   \item{nro_solicitud}{Request number (character). `NA` for 2011–2018.}
#'   \item{periodo}{Period code in `yyyymm` format (character).}
#'   \item{categoria}{Service category (character).}
#'   \item{prestacion}{Specific service requested (character).}
#'   \item{tipo}{Request type: Reclamo, Solicitud, Denuncia, etc. (character).}
#'   \item{fecha_ingreso}{Date the request was received ([Date]).}
#'   \item{hora_ingreso}{Time the request was received (character).}
#'   \item{comuna}{Administrative district number (character).}
#'   \item{barrio}{Neighbourhood name (character).}
#'   \item{calle}{Street name (character).}
#'   \item{altura}{Street number (character).}
#'   \item{esquina_proxima}{Nearest cross street (character).}
#'   \item{canal}{Intake channel (character). `NA` for 2011–2018.}
#'   \item{lat}{WGS84 latitude (numeric). Data quality varies for 2011–2015.}
#'   \item{long}{WGS84 longitude (numeric). Data quality varies for 2011–2015.}
#'   \item{genero}{Reported gender of the requester (character). `NA` for
#'     2011–2018.}
#'   \item{estado_general}{Current status of the request (character). `NA`
#'     for 2011–2018.}
#'   \item{year}{Source year (integer).}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' df_2023 <- suaci_get(2023)
#' }
suaci_get <- function(year) {
  year <- as.integer(year)

  available <- suaci_years()
  if (!year %in% available) {
    rlang::abort(
      paste0(
        "Year ", year, " is not available. ",
        "Use suaci_years() to see available years."
      )
    )
  }

  resource_ids <- .SUACI_RESOURCES[[as.character(year)]]
  urls         <- paste0(.SUACI_BASE_URL, "/", resource_ids, "/download")

  dfs <- lapply(urls, function(url) {
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)

    req <- httr2::request(url)
    httr2::req_perform(req, path = tmp)

    parse_suaci_csv(tmp, year)
  })

  dplyr::bind_rows(dfs)
}

#' Download SUACI data for all available years
#'
#' Calls [suaci_get()] for every year returned by [suaci_years()] and
#' combines the results into a single tibble. Downloading the full dataset
#' requires fetching several gigabytes of data; this may take several minutes
#' depending on your connection.
#'
#' @param verbose Logical. If `TRUE` (the default), prints a progress message
#'   before downloading each year.
#'
#' @return A [tibble][tibble::tibble] with the same columns as [suaci_get()],
#'   containing data for all available years.
#'
#' @export
#' @examples
#' \dontrun{
#' all_data <- suaci_get_all()
#' }
suaci_get_all <- function(verbose = TRUE) {
  years <- suaci_years()

  dfs <- lapply(years, function(y) {
    if (isTRUE(verbose)) {
      message("Downloading ", y, "...")
    }
    suaci_get(y)
  })

  dplyr::bind_rows(dfs)
}
