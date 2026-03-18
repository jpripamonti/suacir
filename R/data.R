#' Sample SUACI data (100 rows)
#'
#' A sample of 100 rows from the 2024 SUACI dataset, included for
#' demonstration and testing purposes without requiring an internet connection.
#'
#' @format A tibble with 100 rows and 18 columns:
#' \describe{
#'   \item{nro_solicitud}{Request number (character).}
#'   \item{periodo}{Period code in `yyyymm` format (character).}
#'   \item{categoria}{Service category (character).}
#'   \item{prestacion}{Specific service requested (character).}
#'   \item{tipo}{Request type (character).}
#'   \item{fecha_ingreso}{Date the request was received ([Date]).}
#'   \item{hora_ingreso}{Time the request was received (character).}
#'   \item{comuna}{Administrative district number (character).}
#'   \item{barrio}{Neighbourhood name (character).}
#'   \item{calle}{Street name (character).}
#'   \item{altura}{Street number (character).}
#'   \item{esquina_proxima}{Nearest cross street (character).}
#'   \item{canal}{Intake channel (character).}
#'   \item{lat}{WGS84 latitude (numeric).}
#'   \item{long}{WGS84 longitude (numeric).}
#'   \item{genero}{Reported gender of the requester (character).}
#'   \item{estado_general}{Current status of the request (character).}
#'   \item{year}{Source year (integer).}
#' }
#' @source \url{https://data.buenosaires.gob.ar/dataset/sistema-unico-atencion-ciudadana}
"suaci_sample"
