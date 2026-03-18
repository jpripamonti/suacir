# Resource ID registry ----------------------------------------------------------
# Maps each available year to its CKAN resource ID(s) on Buenos Aires Data.
# Years with two IDs have data split across two CSV files on the portal.

.SUACI_RESOURCES <- list(
  `2011` = "juqdkmgo-1952-resource",
  `2012` = "juqdkmgo-1953-resource",
  `2013` = "juqdkmgo-1954-resource",
  `2014` = "juqdkmgo-1955-resource",
  `2015` = "juqdkmgo-1956-resource",
  `2016` = "juqdkmgo-1957-resource",
  `2017` = "juqdkmgo-1958-resource",
  `2018` = "juqdkmgo-1959-resource",
  `2019` = "4aa817d1-0973-4251-858a-7ac15dcacf33",
  `2020` = c(
    "d4efb34e-485d-418c-be03-078cd61be5e1",
    "17a35d11-07ae-4d4d-949a-068a42b21835"
  ),
  `2021` = c(
    "976557f6-f3a2-464f-b1c3-bac12b3bb6a5",
    "3eb57505-84d5-4eff-810d-8f59fdc3aa20"
  ),
  `2022` = c(
    "0568f97b-e4e8-4a40-9557-4bd69e85c757",
    "f74143a6-919b-4664-97b4-93580ae7a45e"
  ),
  `2023` = c(
    "7996b532-7f51-4b1a-b9b8-041e20d3b44a",
    "31ac3f7b-d99e-4b2f-8322-e4fc7b9cde3b"
  ),
  `2024` = c(
    "daed762f-4133-4cd4-954c-5159f2c1a2d4",
    "c979cfaa-9e71-4fee-8fd3-7f23f730d11d"
  ),
  `2025` = c(
    "ec9da864-d2e0-480d-8fd3-06807c083ff4",
    "98b51fa7-258b-49ff-9edd-b3dc05f177be"
  )
)

.SUACI_BASE_URL <- paste0(
  "https://data.buenosaires.gob.ar/dataset/",
  "sistema-unico-atencion-ciudadana/resource"
)

#' Available years for SUACI data
#'
#' Returns an integer vector of the years for which SUACI data is available
#' on the Buenos Aires Data portal.
#'
#' @param use_ckan Logical. If `TRUE`, the list of available years is fetched
#'   live from the CKAN API instead of the built-in registry. Requires an
#'   internet connection. Default is `FALSE`.
#'
#' @return An integer vector of available years.
#' @export
#' @examples
#' suaci_years()
#' \dontrun{
#' suaci_years(use_ckan = TRUE)
#' }
suaci_years <- function(use_ckan = FALSE) {
  if (isTRUE(use_ckan)) {
    as.integer(names(suaci_ckan_resources()))
  } else {
    as.integer(names(.SUACI_RESOURCES))
  }
}
