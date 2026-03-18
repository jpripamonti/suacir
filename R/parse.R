# Column name mapping: old format (2011-2018) -> unified schema
.OLD_TO_NEW <- c(
  rubro                    = "categoria",
  concepto                 = "prestacion",
  tipo_prestacion          = "tipo",
  domicilio_cgp            = "comuna",
  domicilio_barrio         = "barrio",
  domicilio_calle          = "calle",
  domicilio_altura         = "altura",
  domicilio_esquina_proxima = "esquina_proxima"
)

# Final column order for all output tibbles
.SUACI_COLS <- c(
  "nro_solicitud", "periodo", "categoria", "prestacion", "tipo",
  "fecha_ingreso", "hora_ingreso", "comuna", "barrio", "calle",
  "altura", "esquina_proxima", "canal", "lat", "long",
  "genero", "estado_general", "year"
)

# Helper: rename columns using a named character vector (old -> new)
.rename_cols <- function(df, mapping) {
  present <- names(mapping)[names(mapping) %in% names(df)]
  if (length(present) > 0) {
    names(df)[match(present, names(df))] <- mapping[present]
  }
  df
}

# Helper: replace comma decimal separator with dot and coerce to numeric
.comma_to_numeric <- function(x) {
  as.numeric(gsub(",", ".", as.character(x)))
}

#' @importFrom readr read_delim cols col_character
#' @importFrom dplyr select any_of
#' @keywords internal
NULL

#' Parse a SUACI CSV file into a unified tibble
#'
#' Reads a raw SUACI CSV file, harmonises column names, parses dates and
#' coordinates, and returns a tibble with consistent column types regardless
#' of the source year.
#'
#' This function is primarily intended for internal use and testing. End users
#' should call [suaci_get()] instead.
#'
#' @param path Path to the CSV file.
#' @param year Integer. The year the file corresponds to.
#'
#' @return A [tibble][tibble::tibble] with columns defined in `.SUACI_COLS`.
#'
#' @keywords internal
parse_suaci_csv <- function(path, year) {
  year <- as.integer(year)

  # Determine field delimiter
  sep <- if (year == 2025) "," else ";"

  # Read all columns as character to handle mixed locale number formats.
  # name_repair = "minimal" preserves duplicate column names so we can
  # deduplicate them ourselves and keep the correct first occurrence.
  raw <- readr::read_delim(
    path,
    delim = sep,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    name_repair = "minimal",
    trim_ws = TRUE
  )

  # Drop duplicate column names (e.g. 2016 has lat;long;lat;long).
  # Keep only the first occurrence of each name.
  raw <- raw[, !duplicated(names(raw))]

  # --- Column harmonisation ---------------------------------------------------

  if (year <= 2018) {
    # Rename old-format columns to unified schema
    raw <- .rename_cols(raw, .OLD_TO_NEW)

    # Add columns absent in the old format
    raw$nro_solicitud  <- NA_character_
    raw$canal          <- NA_character_
    raw$genero         <- NA_character_
    raw$estado_general <- NA_character_
  }

  # --- Date parsing -----------------------------------------------------------

  raw$fecha_ingreso <- if (year == 2024) {
    as.Date(raw$fecha_ingreso, format = "%d/%m/%Y")
  } else {
    as.Date(raw$fecha_ingreso)
  }

  # --- Coordinate extraction --------------------------------------------------
  # Priority:
  #   2019-2024: use lat_wgs84 / long_wgs84 (comma-decimal WGS84)
  #   2016-2018: use lat / long columns (comma-decimal WGS84)
  #   2011-2015: use lat / long columns (dot-decimal; source quality varies)
  #   2025:      use lat / long columns (dot-decimal, mostly empty)

  if (year >= 2019 && year <= 2024 &&
      "lat_wgs84" %in% names(raw) && "long_wgs84" %in% names(raw)) {
    raw$lat  <- .comma_to_numeric(raw$lat_wgs84)
    raw$long <- .comma_to_numeric(raw$long_wgs84)
  } else if (year >= 2016 && year <= 2018) {
    raw$lat  <- .comma_to_numeric(raw$lat)
    raw$long <- .comma_to_numeric(raw$long)
  } else {
    raw$lat  <- as.numeric(raw$lat)
    raw$long <- as.numeric(raw$long)
  }

  # --- Year column ------------------------------------------------------------

  raw$year <- year

  # --- Select and order final columns -----------------------------------------

  dplyr::select(raw, dplyr::any_of(.SUACI_COLS))
}
