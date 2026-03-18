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
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
}

# Helper: recover coordinates with scale errors present in 2011-2013 and 2016.
#
# All SUACI records lie within the City of Buenos Aires:
#   lat  ∈ [-35.0, -34.3]
#   long ∈ [-58.7, -58.2]
#
# Some rows in the source files have coordinates multiplied by an integer factor
# (2, 3, 4, 5, or 10) due to upstream data-entry or export bugs. For example,
# 2011-2013 contain values like -345.8 (should be -34.58, factor = 10), and
# 2016 contains values like -69.16 (should be -34.58, factor = 2).
#
# The recovery algorithm tries divisors 2, 3, 4, 5, 10 in order, always
# working from the original raw value. The first divisor whose result falls
# within the CABA bounding box is accepted. Coordinates that cannot be
# recovered by any divisor are set to NA.
#
# Coverage on 2016 (965 946 rows with coordinates):
#   divisor 1  → 98.5 %  (already correct)
#   divisor 2  →  1.0 %
#   divisor 3  →  0.3 %
#   divisor 4  →  0.1 %
#   divisor 5  →  0.1 %
#   irrecoverable → < 0.1 %
# Each axis is recovered independently because lat and long may carry different
# scale errors in the same row (e.g. lat already correct at -34.59 while long
# is still -584.9 in the same record, as observed in 2011-2013).
.recover_one_coord <- function(x, lo, hi) {
  out <- x
  for (d in c(2, 3, 4, 5, 10)) {
    still_bad <- !is.na(out) & (out < lo | out > hi)
    if (!any(still_bad)) break
    candidate       <- x[still_bad] / d   # always divide the original raw value
    fixed           <- candidate >= lo & candidate <= hi
    out[still_bad][fixed] <- candidate[fixed]
  }
  out[!is.na(out) & (out < lo | out > hi)] <- NA_real_
  out
}

.recover_coords <- function(lat, long) {
  list(
    lat  = .recover_one_coord(lat,  lo = -35.0, hi = -34.3),
    long = .recover_one_coord(long, lo = -58.7, hi = -58.2)
  )
}

# Helper: convert CABA local metric coordinates to WGS84 degrees.
#
# From 2019 onward the portal stores two parallel coordinate representations:
#   - lat / long : a local planar coordinate system (approximate metres from a
#     local origin), used internally by the Buenos Aires GIS infrastructure.
#   - lat_wgs84 / long_wgs84 : WGS84 geographic coordinates (what users need).
#
# For 2025 the WGS84 columns were dropped but the local metric columns remain.
# The linear relationship between the two systems was calibrated by regressing
# lat_wgs84 ~ local_lat and long_wgs84 ~ local_long on 1,951 paired
# observations from 2019 (R² = 0.9999999, max residual ≈ 4 m):
#
#   lat_wgs84  = local_lat  × 9.013576e-06 − 35.53062
#   long_wgs84 = local_long × 1.090318e-05 − 59.55362
.local_to_wgs84 <- function(local_lat, local_long) {
  lat  <- local_lat  * 9.013576e-06 - 35.53062
  long <- local_long * 1.090318e-05 - 59.55362
  list(lat = lat, long = long)
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
  # All SUACI records are within CABA (lat ≈ -34.5 to -34.7, long ≈ -58.3 to -58.5).
  # The source files use different coordinate representations by year:
  #
  #   2011-2013: lat/long, dot-decimal WGS84. Contain a mix of correct values
  #              and values scaled by 10x (e.g. -345.8 instead of -34.58).
  #              Fixed by .fix_coord_scale().
  #   2014-2015: lat/long, comma-decimal WGS84. Clean data.
  #   2016:      lat/long, comma-decimal WGS84. Same 10x errors as 2011-2013,
  #              plus isolated corrupt values that are set to NA.
  #   2017-2018: lat/long, comma-decimal WGS84. Clean data.
  #   2019-2024: lat/long are in a CABA local metric CRS (values ~93 000-110 000),
  #              NOT WGS84. lat_wgs84/long_wgs84 columns contain the correct
  #              geographic coordinates and are used instead.
  #   2025:      lat/long are in the same local metric CRS as 2019-2024 but
  #              lat_wgs84/long_wgs84 columns were dropped. Converted to WGS84
  #              using .local_to_wgs84() (max error ≈ 4 m).

  # Replace empty strings with NA in coordinate columns before numeric parsing
  for (.col in intersect(c("lat", "long", "lat_wgs84", "long_wgs84"), names(raw))) {
    raw[[.col]][raw[[.col]] == ""] <- NA_character_
  }
  rm(.col)

  if ("lat_wgs84" %in% names(raw) && "long_wgs84" %in% names(raw)) {
    # 2019-2024: explicit WGS84 columns available
    raw$lat  <- .comma_to_numeric(raw$lat_wgs84)
    raw$long <- .comma_to_numeric(raw$long_wgs84)
  } else if (year >= 2025) {
    # 2025+: local metric CRS → convert to WGS84
    wgs84 <- .local_to_wgs84(
      suppressWarnings(as.numeric(gsub(",", ".", raw$lat))),
      suppressWarnings(as.numeric(gsub(",", ".", raw$long)))
    )
    raw$lat  <- wgs84$lat
    raw$long <- wgs84$long
  } else if (year >= 2014) {
    # 2014-2018: comma-decimal WGS84; 2016 has multi-factor scaling errors
    fixed    <- .recover_coords(.comma_to_numeric(raw$lat), .comma_to_numeric(raw$long))
    raw$lat  <- fixed$lat
    raw$long <- fixed$long
  } else {
    # 2011-2013: dot-decimal WGS84 with scaling errors (mainly ×10)
    fixed    <- .recover_coords(
      suppressWarnings(as.numeric(raw$lat)),
      suppressWarnings(as.numeric(raw$long))
    )
    raw$lat  <- fixed$lat
    raw$long <- fixed$long
  }

  # --- Year column ------------------------------------------------------------

  raw$year <- year

  # --- Select and order final columns -----------------------------------------

  dplyr::select(raw, dplyr::any_of(.SUACI_COLS))
}
