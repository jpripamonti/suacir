fix <- function(name) {
  testthat::test_path("fixtures", name)
}

# --- suaci_years() ------------------------------------------------------------

test_that("suaci_years() returns an integer vector", {
  years <- suaci_years()
  expect_type(years, "integer")
})

test_that("suaci_years() contains expected range", {
  years <- suaci_years()
  expect_true(2011L %in% years)
  expect_true(2025L %in% years)
  expect_false(2010L %in% years)
  expect_false(2026L %in% years)
})

# --- parse_suaci_csv(): column names ------------------------------------------

test_that("old-format (2016) columns are renamed to unified schema", {
  df <- suacir:::parse_suaci_csv(fix("fixture_2016.csv"), 2016)
  expect_true("categoria"      %in% names(df))
  expect_true("prestacion"     %in% names(df))
  expect_true("tipo"           %in% names(df))
  expect_true("comuna"         %in% names(df))
  expect_true("barrio"         %in% names(df))
  expect_true("calle"          %in% names(df))
  expect_false("rubro"         %in% names(df))
  expect_false("concepto"      %in% names(df))
  expect_false("tipo_prestacion" %in% names(df))
  expect_false("domicilio_cgp" %in% names(df))
})

test_that("new-format (2019) keeps unified column names", {
  df <- suacir:::parse_suaci_csv(fix("fixture_2019.csv"), 2019)
  expect_true("nro_solicitud"  %in% names(df))
  expect_true("categoria"      %in% names(df))
  expect_true("prestacion"     %in% names(df))
  expect_true("canal"          %in% names(df))
  expect_true("genero"         %in% names(df))
  expect_true("estado_general" %in% names(df))
})

test_that("all output tibbles have the year column", {
  for (yr in c(2016, 2019, 2024, 2025)) {
    fname <- paste0("fixture_", yr, ".csv")
    df <- suacir:::parse_suaci_csv(fix(fname), yr)
    expect_true("year" %in% names(df), label = paste("year column in", yr))
  }
})

# --- parse_suaci_csv(): column types -----------------------------------------

test_that("fecha_ingreso is Date for all fixture years", {
  for (yr in c(2016, 2019, 2024, 2025)) {
    fname <- paste0("fixture_", yr, ".csv")
    df <- suacir:::parse_suaci_csv(fix(fname), yr)
    expect_s3_class(df$fecha_ingreso, "Date")
  }
})

test_that("lat and long are numeric for all fixture years", {
  for (yr in c(2016, 2019, 2024, 2025)) {
    fname <- paste0("fixture_", yr, ".csv")
    df <- suacir:::parse_suaci_csv(fix(fname), yr)
    expect_type(df$lat,  "double")
    expect_type(df$long, "double")
  }
})

test_that("year column is integer", {
  df <- suacir:::parse_suaci_csv(fix("fixture_2019.csv"), 2019)
  expect_type(df$year, "integer")
  expect_true(all(df$year == 2019L))
})

# --- parse_suaci_csv(): date correctness --------------------------------------

test_that("ISO dates (2019) are parsed correctly", {
  df <- suacir:::parse_suaci_csv(fix("fixture_2019.csv"), 2019)
  expect_equal(df$fecha_ingreso[1], as.Date("2019-06-05"))
})

test_that("d/m/yyyy dates (2024) are parsed correctly", {
  df <- suacir:::parse_suaci_csv(fix("fixture_2024.csv"), 2024)
  # 4/8/2024 -> 2024-08-04
  expect_equal(df$fecha_ingreso[1], as.Date("2024-08-04"))
  # 8/3/2024 -> 2024-03-08
  expect_equal(df$fecha_ingreso[2], as.Date("2024-03-08"))
})

# --- parse_suaci_csv(): coordinates ------------------------------------------

test_that("2016 coordinates are parsed from comma-decimal lat/long", {
  df <- suacir:::parse_suaci_csv(fix("fixture_2016.csv"), 2016)
  expect_true(all(df$lat  > -40 & df$lat  < -30, na.rm = TRUE))
  expect_true(all(df$long > -62 & df$long < -54, na.rm = TRUE))
})

test_that("2019 coordinates come from lat_wgs84/long_wgs84", {
  df <- suacir:::parse_suaci_csv(fix("fixture_2019.csv"), 2019)
  expect_equal(df$lat[1],  -34.583759, tolerance = 1e-4)
  expect_equal(df$long[1], -58.425011, tolerance = 1e-4)
})

# --- parse_suaci_csv(): old-format NA columns --------------------------------

test_that("old-format years have NA in new-only columns", {
  df <- suacir:::parse_suaci_csv(fix("fixture_2016.csv"), 2016)
  expect_true(all(is.na(df$nro_solicitud)))
  expect_true(all(is.na(df$canal)))
  expect_true(all(is.na(df$genero)))
  expect_true(all(is.na(df$estado_general)))
})

# --- parse_suaci_csv(): 2016 duplicate columns --------------------------------

test_that("2016 duplicate lat/long columns are deduplicated", {
  df <- suacir:::parse_suaci_csv(fix("fixture_2016.csv"), 2016)
  expect_equal(sum(names(df) == "lat"),  1L)
  expect_equal(sum(names(df) == "long"), 1L)
})

# --- parse_suaci_csv(): 2025 comma delimiter ----------------------------------

test_that("2025 comma-delimited file is parsed correctly", {
  df <- suacir:::parse_suaci_csv(fix("fixture_2025.csv"), 2025)
  expect_gt(nrow(df), 0L)
  expect_equal(df$nro_solicitud[1], "00300553/25")
  expect_s3_class(df$fecha_ingreso, "Date")
})

# --- suaci_get(): input validation --------------------------------------------

test_that("suaci_get() errors on unavailable year", {
  expect_error(suaci_get(2000), regexp = "not available")
  expect_error(suaci_get(2026), regexp = "not available")
})
