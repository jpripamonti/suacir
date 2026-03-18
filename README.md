# suacir

**suacir** is an R package for downloading and analysing data from the
[Sistema Único de Atención Ciudadana (SUACI)](https://data.buenosaires.gob.ar/dataset/sistema-unico-atencion-ciudadana),
published by the Buenos Aires City Government on the Buenos Aires Data portal.
The dataset covers citizen requests, complaints, and enquiries received
by the city from 2011 to the present.

## Installation

```r
remotes::install_github("jpripamonti/suacir")
```

## Usage

```r
library(suacir)

# Which years are available?
suaci_years()
#>  [1] 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025

# Download a single year
df_2023 <- suaci_get(2023)
dplyr::glimpse(df_2023)

# Download all years (large download — several GB)
all_data <- suaci_get_all()

# Use the built-in sample (100 rows, no internet required)
dplyr::glimpse(suaci_sample)
```

## Output columns

All tibbles returned by `suaci_get()` and `suaci_get_all()` share a consistent
schema regardless of the source year:

| Column | Type | Description |
|---|---|---|
| `nro_solicitud` | character | Request number (`NA` for 2011–2018) |
| `periodo` | character | Period code (`yyyymm`) |
| `categoria` | character | Service category |
| `prestacion` | character | Specific service |
| `tipo` | character | Request type (Reclamo, Solicitud, etc.) |
| `fecha_ingreso` | Date | Date received |
| `hora_ingreso` | character | Time received |
| `comuna` | character | Administrative district |
| `barrio` | character | Neighbourhood |
| `calle` | character | Street name |
| `altura` | character | Street number |
| `esquina_proxima` | character | Nearest cross street |
| `canal` | character | Intake channel (`NA` for 2011–2018) |
| `lat` | numeric | WGS84 latitude |
| `long` | numeric | WGS84 longitude |
| `genero` | character | Requester gender (`NA` for 2011–2018) |
| `estado_general` | character | Request status (`NA` for 2011–2018) |
| `year` | integer | Source year |

### Notes on coordinates

- **2011–2015**: `lat`/`long` are stored with dot decimal separators in the
  source files but contain data quality issues (some values appear scaled by
  a factor of 10). Use with caution.
- **2016–2018**: `lat`/`long` are correct WGS84 degrees.
- **2019–2024**: coordinates are sourced from the `lat_wgs84`/`long_wgs84`
  columns in the original files.
- **2025**: `lat`/`long` columns are present but largely empty.

## Data source

Buenos Aires Data portal — Sistema Único de Atención Ciudadana:
<https://data.buenosaires.gob.ar/dataset/sistema-unico-atencion-ciudadana>

## License

MIT
