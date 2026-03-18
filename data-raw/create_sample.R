## code to prepare `suaci_sample` dataset

# Run from the package root with: source("data-raw/create_sample.R")
# Requires: devtools::load_all() or the package to be installed.

devtools::load_all()

set.seed(42)
raw_2024 <- suacir:::parse_suaci_csv(
  "data-raw/sistema-unico-de-atencion-ciudadana-2024.csv",
  2024
)

suaci_sample <- dplyr::slice_sample(raw_2024, n = 100)

usethis::use_data(suaci_sample, overwrite = TRUE)
