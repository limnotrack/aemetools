aeme_parameters <- read.csv("data-raw/aeme_parameters.csv") |>
  dplyr::mutate(group = NA)
usethis::use_data(aeme_parameters, overwrite = TRUE)
