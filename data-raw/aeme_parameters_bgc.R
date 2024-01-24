## code to prepare `aeme_parameters_bgc` dataset goes here

aeme_parameters_bgc <- read.csv("data-raw/aeme_parameters_bgc.csv")

usethis::use_data(aeme_parameters_bgc, overwrite = TRUE)
