library(tidyverse)
library(geographr)
library(sf)

pkgload::load_all(".")

points_nhs_trusts22 <-
  points_nhs_trusts22 |>
  filter(nhs_trust22_code %in% criteria_to_reside$nhs_trust22_code)

usethis::use_data(points_nhs_trusts22, overwrite = TRUE)