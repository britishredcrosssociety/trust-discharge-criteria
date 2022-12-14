# ---- load libs ----
library(tidyverse)
library(lubridate)
library(healthyr)
library(geographr)
library(sf)

# ---- Trust names ----
trust_names <-
  points_nhs_trusts22 |>
  st_drop_geometry() |>
  select(starts_with("nhs_"))

# ---- Calculate percentage of beds ----
# The hospital discharge data states:
# This SitRep collects data for all inpatients 18 and over including critical
# care and COVID-19 positive patients but excluding paediatrics, maternity, and
# deceased patients. This includes data for acute trusts with a type 1 A&E
# department. Mental Health Trusts, specialised Trusts (including Children’s and
# Women’s Trusts) are not in scope of this collection.

# Match available bed numbers to requirements above (18+ including critical)
available_beds <-
  england_critical_general_acute_beds |>
  select(
    nhs_trust22_code,
    date,
    general_acute_beds_available,
    adult_critical_care_beds_available
  ) |>
  mutate(available_beds = general_acute_beds_available + adult_critical_care_beds_available) |>
  select(nhs_trust22_code, month = date, available_beds)

# Divide criteria to reside by bed availability, matching by month and then
# calculate a 3-week moving average
criteria_to_reside <-
  england_criteria_to_reside |>
  mutate(
    month = str_c(
      as.character(month(date, label = TRUE, abbr = FALSE)),
      " 2022"
    )
  ) |>
  left_join(available_beds) |>
  mutate(perc_not_meet_criteria = do_not_meet_criteria_to_reside / available_beds) |>
  select(-month) |>
  left_join(trust_names) |>
  relocate(nhs_trust22_name) |>
  group_by(nhs_trust22_code) |>
  mutate(mean_perc_not_meet_criteria = mean(perc_not_meet_criteria)) |>
  mutate(
    moving_average = slider::slide_index_mean(
      perc_not_meet_criteria,
      i = date,
      before = 21
    )
  ) |>
  ungroup()

usethis::use_data(criteria_to_reside, overwrite = TRUE)
