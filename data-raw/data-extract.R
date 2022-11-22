# ---- Create a data extract to share with the surge team ----
# Not to be included in the app - added to .rscignore

library(tidyverse)
library(geographr)
library(sf)

devtools::load_all()

criteria_to_reside_mean <-
  criteria_to_reside |>
  select(
    nhs_trust22_code,
    mean_perc_not_meet_criteria
  ) |>
  distinct()

criteria_by_region <- criteria_to_reside_mean |>
  left_join(lookup_nhs_trusts22_nhs_region21, by = "nhs_trust22_code") |>
  mutate(mean_perc_not_meet_criteria = round(mean_perc_not_meet_criteria * 100, 1)) |>
  select(
    `Trust name` = nhs_trust22_name,
    `NHS region` = nhs_region21_name,
    `% not meeting criteria to reside (Apr-Oct)` = mean_perc_not_meet_criteria
  ) |>
  group_by(`NHS region`) |>
  arrange(desc(`% not meeting criteria to reside (Apr-Oct)`), .by_group = TRUE) |>
  ungroup()

write_csv(criteria_by_region, "data-raw/data-extract.csv")
