library(tidyverse)
library(glue)
library(geomtextpath)

pkgload::load_all(".")

value_mean <-
  criteria_to_reside |>
  filter(nhs_trust22_code == "R0B") |>
  slice(1) |>
  pull(mean_perc_not_meet_criteria)

label_mean <-
  as.character(round((value_mean * 100), 1))

value_highest <-
  criteria_to_reside |>
  filter(nhs_trust22_code == "R0B") |>
  filter(perc_not_meet_criteria == max(perc_not_meet_criteria)) |>
  slice(1) |>
  pull(perc_not_meet_criteria)

label_highest <-
  as.character(round((value_highest * 100), 1))

criteria_to_reside |>
  filter(perc_not_meet_criteria < .45) |>
  mutate(label = if_else(nhs_trust22_code == "R0B", nhs_trust22_name, NA_character_)) |>
  mutate(
    colour = if_else(nhs_trust22_code == "R0B", "#D0021B", "#BBBBBB"),
    alpha = if_else(nhs_trust22_code == "R0B", 1, 0.1),
  ) |>
  ggplot(
    aes(
      x = date,
      y = perc_not_meet_criteria,
      group = nhs_trust22_code,
      label = label
    )
  ) +
  geom_line(
    aes(alpha = alpha, colour = colour),
    show.legend = FALSE
  ) +
  geom_texthline(
    yintercept = value_mean,
    label = glue("Mean value: {label_mean}%"),
    linetype = "dashed",
    size = 6,
    colour = "#6A9EAA"
  ) +
  geom_texthline(
    yintercept = value_highest,
    label = glue("Highest value: {label_highest}%"),
    size = 6,
    colour = "#2B7586"
  ) +
  scale_colour_manual(values = c("#BBBBBB", "#D0021B")) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
  ) +
  labs(x = NULL, y = NULL)

# TODO:
# - Add plots lines back into app with dynamic values.