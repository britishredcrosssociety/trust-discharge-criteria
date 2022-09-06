library(tidyverse)
library(glue)
library(geomtextpath)

pkgload::load_all(".")

value_mean_group <-
  criteria_to_reside |>
  summarise(mean = mean(perc_not_meet_criteria)) |>
  pull(mean)

label_mean_group <-
  as.character(round((value_mean_group * 100), 1))

value_mean_clicked <-
  criteria_to_reside |>
  filter(nhs_trust22_code == "R0B") |>
  slice(1) |>
  pull(mean_perc_not_meet_criteria)

label_mean_clicked <-
  as.character(round((value_mean_clicked * 100), 1))

label_clicked_trust <-
  criteria_to_reside |>
  filter(nhs_trust22_code == "R0B") |>
  slice(1) |>
  pull(nhs_trust22_name)

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
    yintercept = value_mean_group,
    label = glue("Mean (all trusts): {label_mean_group}%"),
    linetype = "dashed",
    size = 5,
    colour = "#2B7586",
  ) +
  geom_texthline(
    yintercept = value_mean_clicked,
    label = glue("Mean (selected trust): {label_mean_clicked}%"),
    linetype = "dashed",
    size = 5,
    colour = "#CC434F"
  ) +
  scale_colour_manual(values = c("#BBBBBB", "#D0021B")) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
  ) +
  labs(x = NULL, y = NULL, title = glue("Selected trust: {label_clicked_trust}"))

# TODO:
# - Add coloured and bold titled with ggtext.
