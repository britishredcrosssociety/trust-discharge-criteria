plotUI <- function(id) {
  plotOutput(
    NS(id, "plot")
  )
}

plotServer <- function(id, clicked) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot(
      {
        if (is.null(clicked())) {
          criteria_to_reside |>
            filter(perc_not_meet_criteria < .45) |>
            ggplot(aes(x = date, y = perc_not_meet_criteria, group = nhs_trust22_code)) +
            geom_line(alpha = 0.1, colour = "#BBBBBB", show.legend = FALSE) +
            scale_y_continuous(labels = scales::percent) +
            theme(
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.grid.major = element_blank(),
            ) +
            labs(x = NULL, y = NULL)
        } else {
          criteria_to_reside |>
            filter(perc_not_meet_criteria < .45) |>
            mutate(
              colour = if_else(nhs_trust22_code == clicked(), "#D0021B", "#BBBBBB"),
              alpha = if_else(nhs_trust22_code == clicked(), 1, 0.1),
            ) |>
            ggplot(aes(x = date, y = perc_not_meet_criteria, group = nhs_trust22_code)) +
            geom_line(aes(alpha = alpha, colour = colour), show.legend = FALSE) +
            scale_colour_manual(values = c("#BBBBBB", "#D0021B")) +
            scale_y_continuous(labels = scales::percent) +
            theme(
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.grid.major = element_blank(),
            ) +
            labs(x = NULL, y = NULL)
        }
      },
      bg = "transparent"
    )
  })
}