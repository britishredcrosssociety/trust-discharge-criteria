plotUI <- function(id) {
  plotOutput(
    NS(id, "plot"),
    width = "90%"
  )
}

plotServer <- function(id, clicked) {
  moduleServer(id, function(input, output, session) {

    # Render plots
    output$plot <- renderPlot(
      {
        # Render a blank plot upon loading the app
        if (is.null(clicked())) {
          criteria_to_reside |>
            filter(moving_average < .4) |>
            ggplot(aes(x = date, y = moving_average, group = nhs_trust22_code)) +
            geom_line(alpha = 0.1, colour = "#BBBBBB", show.legend = FALSE) +
            scale_y_continuous(labels = scales::percent) +
            theme(
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.grid.major = element_blank(),
            ) +
            labs(x = NULL, y = NULL)
        } else {
          # First, generate dynamic values based on selected Trust
          value_mean_group <-
            criteria_to_reside |>
            summarise(mean = mean(perc_not_meet_criteria)) |>
            pull(mean)

          label_mean_group <-
            as.character(round((value_mean_group * 100), 1))

          value_mean_clicked <-
            criteria_to_reside |>
            filter(nhs_trust22_code == clicked()) |>
            slice(1) |>
            pull(mean_perc_not_meet_criteria)

          label_mean_clicked <-
            as.character(round((value_mean_clicked * 100), 1))

          label_clicked_trust <-
            criteria_to_reside |>
            filter(nhs_trust22_code == clicked()) |>
            slice(1) |>
            pull(nhs_trust22_name)

          y_upper_limit <-
            criteria_to_reside |>
            filter(nhs_trust22_code == clicked()) |>
            filter(moving_average == max(moving_average)) |>
            mutate(y_limit = if_else(moving_average < .4, .4, moving_average)) |>
            slice(1) |>
            pull(y_limit)

          # Plot
          criteria_to_reside |>
            mutate(
              colour = if_else(nhs_trust22_code == clicked(), "#D0021B", "#BBBBBB"),
              alpha = if_else(nhs_trust22_code == clicked(), 1, 0.1),
            ) |>
            ggplot(
              aes(
                x = date,
                y = moving_average,
                group = nhs_trust22_code,
              )
            ) +
            geom_line(
              aes(alpha = alpha, colour = colour),
              show.legend = FALSE
            ) +
            geom_hline(
              yintercept = value_mean_group,
              linetype = "dashed",
              colour = "#2B7586",
            ) +
            geom_hline(
              yintercept = value_mean_clicked,
              linetype = "dashed",
              colour = "#CC434F"
            ) +
            scale_colour_manual(values = c("#BBBBBB", "#D0021B")) +
            scale_y_continuous(labels = scales::percent) +
            coord_cartesian(ylim = c(0, y_upper_limit)) +
            theme(
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.grid.major = element_blank(),
              plot.title = element_markdown(lineheight = 1.2),
              plot.title.position = "plot"
            ) +
            labs(
              x = NULL,
              y = NULL,
              title = glue("<span style = 'font-size:16pt; color:#5C747A;'>Solid red line shows 3-week moving average. Dotted lines show mean values.</span><br>
                            <span style = 'font-size:12pt; color:#2B7586;'>Mean of all trusts: {label_mean_group}%</span><br>
                            <span style = 'font-size:12pt; color:#D0021B;'>Mean of {label_clicked_trust}: {label_mean_clicked}%</span><br>")
            )
        }
      },
      bg = "transparent"
    )
  })
}

plotTest <- function() {
  ui <- fluidPage(
    plotUI("test")
  )

  server <- function(input, output, session) {
    clicked <- reactiveVal()

    plotServer("test", clicked)
  }

  shinyApp(ui, server)
}
