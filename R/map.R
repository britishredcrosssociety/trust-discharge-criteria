mapUI <- function(id) {
  leafletOutput(
    NS(id, "map"),
    width = "100%",
    height = "100%"
  )
}

mapServer <- function(id, clicked) {
  moduleServer(id, function(input, output, session) {

    # Identify worse performing trusts to assign marker colours
    worst_performers <-
      criteria_to_reside |>
      distinct(nhs_trust22_code, mean_perc_not_meet_criteria) |>
      arrange(desc(mean_perc_not_meet_criteria)) |>
      slice_head(prop = .1) |>
      pull(nhs_trust22_code)

    output$map <-
      renderLeaflet({
        leaflet(options = leafletOptions(zoomControl = FALSE)) |>
          setView(lat = 54, lng = -2.0, zoom = 7) |>
          addProviderTiles(
            providers$CartoDB.Positron,
            options = providerTileOptions(minZoom = 7)
          ) |>
          setMaxBounds(-12, 49, 3.0, 61) |>
          htmlwidgets::onRender(
            "function(el, x) {
            L.control.zoom({position:'bottomleft'}).addTo(this);
             }"
          ) |>
          addAwesomeMarkers(
            data = points_nhs_trusts22 |> filter(nhs_trust22_code %in% worst_performers),
            layerId = ~nhs_trust22_code,
            popup = ~nhs_trust22_name,
            label = ~nhs_trust22_name,
            icon = awesomeIcons(
              icon = "hospital-o",
              library = "fa",
              markerColor = "black",
              iconColor = "#FFFFFF"
            )
          ) |> 
          addAwesomeMarkers(
            data = points_nhs_trusts22 |> filter(!(nhs_trust22_code %in% worst_performers)),
            layerId = ~nhs_trust22_code,
            popup = ~nhs_trust22_name,
            label = ~nhs_trust22_name,
            icon = awesomeIcons(
              icon = "hospital-o",
              library = "fa",
              markerColor = "cadetblue",
              iconColor = "#FFFFFF"
            )
          )
      })

    observeEvent(input$map_marker_click, {
      input$map_marker_click$id |>
        clicked()
    })
  })
}

mapTest <- function() {
  ui <- bootstrapPage(
    tags$head(
      tags$style(type = "text/css", "html, body {width:100%;height:100%;}")
    ),
    mapUI("test")
  )
  server <- function(input, output, session) {
    clicked <- reactiveVal()

    mapServer("test", clicked)

    observe({
      print(clicked())
    })
  }
  shinyApp(ui, server)
}
