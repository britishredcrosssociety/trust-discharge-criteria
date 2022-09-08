# ---- Load libs ----
library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)
library(ggtext)
library(glue)
library(sf)

# ---- App function ----
criteria <- function() {

  # ---- UI ----
  ui <- bootstrapPage(

    # - Add tags to render map fullscreen -
    tags$head(
      tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
      tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
      tags$style("h1 {color:#262626;}"),
      tags$style("h4 {color:#5C747A;}"),
      tags$style("p {color:#9CAAAE;}"),
      tags$style("a {color:#9CAAAE;}")
    ),

    # - Call map module -
    mapUI("map"),

    # - Title & subheading -
    absolutePanel(
      top = 10, left = 20, style = "z-index:500; text-align: left;",
      tags$h1("Patients Not Meeting Criteria to Reside"),
      tags$h4("Click on an NHS Trust to update the plot."),
      tags$p(
        "British Red Cross analysis of ",
        tags$a(href = "https://www.england.nhs.uk/statistics/statistical-work-areas/", target = "_blank", "NHS"),
        "discharge and beds data."
      ),
      plotUI("plot")
    )
  )

  # ---- Server ----
  server <- function(input, output, session) {

    # Create empty reactive to track state to pass between modules
    clicked <- reactiveVal()

    # Call module servers
    mapServer("map", clicked = clicked)
    plotServer("plot", clicked = clicked)
  }

  shinyApp(ui, server)
}