# ---- Load libs ----
library(shiny)
library(ggplot2)
library(leaflet)

# ---- App function ----
app <- function() {

  # ---- UI ----
  ui <- fluidPage()

  # ---- Server ----
  server <- function(input, output, session) {
  }

  shinyApp(ui, server)
}
