library(shiny)
library(plotly)

ui <- navbarPage("My Application",
  tabPanel("Taxi Fares Map", fluidPage(
    titlePanel("Taxi Fares Map"),
    sidebarLayout(
      sidebarPanel(
        # Add any input controls if needed
      ),
      mainPanel(
        plotlyOutput("taxiPlot")
      )
    )
  )),
  tabPanel("Component 2"),
  tabPanel("Component 3")
)
