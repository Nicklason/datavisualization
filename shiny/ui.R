library(shiny)
library(plotly)

# Define UI for application
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
  tabPanel("Taxi trips by zone", fluidPage(
    titlePanel("Taxi trips by zone"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "pickOrDrop",
                    label = "Choose location type:",
                    choices = c("Pick up", "Drop off"))
      ),
      mainPanel(
        plotOutput("taxiTripsByZone")
      )
    )
  )),
  tabPanel("Trip Count by Weekday and Hour", fluidPage(
    titlePanel("Trip Count by Weekday and Hour"),
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        plotOutput("taxiTripsByWeekday")
      )
    )
  )),
  tabPanel("Payment type distribution of taxi fares", fluidPage(
    titlePanel("Taxi fares distribution by type"),
    sidebarLayout(
      sidebarPanel(
        
      ),
      mainPanel(
        plotOutput("TaxiTripsByPayment")
      )
    )
  ))
)
