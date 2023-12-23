library(shiny)
library(plotly)
library(readr)

# Could just use https://shiny.posit.co/r/reference/shiny/latest/updateselectinput but I am lazy

taxi_zones <- read_csv("https://d37ci6vzurychx.cloudfront.net/misc/taxi+_zone_lookup.csv") # nolint

zones <- taxi_zones$LocationID
names(zones) <- taxi_zones$Zone


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
  )),
  tabPanel("Taxi trips to and from airports", fluidPage(
    titlePanel("Taxi trips to and from airports"),
    sidebarLayout(
      sidebarPanel(
        selectInput("locations", "Select locations to highlight", choices = zones, multiple = TRUE),
        sliderInput("minMaxTrips", "Minimum and maximum trips", 0, 10000, step=100, value = c(0, 10000))
      ),
      mainPanel(
        plotlyOutput("taxiTripsAirports")
      )
    )
  ))
)
