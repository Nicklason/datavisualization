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
          selectInput("time_filter", "Select Time Filter", choices = c("Hour", "Week", "Month")),
          sliderInput("hour_slider", "Select Hour", min = 0, max = 23, value = 0),
          sliderInput("week_slider", "Select Week", min = 1, max = 52, value = 1),
          sliderInput("month_slider", "Select Month", min = 1, max = 12, value = 1),
          selectInput("zone_filter", "Select Zone", choices = unique(taxi_data$zone))
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
