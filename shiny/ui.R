library(shiny)
library(plotly)
library(readr)

# Could just use https://shiny.posit.co/r/reference/shiny/latest/updateselectinput but I am lazy

taxi_zones <- read_csv("https://d37ci6vzurychx.cloudfront.net/misc/taxi+_zone_lookup.csv") # nolint

zones <- taxi_zones$LocationID
names(zones) <- taxi_zones$Zone

pickupOrDropoff <- c("PULocationID", "DOLocationID")
names(pickupOrDropoff) <- c("Pick up", "Drop off")

# Define UI for application
ui <- navbarPage("My Application",
  tabPanel("Report", fluidPage(
    titlePanel("Report"),
    sidebarLayout(
      sidebarPanel(
        a(href = "https://raw.githubusercontent.com/Nicklason/datavisualization/main/report.pdf", "Download PDF")
      ),
      mainPanel(
        h5("Some graphs takes longer to load please be patient. Enjoy!"),
      )
    )
  )),
  tabPanel("Taxi Fares Map", fluidPage(
    titlePanel("Taxi Fares Map"),
    sidebarLayout(
      sidebarPanel(
          selectInput("time_filter", "Select Time Filter", choices = c("Hour", "Week", "Month")),
          sliderInput("hour_slider", "Select Hour", min = 0, max = 23, value = 0),
          sliderInput("week_slider", "Select Week", min = 1, max = 52, value = 1),
          sliderInput("month_slider", "Select Month", min = 1, max = 12, value = 1),
          selectInput("zone_filter", "Select Zones", choices = zones, multiple = TRUE)
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
  )),
  tabPanel("Speed and distance", fluidPage(
    titlePanel("Speed and distance"),
    sidebarLayout(
      sidebarPanel(
        selectInput("locationsDistanceAndSpeed", "Select locations", choices = zones, multiple = TRUE),
        selectInput(inputId = "pickOrDropDistanceAndSpeed",
                    label = "Choose location type:",
                    choices = pickupOrDropoff),
        sliderInput("xRangeDistanceAndSpeed", "Select x axis range:", min = 0, max = 300, value = c(0, 300), step = 0.1),
        sliderInput("yRangeDistanceAndSpeed", "Select y axis range:", min = 0, max = 200, value = c(0, 200))
      ),
      mainPanel(
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"),
            plotOutput(
              "speedAndDistance",
              brush = brushOpts(
                id = "speedAndDistanceBrush",
                delay = 5000
              )
            ),
            plotOutput("speedAndDistancePie")
          )
        ),
        dataTableOutput("speedAndDistanceTable")
      )
    )
  )),
  tabPanel("Top 10 taxi locations over time", fluidPage(
    titlePanel("Top 10 taxi locations over time"),
    sidebarLayout(
      sidebarPanel(
        # Add any input controls if needed
      ),
      mainPanel(
        imageOutput("totalTripsFromPlacesOverTime")
      )
    )
  )),
  tabPanel("Speed", fluidPage(
    titlePanel("Speed"),
    sidebarLayout(
      sidebarPanel(
        # Add any input controls if needed
      ),
      mainPanel(
        plotOutput("speed")
      )
    )
  )),
  tabPanel("Speed based on amount of passengers", fluidPage(
    titlePanel("Speed based on amount of passengers"),
    sidebarLayout(
      sidebarPanel(
        # Add any input controls if needed
      ),
      mainPanel(
        plotOutput("speedBasedOnAmountOfPassengers")
      )
    )
  )),
  tabPanel("Distance traveled based on amount of passengers", fluidPage(
    titlePanel("Distance traveled based on amount of passengers"),
    sidebarLayout(
      sidebarPanel(
        # Add any input controls if needed
      ),
      mainPanel(
        plotOutput("distancedBasedOnAmountOfPassengers")
      )
    )
  ))
)
