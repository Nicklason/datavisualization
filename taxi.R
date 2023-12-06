# Read Simple Feature (SF) file
library(sf)
# Read Parquet file
library(arrow)
# Read CSV file
library(readr)
# Functions to work with dataframes
library(dplyr)
# Create plots
library(ggplot2)
# replace_na function
library(tidyr)
# shiny app
library(shiny)
# interactive ggplot
library(plotly)
# create intervals
library(classInt)
# string templating
library(glue)

# Read the geojson file
taxi_shp <- read_sf('https://data.cityofnewyork.us/api/geospatial/d3c5-ddgc?method=export&format=GeoJSON') # nolint
# Read the parquet file
taxi_data <- read_parquet('https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2023-01.parquet') # nolint
# Read the taxi zone lookup file
taxi_zones <- read_csv("https://d37ci6vzurychx.cloudfront.net/misc/taxi+_zone_lookup.csv") # nolint

# Calculate points at which to plot labels (https://stackoverflow.com/a/50860504/9698208) # nolint
centroids <- taxi_shp %>% 
  st_centroid() %>% 
  bind_cols(as_data_frame(st_coordinates(.)))

  # Define UI for application
ui <- fluidPage(
  titlePanel("Taxi Fares Map"),
  sidebarLayout(
    sidebarPanel(
      # Add any input controls if needed
    ),
    mainPanel(
      plotlyOutput("taxiPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$taxiPlot <- renderPlotly({
    # Calculate the total amount of money spent in each location
    total <- merge(
      aggregate(taxi_data$total_amount, by=list(location_id=taxi_data$DOLocationID), FUN=sum),
      aggregate(taxi_data$total_amount, by=list(location_id=taxi_data$PULocationID), FUN=sum),
      by="location_id",
      all.x=TRUE,
      all.y=TRUE
    ) %>%
    mutate_at(c('x.x','x.y'), ~replace_na(.,0)) %>%
    mutate(money = x.x + x.y, x.x = NULL, x.y = NULL) %>%
    mutate_at(c('location_id'), as.character) %>%
    left_join(taxi_shp, ., by = c('location_id' = 'location_id'))

    # Scale money down to not show scientific notation numbers
    total$money_scaled = total$money / 100000
    
    # Create 7 intervals for the monies
    breaks_qt <- classIntervals(c(0, total$money_scaled), n = 7, style = "quantile")

    # Use the intervals
    total <- mutate(total, money_cat = cut(money_scaled, breaks_qt$brks))

    # Add label to each row
    total <- total %>%
      mutate(
        model = rownames(.),
        label = glue::glue('{zone} \nMoney spent: ${money}' )
      )

    # Create plot
    p <- ggplot() +
      geom_sf(data = total, aes(group = location_id, fill = money_cat, text = label), color = "white") +
    scale_colour_distiller(palette="Reds", name="Frequency", guide = "colorbar") +
    coord_sf() +
    theme_void() +
      scale_fill_brewer(palette = "OrRd")

    # Make the plot interactive
    ggplotly(p, tooltip = "text", height = 1000)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)