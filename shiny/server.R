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
# interactive ggplot
library(plotly)
# create intervals
library(classInt)
# string templating
library(glue)
# dates
library(lubridate)
# arrange multiple plots (and other things too?) in a grid
library(gridExtra)

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

# Extract hour and weekday information
taxi_data$pickup_hour <- hour(taxi_data$tpep_pickup_datetime)
taxi_data$pickup_weekday <- wday(taxi_data$tpep_pickup_datetime, label = TRUE)

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
  
  locationID <- reactive({
    switch(input$pickOrDrop,
           "Pick up" = "PULocationID",
           "Drop off" = "DOLocationID")
  })
  
  output$taxiTripsByZone <- renderPlot({
    # Extract the current value of the reactive locationID
    current_location <- locationID()

    # Convert the table object into a list
    tripsByZone <- taxi_data %>%
      group_by(!!sym(current_location)) %>%
      summarise(trips = n()) %>%
      mutate(across(all_of(current_location), as.character)) %>%
      rename_with(~ "location_id", .cols = all_of(current_location))
    
    totalAmountByZone <- taxi_data %>%
      group_by(!!sym(current_location)) %>%
      summarise(money = sum(total_amount)) %>%
      mutate(across(all_of(current_location), as.character)) %>%
      rename_with(~ "location_id", .cols = all_of(current_location))

    grouped <- tripsByZone %>%
      # Join amount of trips by zone
      inner_join(totalAmountByZone, by = c("location_id" = "location_id")) %>%
      # Join simple feature from shape
      inner_join(taxi_shp, by = c("location_id" = "location_id"))
        
    # For some reason I have to specify geometry = geometry
    ggplot() +
      geom_sf(data = grouped, aes(group = location_id, fill = trips, geometry = geometry), color = "white") +
      scale_fill_distiller(palette = "OrRd", name = "Frequency", trans = "reverse") +
      coord_sf() +
      theme_void()
  })

  output$taxiTripsByWeekday <- renderPlot({
    passengers <- taxi_data %>%
      group_by(pickup_weekday, pickup_hour) %>%
      summarise(trips = n())

    ggplot(passengers, aes(x = pickup_hour, y = trips, group = pickup_weekday, color = pickup_weekday)) +
      geom_line() +
      labs(x = "Hour of Day", y = "Total trips", title = "Trip Count by Weekday and Hour") +
      scale_x_continuous(breaks = seq(0, 23, by = 1)) +
      scale_color_discrete(name = "Weekday")
  })
  output$TaxiTripsByPayment <- renderPlot({
    
    # Extract the current value of the reactive locationID
    current_location <- locationID()
    
    # Filter only relevant payment types (adjust the values as needed)
    relevant_payment_types <- c("1", "2")
    taxi_data_filtered <- taxi_data %>% filter(payment_type %in% relevant_payment_types)
    
    # Group by payment type and location
    tripsByPaymentType <- taxi_data_filtered %>%
      group_by(payment_type, !!sym(current_location)) %>%
      summarise(trips = n()) %>%
      mutate(across(all_of(current_location), as.character)) %>%
      rename_with(~ "location_id", .cols = all_of(current_location)) %>%
      distinct()  # Address many-to-many relationship warning
    
    # Join simple feature from shape
    grouped <- inner_join(taxi_shp, tripsByPaymentType, by = c("location_id" = "location_id"))
    
    # Plot for most common payment type at each location
    plot_location <- ggplot(grouped, aes(group = location_id, fill = factor(payment_type), geometry = geometry)) +
      geom_sf(color = "white") +
      scale_fill_manual(name = "Payment_type", values = c("1" = "blue", "2" = "red"), labels = c("Credit Card", "Cash")) +
      theme_void() +
      ggtitle("Most Common Payment Type at Each Location")
    
    # Overall total
    total_by_payment <- taxi_data_filtered %>%
      group_by(payment_type) %>%
      summarise(total_trips = n())
    
    # Plot for overall total
    plot_total <- ggplot(total_by_payment, aes(x = factor(payment_type), y = total_trips, fill = factor(payment_type))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(name = "Payment_type", values = c("1" = "blue", "2" = "red"), labels = c("Credit Card", "Cash")) +
      theme_minimal() +
      ggtitle("Overall Total Trips by Payment Type")
    
    # Arrange the two plots
    grid.arrange(plot_location, plot_total, ncol = 2)
  })
  
}
