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
taxi_zones <- read_csv("https://d37ci6vzurychx.cloudfront.net/misc/taxi+_zone_lookup.csv") %>% # nolint
  mutate(across("LocationID", as.integer))

# Extract hour and weekday information
taxi_data$pickup_hour <- hour(taxi_data$tpep_pickup_datetime)
taxi_data$pickup_weekday <- wday(taxi_data$tpep_pickup_datetime, label = TRUE)

airport_location_ids <- c(1, 132, 138)

# Filter dataset to only include trips to/from airports
taxi_data_airports <- taxi_data %>%
  filter(PULocationID %in% airport_location_ids | DOLocationID %in% airport_location_ids)

trip_time <- as.numeric(difftime(taxi_data$tpep_dropoff_datetime, taxi_data$tpep_pickup_datetime, units = "hours"))
trip_distance <- taxi_data$trip_distance * 1.609344
trip_speed <- trip_distance / trip_time

trip_time_distance_speed <- data.frame(trip_time = trip_time, trip_distance = trip_distance, trip_speed = trip_speed, PULocationID = taxi_data$PULocationID, DOLocationID = taxi_data$DOLocationID) %>%
  filter(trip_time > 0 & trip_distance > 0 & trip_speed < 130)

# Calculate points at which to plot labels (https://stackoverflow.com/a/50860504/9698208) # nolint
centroids <- taxi_shp %>% 
  st_centroid() %>% 
  bind_cols(as_data_frame(st_coordinates(.)))

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
    
    # Create 7 intervals for the monies
    breaks_qt <- classIntervals(c(0, total$money), n = 7, style = "quantile")

    # Use the intervals
    total <- mutate(total, money_cat = cut(money, breaks_qt$brks))

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
      scale_color_discrete(name = "Weekday") +
      geom_vline(xintercept = 0) +
      annotate(geom = "text", x = 1, y = 28000, label = "Weekend peak", color = "red") +
      geom_vline(xintercept = 6) + 
      annotate(geom = "text", x = 7, y = 0, label = "Weekend bottom", color = "red") +
      geom_vline(xintercept = 19) +
      annotate(geom = "text", x = 20, y = 38000, label = "Workday peak", color = "red")
  })

  output$taxiTripsAirports <- renderPlotly({
    p <- ggplot() +
      geom_sf(data = taxi_shp, aes(group = objectid), color = "white") +
      coord_sf() +
      theme_void()

    if (isTruthy(input$locations)) {
      trips <- taxi_data_airports %>%
        filter(PULocationID %in% airport_location_ids)

      airportTrips <- trips %>%
        filter(PULocationID != DOLocationID & (DOLocationID %in% input$locations | PULocationID %in% input$locations)) %>%
        mutate(across(PULocationID, as.character)) %>%
        mutate(across(DOLocationID, as.character)) %>%
        group_by(PULocationID, DOLocationID) %>%
        summarise(trips = n()) %>%
        filter(trips >= input$minMaxTrips[1] & trips <= input$minMaxTrips[2])

      arrows <- airportTrips %>%
        inner_join(centroids, by = c("PULocationID" = "location_id"), suffix = c("", "PU")) %>%
        inner_join(centroids, by = c("DOLocationID" = "location_id"), suffix = c("PU", "DO"))

      p <- p +
        geom_segment(
          data = arrows,
          aes(
            x = XPU,
            y = YPU,
            xend = XDO,
            yend = YDO,
            color = trips
          ),
          size = 0.4,
          alpha = 0.5,
          arrow = arrow(length = unit(0.01, "npc")),
          default_crs = sf::st_crs(4326)
        )
    }

    ggplotly(p, height = 1000)
  })

  speedAndDistanceBrush <- NULL
  makeReactiveBinding("speedAndDistanceBrush")

  observeEvent(input$speedAndDistanceBrush, {
    speedAndDistanceBrush <<- input$speedAndDistanceBrush
  })

  output$speedAndDistance <- renderPlot({
    stuff <- trip_time_distance_speed %>%
      filter((!!sym(input$pickOrDropDistanceAndSpeed) %in% input$locationsDistanceAndSpeed)) %>%
      filter(
        trip_distance >= input$xRangeDistanceAndSpeed[1] &
        trip_distance <= input$xRangeDistanceAndSpeed[2] &
        trip_speed >= input$yRangeDistanceAndSpeed[1] &
        trip_speed <= input$yRangeDistanceAndSpeed[2]
      )

    brushed <- brushedPoints(stuff, speedAndDistanceBrush) %>%
      select(-one_of(input$pickOrDropDistanceAndSpeed))

    if (input$pickOrDropDistanceAndSpeed == "DOLocationID") {
      brushed <- brushed %>%
        rename(LocationID = PULocationID)
    } else {
      brushed <- brushed %>%
        rename(LocationID = DOLocationID)
    }

    brushed <- brushed %>% mutate(across(LocationID, as.character))

    ggplot() +
      geom_point(stuff, mapping = aes(x = trip_distance, y = trip_speed), size=0.2) +
      geom_point(brushed, mapping = aes(x = trip_distance, y = trip_speed, color = LocationID), size=0.2)
  })

  output$speedAndDistancePie <- renderPlot({
    stuff <- trip_time_distance_speed %>%
      filter((!!sym(input$pickOrDropDistanceAndSpeed) %in% input$locationsDistanceAndSpeed))

    filtered <- brushedPoints(stuff, speedAndDistanceBrush)

    if (nrow(filtered) > 0) {
      # calculate percentages of groups of locations (pu or do)

      if (input$pickOrDropDistanceAndSpeed == "DOLocationID") {
        filtered <- filtered %>%
          rename(LocationID = PULocationID)
      } else {
        filtered <- filtered %>%
          rename(LocationID = DOLocationID)
      }

      result <- filtered %>%
        mutate(across(LocationID, as.character)) %>%
        group_by(LocationID) %>%
        summarise(count = n()) %>%
        mutate(percentage = count / sum(count) * 100)

      p <- ggplot(result, aes(x="", y=count, fill=LocationID)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0)

      # this is cool. you can extract the color from a plot, then sort categories lexicographically because that is how ggplot sorts it
      # that can then be used to map categories and colors
      #plot_data <- ggplot_build(p)$data[[1]]  # Extract data used for plotting
      #category_colors <- plot_data$fill  # Extract fill colors used for categories
      #unique_categories <- sort(as.character(unique(filtered$LocationID)))  # Get unique categories

      # Map colors to categories
      #color_map <- data.frame(category = unique_categories, color = category_colors)

      #message(color_map)

      p
    }
  })

  output$speedAndDistanceTable <- renderDataTable({
    stuff <- trip_time_distance_speed %>%
      filter((!!sym(input$pickOrDropDistanceAndSpeed) %in% input$locationsDistanceAndSpeed))

    brushedPoints(stuff, speedAndDistanceBrush) %>%
      select(-one_of(input$pickOrDropDistanceAndSpeed))
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
    
    # Calculate the total trips for each location
    total_trips_by_location <- tripsByPaymentType %>%
      group_by(location_id) %>%
      summarise(total_trips = sum(trips))
    
    # Calculate the percentage of trips for each payment type at each location
    tripsByPaymentType <- tripsByPaymentType %>%
      left_join(total_trips_by_location, by = "location_id") %>%
      mutate(percentage = (trips / total_trips) * 100)
    
    # Join simple feature from shape
    grouped <- inner_join(taxi_shp, tripsByPaymentType, by = c("location_id" = "location_id"))
    
    # Plot for most common payment type at each location as a heatmap
    plot_location <- ggplot(grouped, aes(group = location_id, fill = percentage, geometry = geometry)) +
      geom_sf(color = "white") +
      scale_fill_gradient(name = "Percentage", low = "blue", high = "orange") +
      theme_void() +
      ggtitle("% of Trips by Payment Type at each zone")
    
    # Overall total
    total_by_payment <- taxi_data_filtered %>%
      group_by(payment_type) %>%
      summarise(total_trips = n())
    
    # Plot for overall total
    plot_total <- ggplot(total_by_payment, aes(x = factor(payment_type), y = total_trips, fill = factor(payment_type))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(name = "Payment_type", values = c("1" = "blue", "2" = "orange"), labels = c("Credit Card", "Cash")) +
      theme_minimal() +
      ggtitle("Overall Total Trips by Payment Type")
    
    # Arrange the two plots
    grid.arrange(plot_location, plot_total, ncol = 2)
  })
  
  
}
