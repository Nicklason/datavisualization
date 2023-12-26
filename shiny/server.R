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
# create animations
library(gganimate)
# create gifs
library(gifski)
library(tidyverse)

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

  output$totalTripsFromPlacesOverTime <- renderImage({
    # Get day of month
    taxi_data$day <- day(taxi_data$tpep_pickup_datetime)

    # Get top 10 places each day
    data <- taxi_data %>%
      group_by(day, PULocationID) %>%
      summarise(count = n()) %>%
      group_by(PULocationID) %>%
      mutate(cumulative_count = cumsum(count)) %>%
      group_by(day) %>%
      arrange(desc(cumulative_count)) %>%
      top_n(10, cumulative_count) %>%
      mutate(name = taxi_zones$Zone[PULocationID]) %>%
      mutate(rank = rank(-cumulative_count),
        Count_rel = cumulative_count / cumulative_count[rank==1])

    # Create plot
    staticplot = ggplot(data, aes(rank, group = name, 
      fill = as.factor(name), color = as.factor(name))) +
      geom_tile(aes(y = cumulative_count/2,
        height = cumulative_count,
        width = 0.9), alpha = 0.8, color = NA) +
      geom_text(aes(y = 0, label = paste(name, " ")), vjust = 0.2, hjust = 1) +
      geom_text(aes(y=cumulative_count,label = cumulative_count, hjust=0)) +
      coord_flip(clip = "off", expand = FALSE) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_reverse() +
      guides(color = FALSE, fill = FALSE) +
      theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey"),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

    # Create animation
    anim_save("totalTripsToPlacesOverTime.gif",
      animate(staticplot +
        transition_states(day, transition_length = 4, state_length = 1) +
        labs(title = "Day: {closest_state}",
          subtitle = "Top 10 Locations",
          caption = "Amount of trips") +
        ease_aes('linear'),
        nframes = 400,
        fps = 10,
        width = 600,
        height = 600,
        renderer = gifski_renderer()
      )
    )

    # Return the image/gif
    list(src = "totalTripsToPlacesOverTime.gif",
      contentType = "image/gif",
      width = 600,
      height = 600,
      alt = "This is alternate text"
    )

    # Delete the file
  }, deleteFile = TRUE)
}
