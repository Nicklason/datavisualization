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
# Don't use scientific notation in the plot
options(scipen = 999)

# Read the geojson file
taxi_shp <- read_sf('https://data.cityofnewyork.us/api/geospatial/d3c5-ddgc?method=export&format=GeoJSON')
# Read the parquet file
taxi_data <- read_parquet('https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2023-01.parquet')
# Read the taxi zone lookup file
taxi_zones <- read_csv("https://d37ci6vzurychx.cloudfront.net/misc/taxi+_zone_lookup.csv")

# Calculate points at which to plot labels (https://stackoverflow.com/a/50860504/9698208)
centroids <- taxi_shp %>% 
  st_centroid() %>% 
  bind_cols(as_data_frame(st_coordinates(.)))

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

# Plot the map
ggplot() + 
  # Plot the locations and color them based on the total amount of money spent
  geom_sf(data = total, aes(group = location_id, fill = money), color = "white") + 
  # Plot the labels for the locations
  geom_text(aes(X, Y, label = location_id), data = centroids, size = 1) +
  # Remove x and y axis
  theme_void() +
  # Change color gradient
  scale_fill_gradient(
    low = "yellow",
    high = "red",
    na.value = "grey50",
    trans = "log10"
  )

#new plot
time_difference <- difftime(taxi_data$tpep_dropoff_datetime, taxi_data$tpep_pickup_datetime, "pct", "hours")

distance_travelled <- c(taxi_data$trip_distance)

speed <- distance_travelled / as.numeric(sub(".*\\s([0-9]+\\.[0-9]+).*", "\\1", time_difference))
speed_limited <- speed[speed<100 & speed>0]

#plot graph for average speed
boxplot(speed_limited,outline = FALSE, xlab = "Taxi cap" , ylab = "Speed [mph]")
title("Speed")

speed_passengers_distance <- data.frame( "speed" = speed, "passenger_count" = taxi_data$passenger_count, "distance" = taxi_data$trip_distance) 

# Accounted for outliers based on min 0 mph and max speed of 100 mph 
speed_passengers_distance_limited <- speed_passengers_distance[speed_passengers_distance$speed < 100 & speed_passengers_distance$speed > 0,]

#plot graph for average speed based on amount of passengers  
boxplot(speed ~ passenger_count, data = speed_passengers_distance_limited, outline = FALSE, xlab = "Amount of passagers", ylab = "Speed [mph]")
title("Speed based on amount of passengers")


#plot graph for distanced traveled based on amount of passengers  
boxplot(distance ~ passenger_count, data = speed_passengers_distance_limited, outline = FALSE, xlab = "Amount of passagers", ylab = "Distance [miles]" )
title("Distance traveled based on amount of passengers") 
