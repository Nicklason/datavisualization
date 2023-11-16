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
taxi_shp <- read_sf('https://data.cityofnewyork.us/api/geospatial/d3c5-ddgc?method=export&format=GeoJSON') # nolint
# Read the parquet file
taxi_data <- read_parquet('https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2023-01.parquet') # nolint
# Read the taxi zone lookup file
taxi_zones <- read_csv("https://d37ci6vzurychx.cloudfront.net/misc/taxi+_zone_lookup.csv") # nolint

# Calculate points at which to plot labels (https://stackoverflow.com/a/50860504/9698208) # nolint
centroids <- taxi_shp %>% 
  st_centroid() %>% 
  bind_cols(as_data_frame(st_coordinates(.)))

# Calculate the total amount of money spent in each location
total <- merge(
    aggregate(taxi_data$total_amount, by=list(location_id=taxi_data$DOLocationID), FUN=sum), # nolint
    aggregate(taxi_data$total_amount, by=list(location_id=taxi_data$PULocationID), FUN=sum), # nolint
    by="location_id", # nolint
    all.x=TRUE, # nolint
    all.y=TRUE # nolint
  ) %>%
  mutate_at(c('x.x','x.y'), ~replace_na(.,0)) %>%
  mutate(money = x.x + x.y, x.x = NULL, x.y = NULL) %>%
  mutate_at(c('location_id'), as.character) %>% # nolint
  left_join(taxi_shp, ., by = c('location_id' = 'location_id'))

# Plot the map
ggplot() + 
  # Plot the locations and color them based on the total amount of money spent
  geom_sf(data = total, aes(group = location_id, fill = money), color = "white") + 
  geom_segment(data = total, aes(x = longitude.x, y = latitude.x, xend = longitude.y, yend = latitude.y, color=freq),
    arrow = arrow(length = unit(0.01, "npc")))+
    scale_colour_distiller(palette="Reds", name="Frequency", guide = "colorbar") +
    coord_equal() +
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
