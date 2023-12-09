library(tidyverse)
library(ggmap)
library(jsonlite)  # Load the jsonlite package
library(leaflet)

###################################
# Add list of addresses here
###################################
df<-tibble(
  address = c(
    "49 Brook Rd, Bassingbourn, UK",
    "15 High St, Bassingbourn, UK",
    "27 High St, Bassingbourn, UK",
    "12 High St, Bassingbourn, UK",
    "38 High St, Bassingbourn, UK",
    "13 Causeway, Bassingbourn, UK",
    "24 Mill Lane, Bassingbourn, UK",
    "24a Old North Road, Bassingbourn, UK",
    "14 Park View, Bassingbourn, UK",
    "22 Park View, Bassingbourn, UK",
    "88 Spring Lane, Bassingbourn, UK",
    "24 Windmill Close, Bassingbourn, UK",
    "13 Windmill Close, Bassingbourn, UK",
    "2 Tanyard, Bassingbourn, UK",
    "2 Walnut Tree Close, Bassingbourn, UK"
  ),
  number = c(1,2,3,10,14,4,5,6,13,15,16,7,12,8,9),
  dir = sample(c("top", "bottom", "left", "right"),nrow(df),replace=T)
  )

###################################
# Create a custom geocoding function which uses a free service from osm
###################################

custom_geocode <- function(address) {
  url <- paste0("https://nominatim.openstreetmap.org/search?format=json&limit=1&q=", URLencode(address))
  result <- fromJSON(url)
  if (length(result) > 0) {
    return(data.frame(
      lat = as.numeric(result$lat),
      lon = as.numeric(result$lon)
    ))
  } else {
    return(data.frame(lat = NA, lon = NA))
  }
}

###################################
# Get the geocodes for the addresses and bind to original tibble
###################################

geocoded_data <- as_tibble(t(sapply(df$address, custom_geocode)))

df<-cbind(df,geocoded_data) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),

         )

###################################
# Draw a leaflet map
###################################

# Create a Leaflet map with snowflake markers using numbers as labels
map <- leaflet(df) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Black and white tiles
  addMarkers(
    icon = makeIcon(iconUrl = "https://images.vexels.com/media/users/3/128626/isolated/preview/f30fc1dda925b2ceeaef55263ef85b8a-red-xmas-snowflake-by-vexels.png",
                    iconWidth = 25, iconHeight = 25),
    label = ~as.character(number),  # Use numbers as labels
    labelOptions = labelOptions(
      noHide = TRUE,  # Permanent labels


# Display the map
map
