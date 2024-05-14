library(lubridate)
library(sp)     # For spatial data
library(sf)     # Spatila data as well
library(geosphere)
library(tidyverse)
library(readr)
library(lubridate)
library(scales) # plot scales

theme_set(theme_bw())

vector_layer <- st_read(
  "Data/Countries/CTRY_DEC_2023_UK_BFC.shp")

points <- st_read("Data/Total_points")
nrow(points)

unique(points$sighting_o)

points_m <- points[points$sighting_o %in% 
                    c("Mammal Spotted"),]

data <- points_m %>%
  filter(sighting_n < 50)

# Filter duplicates
data <- distinct(data)
nrow(data)


# Convert sighting_timestamp to POSIXct format
data$sighting_1 <- as.POSIXct(data$sighting_1,
                              format = "%Y-%m-%dT%H:%M:%OSZ")

# Filter duplicates based on time and distance
# Function to calculate Euclidean distance
euclidean_distance <- function(lon1, lat1, lon2, lat2) {
  print(lon1)
  #print(lon2)
  #print(lat1)
  #print(lat2)
  distance <- sqrt((lon2 - lon1)^2 + (lat2 - lat1)^2)
  return(distance)
}

# Correcting longitude values
data$lng <- ifelse(data$lng > 180, data$lng - 360, data$lng)

# Filter duplicates based on time and distance using
# Euclidean distance
filtered_df <- data %>%
  group_by(sighting_t) %>%
  filter(n() == 1 | (n() > 1 &
            any(sighting_t == lag(sighting_t) &
            #(sqrt((lng - lag(lng))^2 + (lat - lag(lat))^2)) <= 0.05 &
             #euclidean_distance(lag(lng), lag(lat),
             #lng, lat) <= 0.05 &
            abs(difftime(sighting_1, lag(sighting_1),
                         units = "hours")) <= 6)))
#merge(f_data,data$sighting_condition,by=)
nrow(filtered_df)

st_write(filtered_df, "Sub_Data/Total/Filtered_TotalData.shp",append = F)



sp_list <- c("Badger",
             "Deer",
             "Fox",
             "Dormouse",
              "Deer",
             "Bats",
              "Beaver",
              "Hare",
              "Hedgehog",
              "Marten",
              "Mink",
              "Mole",
              "Mouse",
              "Otter",
              "Polecat",
              "Rabbit",
              "Rat",
              "Seal",
              "Shrew",
              "Squirels",
              "Stoat",
              "Vole",
              "Weasel")

for (sp in sp_list){
  print(sp)
  sp_data <- filtered_df[grepl(sp,filtered_df$sighting_t),]
  folder <- "Sub_Data/"
  dir.create(paste("Sub_Data/",sp,sep=""))
  
  st_write(sp_data,
           paste("Sub_Data/",sp,"/",sp,".shp",sep=""),append = F)
  
}


