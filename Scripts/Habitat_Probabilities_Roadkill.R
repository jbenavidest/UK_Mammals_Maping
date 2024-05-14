# Load required packages
library(raster)
library(sf)
library(readr)
library(tidyverse)
library(KernSmooth)#kernel estimation
library(leaflet)
library(htmlwidgets) # html export
library(webshot)     # png map export

folder_path <- "Sub_Data"

# List all files in the folder
files <- list.files(folder_path)

# Load shapefile representing grid using sf
grid <- st_read(dsn = "Data/Countries/")

# Iterate over each file
for (file in files) {
  # Load raster data
  
  # Load point data using sf
  points <- st_read(dsn = paste("Sub_Data/",file,"/",sep=""))
  
  points <- points[grepl("Roadkill",points$sighting_c),]
  coords <- data.frame(lng = as.numeric(points$lng),
                       lat = as.numeric(points$lat))
  
  if (nrow(coords)<5){
    next
    }else{
  #kde <- bkde2D(as.data.frame(sightings)[ , c("lng", "lat")],
  kde <- bkde2D(coords,
                bandwidth=c(0.07,0.07), gridsize = c(10000,10000))
  
  # Create Raster from Kernel Density output
  KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))

  filename <- paste("Results/Roadkill/",file,".tif",sep="")  # Adjust the filename and extension as needed
  format <- "GTiff"  # Adjust the format as needed (e.g., "GTiff" for GeoTIFF)

  # Export the raster
  KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.001)] <- NA
  
  
  writeRaster(KernelDensityRaster, filename, format=format,overwrite=T)

  custom_palette <- c("#FFFFFF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C")
  palRaster <- colorBin(custom_palette, bins = 7, domain = KernelDensityRaster@data@values, na.color = "transparent")
  
  
  ## Leaflet map with raster
  leaflet_map <- leaflet() %>% addTiles() %>% 
    addRasterImage(KernelDensityRaster, 
                   colors = palRaster, 
                   opacity = .5) %>%
    addLegend(pal = palRaster, 
              values = KernelDensityRaster@data@values, 
              title = "Kernel Density of Points")
  
  html_path <- paste("Results/HTMLs/",file,".html",sep="")
  png_path <- paste("Results/PNGs/",file,".png",sep="")
    }
  #saveWidget(leaflet_map, html_path)
  #webshot(html_path, png_path,vwidth = 1000, vheight = 800)
  }


## Leaflet map with raster
leaflet() %>% addTiles() %>% 
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .5) %>%
  addLegend(pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = "Kernel Density of Points")# %>% 
  #addMarkers(data = coords, ~lng, ~lat) #%>% 
#ggplot(grid) +
#  geom_sf(fill = "#69b3a2", color = "white") +
#  geom_sf(data= points,color = "#000000", size = 0.1,alpha=0.5) +
#  theme_void()

# Create empty raster layer
#raster_template <- raster(extent(grid), resolution = 1000)

# Rasterize points
#raster_points <- rasterize(filt_p,raster_template,"sighting_n",fun="count")


# Save raster file
#writeRaster(raster_points, filename = "Results/density_rasterTotal.tif",
#            format = "GTiff", overwrite = TRUE)
