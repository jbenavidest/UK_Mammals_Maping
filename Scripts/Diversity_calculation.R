library(raster)
library(leaflet)
library(htmlwidgets) # html export
library(webshot)     # png map export


folder_path <- "Results"

files <- list.files(folder_path)

# Load the existing raster
existing_raster <- raster(paste("Results/",files[1],sep=""))

# Define a function to multiply raster values by their logarithms
multiply_by_log <- function(raster) {
    log_raster <- log(raster + 1)  # Adding 1 to avoid logarithm of zero
    multiplied_raster <- raster * log_raster
    return(multiplied_raster)
}
  
sum_raster <- multiply_by_log(existing_raster)
sc <- 1
for (rasti in files[2:length(files)]){
  if (endsWith(rasti, ".tif")){
    # Load your two raster files
    print(rasti)
    raster_layer <- raster(paste("Results/",rasti,sep=""))
    modified_layer <- multiply_by_log(raster_layer)
    raster2_resampled <- resample(modified_layer, sum_raster, method = "bilinear")
    
    sum_raster <- sum_raster + raster2_resampled
  }
}
sum_raster <- sum_raster/length(files)

sum_raster@data@values[which(sum_raster@data@values < 0.01)] <- NA


custom_palette <- c("#BE2A3E", "#E36148", "#F17F4B", "#D6CA64", "#87B560", "#34854C", "#22763F")
palRaster <- colorBin(custom_palette, bins = 7, domain = sum_raster@data@values, na.color = "transparent")


## Leaflet map with raster
leaflet_map <- leaflet() %>% addTiles() %>% 
  addRasterImage(sum_raster, 
                 colors = palRaster, 
                 opacity = .8) %>%
  addLegend(pal = palRaster, 
            values = sum_raster@data@values, 
            title = "Shannon biodiversity hotspots")

html_path <- paste("Results/HTMLs/Diversity.html",sep="")

saveWidget(leaflet_map, html_path)
#webshot(html_path, "Results/Diversity_map",vwidth = 1000, vheight = 800)


# Save the final accumulated raster
writeRaster(sum_raster, "Diversity_hotspots.tif", format="GTiff")
