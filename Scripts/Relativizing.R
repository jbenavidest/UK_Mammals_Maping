library(raster)
library(rgdal)

folder_path <- "Results/"
files <- list.files(folder_path)

# Load shapefile representing grid using sf
grid <- readOGR(dsn = "Data/Countries/")
projected_grid <- spTransform(grid, crs(result))
for (rasti in files){
  if (endsWith(rasti, ".tif")){
      # Load your two raster files
      raster1 <- raster(paste("Results/",rasti,sep=""))
      raster2 <- raster("Results/Total.tif")
      
      # Check the properties of the rasters
      #print(extent(raster1))
      #print(extent(raster2))
      #print(res(raster1))
      #print(res(raster2))
      #print(crs(raster1))
      #print(crs(raster2))
      
      # Resample raster2 to match the extent and resolution of raster1
      raster2_resampled <- resample(raster2, raster1,
                                    tolerance = 100,
                                    method = "ngb")
      
      # If the CRS is different, reproject raster2 to match the CRS of raster1
      if (!identical(crs(raster1), crs(raster2_resampled))) {
        raster2_resampled <- projectRaster(raster2_resampled, crs = crs(raster1))
      }
      
      # Now both rasters have the same extent, resolution, and CRS
      #print(extent(raster2_resampled))
      #print(res(raster2_resampled))
      #print(crs(raster2_resampled))
      
      # Divide raster1 by raster2_resampled
      result <-  raster1 / raster2_resampled
      
      result@data@values[which(result@data@values<0.01)] <- 0
      print("crs(raster2_resampled)")
      
      result <- crop(result,projected_grid)
      
      result_path <- paste("Results/Rel/",rasti,"_rel.tif",sep="")
      # Save the result as a new raster file
      writeRaster(result, result_path, format = "GTiff", overwrite = TRUE)
      
      custom_palette <- c("#BE2A3E", "#E36148", "#F17F4B", "#D6CA64", "#87B560", "#34854C", "#22763F")
      palRaster <- colorBin(custom_palette, bins = 7, domain = result@data@values, na.color = "transparent")
      
      
      ## Leaflet map with raster
      leaflet_map <- leaflet() %>% addTiles() %>% 
        addRasterImage(result, 
                       colors = palRaster, 
                       opacity = .5) %>%
        addLegend(pal = palRaster, 
                  values = result@data@values, 
                  title = paste("Relative Kernel Density of Points",
                                "\n",rasti,
                           sep=""))
      
      html_path <- paste("Results/Rel/htmls/",rasti,".html",sep="")
      png_path <- paste("Results/Rel/PNGs/",file,".png",sep="")
      
      saveWidget(leaflet_map, html_path)
      
  }
}

# Check the properties of the rasters
print(extent(raster1))
print(extent(raster2))
print(res(raster1))
print(res(raster2))
print(crs(raster1))
print(crs(raster2))

# Resample raster2 to match the extent and resolution of raster1
raster2_resampled <- resample(raster2, raster1, method = "bilinear")

# If the CRS is different, reproject raster2 to match the CRS of raster1
if (!identical(crs(raster1), crs(raster2_resampled))) {
  raster2_resampled <- projectRaster(raster2_resampled, crs = crs(raster1))
}

# Now both rasters have the same extent, resolution, and CRS
print(extent(raster2_resampled))
print(res(raster2_resampled))
print(crs(raster2_resampled))

# Divide raster1 by raster2_resampled
result <- raster1 / raster2_resampled

# Save the result as a new raster file
writeRaster(result, "path/to/save/result.tif", format = "GTiff", overwrite = TRUE)
