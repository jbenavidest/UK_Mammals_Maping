library(raster)

folder_path <- "Results/Roadkill"
files <- list.files(folder_path)

for (rasti in files){
  if (endsWith(rasti, ".tif")){
      # Load your two raster files
      raster1 <- raster(paste("Results/Roadkill/",rasti,sep=""))
      raster2 <- raster("Results/Total.tif")
      
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
      
      result_path <- paste("Results/Rel_roadkill/",rasti,"_rel.tif",sep="")
      # Save the result as a new raster file
      writeRaster(result, result_path, format = "GTiff", overwrite = TRUE)
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


unique(data$sighting_t)
