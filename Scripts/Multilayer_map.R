library(raster)
library(htmlwidgets)
library(leaflet)

folder_path <- "Results"

# List all files in the folder
files <- list.files(folder_path)

# Load shapefile representing grid using sf
grid <- st_read(dsn = "Data/Countries/")

leaflet_map <- leaflet() %>% addTiles() 

listir <- c()
# Iterate over each file
for (file in files) {
  if (endsWith(file, ".tif") & !grepl("Total.tif", file)){
    ## Leaflet map with raster
    current <- raster(paste("Results/",file,sep=""))
    
    current@data@values[which(current@data@values<0.0000001)]<- NA
    
    custom_palette <- c("#FFFFFF", "#E36148", "#F17F4B", "#D6CA64", "#87B560", "#34854C", "#22763F")
    palRaster <- colorBin(custom_palette, bins = 7, domain = current@data@values, na.color = "transparent")
    species <- sub("\\.tif$", "", file)
    print(file)
    leaflet_map <- leaflet_map %>% 
            addRasterImage(current, 
                           opacity = .5,
                           group = species) 
    
    listir <- c(listir,species)
  }
}

leaflet_map <- leaflet_map %>%
  addLegend(pal = palRaster, 
  values = current@data@values, 
  title = "Individual Density")

leaflet_map <- addLayersControl(leaflet_map, 
  baseGroups = listir,  # Group names
  options = layersControlOptions(collapsed = FALSE))


html_path <- paste("Results/HTMLs/Multi_density.html",sep="")

saveWidget(leaflet_map, html_path)
