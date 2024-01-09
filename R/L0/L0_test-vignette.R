# TITLE:            name of the script
# PROJECT:          name of project that this script is part of
# AUTHORS:          list anyone contributing to the file
# COLLABORATORS:    other people involved in the wider project but not necessarily on the script
# DATA INPUT:       a brief description of the data read in through the script, including what format it’s in
# DATA OUTPUT:      a brief description of the data output from through the script, including what format it’s in
# DATE:             initiation date of script, plus any major update dates
# OVERVIEW:         Brief description of what this script does
# REQUIRES:         any scripts or code sources that are required
# NOTES:            any additional information that is necessary / helpful to describe what needs to be done next

# Load libraries 
library(geodiv)
# library(raster)
# library(rasterVis)
library(terra)
library(mapdata)
# library(maptools)
# library(rgeos)
library(tidyverse)
library(parallel)
library(sf)
library(ggmap)
library(corrplot)
library(gridExtra)
library(cowplot)
library(factoextra)
library(cluster)

# Load vignette data 
data(orforest) 
orforest <- terra::unwrap(orforest)

# Plot orforest without the trend removed
eviCols <- colorRampPalette(c("lightyellow1", "darkgreen"))(100)

terra::plot(orforest, 
            col = eviCols, 
            xlab = "Longitude", 
            ylab = "Latitude", 
            main = "orforest original")

# Remove a polynomial trend
orfor_rem <- remove_plane(orforest)

# Calculate global metrics over the entire orforest image 
(sa <- sa(orforest)) # average roughness
(sbi <- sbi(orforest)) # surface bearing index
(std <- std(orforest, create_plot = FALSE, option = 1))

# Texture image creation using 'focal_metrics' function 
window <- matrix(1, nrow = 7, ncol = 7)
system.time(
  output_raster <- focal_metrics(orforest, 
                                 window, 
                                 metrics = list('sa'), 
                                 progress = TRUE)
  )

print(output_raster)

terra::plot(output_raster$sa, 
            col = eviCols, 
            main = "sa")

# Texture image creation using 'texture_image' function
system.time(
  output_raster2 <- texture_image(orforest, 
                                  window_type = 'square',
                                  size = 7, 
                                  in_meters = FALSE, 
                                  metric = 'sa', 
                                  parallel = FALSE, 
                                  nclumps = 100
                                  )
)

# Values are different from output_raster
print(output_raster2)

terra::plot(output_raster2, # NOT WORKING - not sure why 
            col = eviCols, 
            main = "sa")

