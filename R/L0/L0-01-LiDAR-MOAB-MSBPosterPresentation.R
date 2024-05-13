# TITLE:            LiDAR data visualization for MSB Poster Presentation
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       NEON LiDAR data (manually downloaded one image for MOAB)
# DATA OUTPUT:      Maps of sa and sfd 
# DATE:             February 2024
# OVERVIEW:         This script contains code to calculate and visualize sa and sfd metrics for a subset of the MOAB NEON site. 
# REQUIRES:         
# NOTES:             

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
ras <- terra::rast(paste0("./data/L0/TEST_NEON_lidar-elev/",
               "NEON.D13.MOAB.DP3.30024.001.2021-04.basic.20231220T223813Z.RELEASE-2023/",
               "NEON_D13_MOAB_DP3_644000_4229000_DTM.tif"))

# Plot orforest without the trend removed
eviCols <- colorRampPalette(c("lightyellow1", "darkgreen"))(100)

crop_ext <- ext(644600, 645000, 4229600, 4230000)
ras <- terra::crop(ras, crop_ext)

out_elev <- ras %>% project("epsg:4326")

terra::plot(out_elev, 
            col = eviCols, 
            xlab = "Longitude", 
            ylab = "Latitude", 
            main = "MOAB: Elevation")

# Remove a polynomial trend
ras_rem <- remove_plane(ras)

# Calculate global metrics over the entire orforest image 
(sa <- sa(ras)) # average roughness
(sbi <- sbi(ras)) # surface bearing index
(std <- std(ras, create_plot = FALSE, option = 1))

# Texture image creation using 'focal_metrics' function 
window <- matrix(1, nrow = 7, ncol = 7)
system.time(
  output_raster <- focal_metrics(ras, 
                                 window, 
                                 metrics = list("sa", 'sfd', "sdr"), 
                                 progress = TRUE)
)

print(output_raster)

out_sa <- output_raster$sa %>% project("epsg:4326")

terra::plot(out_sa, 
            col = eviCols, 
            xlab = "Longitude", 
            ylab = "Latitude", 
            main = "MOAB: Average Roughness")

out_sfd <- output_raster$sfd %>% project("epsg:4326")

terra::plot(out_sfd, 
            col = eviCols, 
            xlab = "Longitude", 
            ylab = "Latitude", 
            main = "MOAB: Fractal Dimension")

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
            main = "MOAB Average Roughness")

