# TITLE:            NEON LiDAR Download and Processing
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       NEON LiDAR AOP data, NEON site polygons 
# DATA OUTPUT:      Geodiversity metrics (spatial rasters and summary stats)
# DATE:             March 2024
# OVERVIEW:         This script contains code to download and generate raster mosaics of LiDAR data for NEON AOPs and calculate geodiversity metrics on them. 
# REQUIRES:         config.R, forked version of NEON-data-skills repo (for mosiacking functions...)
# NOTES:            Bug in neonUtilities package was fixed (as of 13 May 2024). However, data download function still not working -- Error mesage: "URLs may have expired." 

# Load libraries 
library(geodiv)
library(tidyverse)
library(terra)
library(sf)
library(neonUtilities)

# Load NEON API token 
source("./R/config.R")

# Load in functions from forked version of this repo: https://github.com/NEONScience/NEON-Data-Skills
source("../NEON-Data-Skills/tutorials/R/AOP/AOP-L3-rasters/aop_merge_raster_functions.R")
# lapply(list.files("../NEON-utilities/neonUtilities/R/", pattern=".R", full.names=TRUE), source)

# # Increase default timeout time 
options(timeout=300)

# Specify input parameters 
dpID <- "DP3.30024.001"
year <- 2022
siteCode <- "HARV"
dataRootDir <- paste0("./data/L0/NEON_lidar_elev/", siteCode)
outFileDir <- "./data/L0/NEON_lidar_elev/"

# Create data directory if it doens't exist 
if(!dir.exists(dataRootDir)){dir.create(dataRootDir)}

# NEON site boundaries 
sites <- st_read("./data/L0/NEON_sites/terrestrialSamplingBoundaries.shp")
site_bounds <- sites[sites$siteID == siteCode,]

# Based on this tutorial: https://www.neonscience.org/resources/learning-hub/tutorials/merge-aop-raster-data
# makeFullSiteMosaics('DP3.30015.001','2021','MCRA',dataRootDir,outFileDir,NEON_TOKEN,include.provisional=T,COG=T)
# 
makeFullSiteMosaics(dpID,
                    year,
                    siteCode,
                    dataRootDir,
                    outFileDir,
                    include.provisional = T,
                    apiToken = NEON_TOKEN,# Loaded from config.R file
                    COG = F)


# # Revert to default timeout time 
options(timeout=60)
# 
# # calculate_geodiv <- function(site_name, site_bounds, DPID, metrics){
# 
# test <- neonUtilities::byFileAOP(dpID = dpID, 
#                                 site = site_name, 
#                                 year = 2017, 
#                                 include.provisional = FALSE, 
#                                 check.size = TRUE, 
#                                 savepath = "./data/L0/NEON_lidar_elev/")
# }

# Attempt with manually downloaded files
files <- list.files("./data/L0/NEON_lidar_elev/NEON.D01.HARV.DP3.30024.001.2022-08.basic.20240417T175306Z.RELEASE-2024/", pattern = ".tif",full.names = TRUE)
t <- lapply(files, terra::rast)
system.time(u <- terra::sprc(unlist(t)))
system.time(
  v <- terra::mosaic(u)
)

l <- st_transform(site_bounds, terra::crs(v))

plot(v)
plot(l, add=T)

test <- terra::mask(v, l)

# # Trying to remove empty rasters to improve run time... 
# w <- lapply(t, function(x){terra::minmax(x, compute=TRUE)}) %>% 
#   do.call(cbind, .) %>% 
#   t() %>% 
#   as.data.frame() %>% 
#   mutate(test =is.na(min))

# Define a list of input variables as strings
# NOTE: std needs additional input of Option 2 to calculate stdi instead of std
metrics <- list("sa", "sdq", "sbi", "ssk", 
                "sku", "sfd", "std", "sds")

# Define function to calculate time taken for a function
time_function <- function(raster_layer, function_name) {
  start_time <- Sys.time()
  result <- eval(parse(text = paste0(function_name, "(raster_layer)")))
  runtime <- as.numeric(Sys.time() - start_time, units="mins")
  return(list(runtime = runtime, result = result))
}

# Use sapply to apply time_function to each metric
results <- sapply(metrics[-length(metrics)], FUN = function(name) {
  result <- time_function(test, name)
  dimensions <- paste(dim(test), collapse = "x")
  return(c(Metric = name, Raster_Dimensions = dimensions, Time_Taken = result$runtime, Result = result$result))
}, simplify = "data.frame")

# Transpose the results and convert to data frame
results_df <- as.data.frame(t(results), stringsAsFactors = FALSE)

# Set column names
colnames(results_df) <- c("Metric", "Raster_Dimensions", "Time_Taken", "Result")

# Print results
print(results_df)
