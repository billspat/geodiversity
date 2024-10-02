# TITLE:            Geodiversity Data Cleaning
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       SRTMGl3_v003 data downloaded from NASA EarthData 
# DATA OUTPUT:       
# DATE:             August 2024
# OVERVIEW:          
# REQUIRES:         
# NOTES:             

##### Preliminaries  -------------------------------------------------

# Load required libraries
library(terra)
# library(duckdb)
library(dplyr)
library(doParallel)
# library(pbapply)

# Load in data_dir location
# source("./R/config.R")
data_dir <- "/mnt/nvme/geodiversity/"

# Define the directory containing the zipped folders
zip_dir <- paste0(data_dir, "SRTM_gl3_v003")

##### Load Functions -------------------------------------------------

# Function to unzip and load the .hgt file into a duckdb database
load_hgt_save_csv <- function(zip_file, out_path) {
  
  start <- proc.time()
  
  # Define the directory where the zip file will be unzipped
  unzip_dir <- sub("\\.zip$", "", zip_file)  # Remove .zip extension to create directory name
  
  # Unzip the file
  unzip(zip_file, exdir = unzip_dir)
  
  # Find the .hgt file in the unzipped directory
  hgt_file <- list.files(unzip_dir, pattern = "\\.hgt$", full.names = TRUE)
  
  # Load the .hgt file as a terra object
  hgt_raster <- rast(hgt_file) 
  
  # Convert to raster
  hgt_df <- terra::as.data.frame(hgt_raster, xy=TRUE)
  
  # Specify z column name 
  colnames(hgt_df)[[3]] <- "elevation"
  
  # Add column for tile ID
  hgt_df$tile_id <- substr(zip_file, nchar(zip_file)-22, nchar(zip_file)-16)
  
  # # Optionally, clean up the unzipped directory after loading
  # unlink(unzip_dir, recursive = TRUE)

  out_name <- paste0(out_path, hgt_df$tile_id[1], ".csv")
  
  if(!exists(out_name)){
    write.csv(hgt_df, out_name)
    # write.csv(temp, paste0(data_dir, "SRTM_", temp$tile_id[1], ".csv"))
    # dbAppendTable(db, "elevation", temp)
  }
  
  # if(i%%5 == 0){
  # print(paste0(i, " of ", length(usa_zip), " files unzipped (", round(i/length(usa_zip), 2), "%)"))
  # }
  print(paste0("Processed: ", out_name))
  
  tm <- (proc.time() - start)/60
  print(paste0("Processing time: ", round(tm[[3]], 2) , " minutes."))
}

##### Isolate files for data entry ---------------------------------------------

# List all zip files in the directory
zip_files <- list.files(zip_dir, pattern = "\\.zip$", full.names = TRUE)

latlong <- 
  data.frame(
    lat_num = as.numeric(substr(zip_files, nchar(zip_files)-21, nchar(zip_files)-20)),
    lat_dir = substr(zip_files, nchar(zip_files)-22, nchar(zip_files)-22),
    long_num = as.numeric(substr(zip_files, nchar(zip_files)-18, nchar(zip_files)-16)),
    long_dir = substr(zip_files, nchar(zip_files)-19, nchar(zip_files)-19)
  ) %>% 
  mutate(
    lat = dplyr::case_when(
      lat_dir == "S" ~ lat_num*-1, .default = lat_num), 
    long = dplyr::case_when(
      long_dir == "W" ~ long_num*-1, .default = long_num), 
    index = 1:length(long_num)
  ) %>% 
  dplyr::select(
    index, 
    lat, 
    long) %>% 
  filter(
    lat > 14 & long < 60
  )

# Delete files outside US lat long
usa_zip <- zip_files[latlong$index]
rm(zip_files)

##### Add files into database ---------------------------------------------

# Create duckdb database connection 
# to use a database file (not shared between processes)
# db <- dbConnect(duckdb(), dbdir = "./data/my-db.duckdb", read_only = FALSE)

# Initiate database table 
# dbWriteTable(db, "elevation", load_hgt(usa_zip[[1]]))


## MSU HPCC: https://wiki.hpcc.msu.edu/display/ITH/R+workshop+tutorial#Rworkshoptutorial-Submittingparalleljobstotheclusterusing{doParallel}:singlenode,multiplecores
# Request a single node (this uses the "multicore" functionality)
registerDoParallel(cores=100)

# create a blank list to store the results (I truncated the code before the ship-type coding, and just returned the sf of all that day's tracks so I didn't 
#       have to debug the raster part. If we're writing all results within the function - as written here and as I think we should do - the format of the blank list won't really matter.)
res=list()

# foreach and %dopar% work together to implement the parallelization
# note that you have to tell each core what packages you need (another reason to minimize library use), so it can pull those over
# I'm using tidyverse since it combines dplyr and tidyr into one library (I think)
res=foreach(i=321:length(usa_zip),.packages=c("terra", "dplyr"),
            .errorhandling='pass',.verbose=T,.multicombine=TRUE) %dopar% 
              load_hgt_save_csv(zip_file = usa_zip[[i]], 
                                out_path = "/mnt/nvme/geodiversity/csvs/SRTM_")

# Elapsed time and running information
tottime <- proc.time() - start
tottime_min <- tottime[[3]]/60 

cat("Time elapsed:", tottime_min, "\n")
cat("Currently registered backend:", getDoParName(), "\n")
cat("Number of workers used:", getDoParWorkers(), "\n")

  
# pblapply(usa_zip[2:length(usa_zip)], function(x){
#   dbAppendTable(db, "elevation", load_hgt(x))})

# Close database connection 
# duckdb::dbDisconnect(db)
# Make the table spatial


# In a separate script: 
# 2 Import shapefiles for spatial intersection
# 3 Intersect shapefile with table (not sure how fast this will be)
# 4 Export intersection to R, convert to raster (output of part 3)
# 5 Calculate geodiv metrics 

tm <- (proc.time() - start)/60/60
print(paste0("Processing time: ", round(tm[[3]], 2) , " hours."))

# Example queries 
# dbGetQuery(db, "SELECT max(elevation) FROM elevation")
# dbGetQuery(db, "DESCRIBE elevation")
# dbGetQuery(db, "SHOW TABLES")

## RASTER PROCESSING 
# # Combine all rasters into one large raster using the mosaic function
# combined_raster <- do.call(terra::mosaic, hgt_rasters)
# 
# # Optionally, save the combined raster to a file
# writeRaster(combined_raster, "path/to/save/combined_raster.tif", overwrite = TRUE)
#
#
#
# Pull out dataframes from list by name 
# hgt_dfs[which(names(hgt_dfs) == "N00E012")]


