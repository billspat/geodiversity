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

# Load required libraries
library(terra)
library(duckdb)

# Define the directory containing the zipped folders
zip_dir <- "D:/SRTM_gl3_v003"

# List all zip files in the directory
zip_files <- list.files(zip_dir, pattern = "\\.zip$", full.names = TRUE)

# Function to unzip and load the .hgt file as a terra object without using a temporary directory
load_hgt <- function(zip_file) {
  
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
  
  # Optionally, clean up the unzipped directory after loading
  unlink(unzip_dir, recursive = TRUE)
  
  return(hgt_df)
}

# Apply the function to all zip files
hgt_dfs <- lapply(zip_files[1:5], load_hgt)

# Set names to tile names
names(hgt_dfs) <- substr(zip_files[1:5], nchar(zip_files[1:5])-22, nchar(zip_files[1:5])-16)


# Create duckdb database connection 
# to use a database file (not shared between processes)
db <- dbConnect(duckdb(), dbdir = "./data/my-db.duckdb", read_only = FALSE)

# Ingest table into database
dbWriteTable(db, substr(zip_files[2], nchar(zip_files[2])-22, nchar(zip_files[2])-16), hgt_dfs[[2]])

# Example queries 
# dbGetQuery(db, "SELECT max(elevation) FROM N00E006")
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


