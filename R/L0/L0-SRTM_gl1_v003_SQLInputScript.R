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

# Load necessary library
# If not installed, uncomment the next line to install it
# install.packages("readr")

generate_sql_scripts <- function(csv_file_paths, output_directory) {
  # Start with the SQL command to create the 'elevation' table
  create_table_sql <- "
  CREATE TABLE IF NOT EXISTS elevation (
    x DECIMAL(9,6), 
    y DECIMAL(8,6), 
    elevation VARINT, 
    tile_id VARCHAR
  );\n\n"
  
  # Create a list to hold SQL commands for each latitude degree
  sql_commands_by_latitude <- list()
  
  # Loop over each file path in the csv_file_paths list
  for (csv_file in csv_file_paths) {
    # Extract the file name from the full path
    file_name <- basename(csv_file)
    
    # Extract the first three letters of the file name to determine the latitude degree
    latitude_degree <- substr(file_name, 1, 3)
    
    # Generate the SQL command for each file
    sql_command <- paste0("INSERT INTO elevation SELECT round(x, 6), round(y, 6), elevation, tile_id FROM '", csv_file, "';\n")
    
    # Append the command to the corresponding latitude degree in the list
    if (!latitude_degree %in% names(sql_commands_by_latitude)) {
      sql_commands_by_latitude[[latitude_degree]] <- create_table_sql
    }
    sql_commands_by_latitude[[latitude_degree]] <- paste0(sql_commands_by_latitude[[latitude_degree]], sql_command)
  }
  
  # Write each set of SQL commands to a separate output file
  for (latitude_degree in names(sql_commands_by_latitude)) {
    output_sql_file <- file.path(output_directory, paste0("output_script_", latitude_degree, ".sql"))
    write(sql_commands_by_latitude[[latitude_degree]], file = output_sql_file)
    cat("SQL script has been saved to:", output_sql_file, "\n")
  }
}

# Example usage:

# Load in data_dir location
# source("./R/config.R")
# data_dir <- "/mnt/nvme/geodiversity/csvs"
data_dir <- "/mnt/home/kapsarke/Documents/AIS/Data_Raw/2015/"
csv_files <- list.files(data_dir, pattern = "\.csv$", full.names = TRUE)

# Output directory for SQL files
output_directory <- "/mnt/home/kapsarke/Documents/AIS/SQL_Scripts"

# Generate the SQL scripts
generate_sql_scripts(csv_files, output_directory)
