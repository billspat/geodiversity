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

generate_sql_script <- function(csv_file_paths, output_sql_file) {
  # Start with the SQL command to create the 'elevation' table
  create_table_sql <- "
  CREATE TABLE IF NOT EXISTS elevation (
    x NUMERIC,
    y NUMERIC,
    elev NUMERIC,
    tile_id VARCHAR
  );\n\n"
  
  # Initialize SQL commands with the table creation statement
  sql_commands <- create_table_sql
  
  # Loop over each file path in the csv_file_paths list
  for (csv_file in csv_file_paths) {
    # Extract the base name of the file (optional: if you want to show file names without full path in SQL)
    csv_file_base <- basename(csv_file)
    
    # Generate the SQL command for each file
    sql_command <- paste0("INSERT INTO elevation SELECT * FROM read_csv('", csv_file_base, "');\n")
    
    # Append the command to the sql_commands string
    sql_commands <- paste0(sql_commands, sql_command)
  }
  
  # Write the SQL commands to the output file
  write(sql_commands, file = output_sql_file)
  
  cat("SQL script has been saved to:", output_sql_file, "\n")
}

# Example usage:

# Load in data_dir location
# source("./R/config.R")
# data_dir <- "/mnt/nvme/geodiversity/csvs"
data_dir <- "/mnt/home/kapsarke/Documents/AIS/Data_Raw/2015/"
csv_files <- list.files(data_dir)

# Output SQL file path
output_sql_file <- "output_script.sql"

# Generate the SQL script
generate_sql_script(csv_files, output_sql_file)
