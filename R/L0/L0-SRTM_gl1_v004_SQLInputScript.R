# TITLE:            Geodiversity Data Cleaning
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       SRTMGl3_v004 data downloaded from NASA EarthData 
# DATA OUTPUT:       
# DATE:             August 2024
# OVERVIEW:          
# REQUIRES:         
# NOTES:       

require('stringr')

create_table_sql <- function(table_name = 'elevation') {
  create_table_sql_template <- " CREATE TABLE IF NOT EXISTS {table_name}
    lon DECIMAL(9,6), 
    lat DECIMAL(8,6), 
    elevation VARINT, 
    tile_id VARCHAR,
    coordinates GEOMETRY  
  );"

  return(stringr::str_glue(create_table_sql_template))
}

#' create block of sql out of list of file paths
#' 
#' @returns character string 
insert_from_csv_sql <- function(csv_file_paths,
                                   table_name = 'elevation') {

    insert_command_template<- "INSERT INTO {table_name} 
    SELECT 
        round(x, 6) as lon, round(y, 6) as lat, elevation, tile_id, 
        st_point(round(x,6),round(y,6)) as coordinates
    FROM '{csv_file}';"

  insert_statement_list <- lapply( csv_file_paths, 
                            function(csv_file) stringr::str_glue(insert_command_template)
  )
  # collapse into single character value sep by newlines
  insert_statement_block <- paste(insert_statement_list, collapse="\n")
  return(insert_statement_block)

}

#' given a list of files, subset them in to sublists using first part of filename
#' 
#' requires a single file extension to be used to create the file search pattern or glob
#' @returns a list of lists, subsets of files based on the first "prefix_length" characters in the files
#' @example 
#'  file_paths = list.files('*.csv')
#'  subsets <- subset_files(file_paths, "csv", 8)
#'  
subset_files_on_prefix<- function(file_paths, file_extension, prefix_length){
    # from a list of files, create a list of unique prefixes.   The prefixes will be used to create subsets of files
    subset_names<- lapply(file_paths, basename) |> 
        lapply(substr, start = 1, stop = prefix_length)  |> 
        unlist() |> 
        unique()

    # given this list of prefixes, divvy up the filelist into sublists using grep and the file 'glob' prefix*.extension
    file_subsets <- lapply(subset_names, 
                           function(n) {grep(glob2rx(paste0(n,"*.", file_extension)), file_paths, value = TRUE)} )
    names(file_subsets)<-subset_names
    
    return(file_subsets)
}

#' given a list of csv files, write files of subsets of those files 
#' that are sql insert statements
#' @param csv_file_paths list of files with full paths
#' @param output_directory character path to where the SQL scripts are written
#' @returns list of file names written
#' @example 
#'  csv_directory <- 'csvs'
#'  output_directory <- "./sql_scripts"
#'  csv_files<- list.files(csv_directory, full.names = TRUE)
#'  sql_files_written <- generate_sql_scripts(csv_files, output_directory)
#'  print(sql_files_written)
generate_sql_scripts <- function(csv_file_paths, output_directory = ".") {
  # Loop over each file path in the csv_file_paths list

    # split the files into list of subsets base on first 8 characters, which includes Latitude
    # these are named for the prefix
    subsets <- subset_files_on_prefix(csv_file_paths, 'csv', 8)
    # convert the files names into sql inserts in the subsets
    sql_commands_by_latitude <- lapply(subsets, insert_from_csv_sql, table_name = 'elevation')
    
    # assign the same names of the subsets to the sql so we can loop over them
    names(sql_commands_by_latitude) <- names(subsets)

    # write each sql block to a file
    files_written<- lapply( names(list_of_sql_character), 
            function(sql_list_name){ 
                output_sql_file <- file.path(output_directory, paste0("output_script_", sql_list_name, ".sql"))
                write( x = list_of_sql_character[[sql_list_name]], 
                    file = output_sql_file)
                return(output_sql_file)
            }
        )

    return (files_written)
    
}    



# Example usage:

# Load in data_dir location
# source("./R/config.R")
# data_dir <- "/mnt/nvme/geodiversity/csvs"
# data_dir <- "/mnt/home/kapsarke/Documents/AIS/Data_Raw/2015/"
# 
# output_directory <- "/mnt/nvme/geodiversity/sql_scripts"

# Generate the SQL scripts
# generate_sql_scripts(csv_files, output_directory)
