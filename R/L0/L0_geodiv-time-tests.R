# TITLE:            Time testing for geodiv metrics 
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       NA (using dummy data from geodiv package)
# DATA OUTPUT:      Benchmarking plots for metric calculation speeds. 
# DATE:             April 2024
# OVERVIEW:         This script contains benchmarking code to evaluate the time needed to calculate geodiversity metrics in both spatial (raster) and aspatial output formats. 
# REQUIRES:         
# NOTES:             

# Load libraries 
library(geodiv)
library(tidyverse)
library(terra)

# Laod data 
data(orforest)
orf <- terra::unwrap(orforest) 
orf <- terra::crop(orf, terra::ext(orf[1:100, 1:100, drop=FALSE]))

# Define a list of input variables as strings
metrics <- list("sa", "sq", "s10z", "sdq", "sdq6", "sdr", "sbi", "sci", "ssk", 
             "sku", "sds", "sfd", "srw", "std", "svi", "stxr", "ssc", "sv", 
             "sph", "sk", "smean", "svk", "spk", "scl", "sdc")

matrix_sizes <- list(matrix(1, nrow = 3, ncol = 3),
                     matrix(1, nrow = 5, ncol = 5), 
                     matrix(1, nrow = 7, ncol = 7))

raster_sizes <- list(terra::crop(orf, terra::ext(orf[1:20, 1:20, drop=FALSE])),
                     terra::crop(orf, terra::ext(orf[1:10, 1:10, drop=FALSE])))

# Initialize an empty data frame to store the results
result_df <- data.frame(
  metric = character(), 
  matrix_size = character(), 
  raster_size = character(), 
  time_taken = numeric()
  )

# Iterate over the metrics, matrix sizes, and raster sizes, and measure the time taken for each execution
for (metric in metrics) {
  for (matrix_size in matrix_sizes) {
    for (raster_size in raster_sizes) {
      print(paste("Metric:", metric, ", Matrix Size:", paste(nrow(matrix_size), ncol(matrix_size), sep = "x"), 
                  ", Raster Size:", paste(terra::nrow(raster_size), terra::ncol(raster_size), sep = "x")))
      time_taken <- tryCatch({
        system.time({
          result <- geodiv::focal_metrics(raster_size, window = matrix_size, metrics = metric, progress = FALSE)
        })[3] # extracting the elapsed time
      }, error = function(e) {
        print(paste("Error:", e$message))
        NA
      })
      
      # Add the metric, matrix size, raster size, and the time taken to the data frame
      result_df <- rbind(result_df, 
                         data.frame(metric = metric,
                                    matrix_size = paste(nrow(matrix_size), ncol(matrix_size), sep = "x"),
                                    raster_size = terra::ncell(raster_size)), 
                                    time_taken = ifelse(is.na(time_taken), NA, time_taken))
    }
  }
}

# Create the ggplot bar graph
ggplot(result_df, aes(x = metric, y = time_taken, fill = raster_size)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = subset(result_df, is.na(time_taken)), aes(label = "ERROR"), vjust = -0.5, color = "red") +
  labs(title = "Time Taken for Focal Metrics Calculation",
       x = "Metric",
       y = "Time Taken (seconds)",
       fill = "Raster Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis text at 45 degrees

# ggsave(paste0(getwd(), "/figures/Runtimes_20x20.png"), p)

############################################################################


# Define function to calculate time taken for a function
time_function <- function(raster_layer, function_name) {
  start_time <- Sys.time()
  result <- eval(parse(text = paste0(function_name, "(raster_layer)")))
  end_time <- Sys.time()
  return(list(runtime = end_time - start_time, result = result))
}

# Use sapply to apply time_function to each metric
results <- sapply(metrics[-length(metrics)], FUN = function(name) {
  result <- time_function(orf, name)
  dimensions <- paste(dim(orf), collapse = "x")
  return(c(Metric = name, Raster_Dimensions = dimensions, Time_Taken = result$runtime, Result = result$result))
}, simplify = "data.frame")

# Transpose the results and convert to data frame
results_df <- as.data.frame(t(results), stringsAsFactors = FALSE)

# Set column names
colnames(results_df) <- c("Metric", "Raster_Dimensions", "Time_Taken", "Result")

# Print results
print(results_df)
