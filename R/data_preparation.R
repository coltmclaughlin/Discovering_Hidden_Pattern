# Required libraries
library(dplyr)
library(ggplot2)

# Load processed data
processed_data <- read.csv("data/processed/processed_data.csv")

# Get max value of rising graph set
max_values <- c()
sub_max_result <- 0

# Preallocate memory for max_values
max_possible_length <- length(processed_data$result)
max_values <- numeric(max_possible_length)

# Initialize variables
sub_max_result <- 0
current_index <- 1

# Iterate through the processed_data, adding an extra zero at the end
for (result in c(processed_data$result, 0)) {
  if (result >= sub_max_result) {
    sub_max_result <- result
  } else {
    max_values[current_index] <- sub_max_result
    sub_max_result <- result
    current_index <- current_index + 1
  }
}

# Remove zero values from the preallocated vector
max_values <- max_values[1:(current_index - 1)]
