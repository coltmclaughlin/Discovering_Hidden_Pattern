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

# Get index in max_values with 4 or more <= 1.99 results after first result >= 2
potential_patterns <- numeric()
for (i in 2:(length(max_values) - 3)) {
  if (max_values[i - 1] >= 2 && sum(max_values[i:(i + 3)] <= 1.99) >= 4) {
    potential_patterns <- c(potential_patterns, i)
  }
}

bet1 = max_values[potential_patterns + 4]
