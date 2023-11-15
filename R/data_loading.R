# data_loading.R: This script loads, cleans, and preprocesses the raw data

# Load required libraries
library(readr)
library(dplyr)
library(lubridate)

# User-defined function for checking missing values
check_missing_values <- function(data) {
  missing_count <- sapply(data, function(x) sum(is.na(x)))
  missing_count <- data.frame(Feature = names(missing_count), Missing_Values = missing_count,
                              Percentage = (missing_count / nrow(data)) * 100)
  return(missing_count)
}

# Read the data from the CSV file
raw_data <- read.csv("data/result1.csv")

# Examine the data types and structure of raw_data
str(raw_data)
head(raw_data)

# Check for missing values in the dataset
missing_values_summary <- check_missing_values(raw_data)
print(missing_values_summary)

# Remove rows with missing values if there are any
if (sum(missing_values_summary$Missing_Values) > 0) {
  raw_data <- raw_data[complete.cases(raw_data), ]
}

# Convert 'date' column to a proper date-time format
raw_data$date <- as.POSIXct(raw_data$date, format = "%Y-%m-%d %H:%M:%OS")

# Validate data types and inspect the cleaned data
str(raw_data)
summary(raw_data)
head(raw_data)

# Store the cleaned data to the 'data' object for further analysis
data <- raw_data
