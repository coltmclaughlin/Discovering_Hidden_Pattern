# data_processing.R: This script processes, wrangles, and transforms the data for analysis

# Load required libraries
library(dplyr)
library(lubridate)

# Add this line if you did not source the 'data_loading.R' script
source('R/data_loading.R')

# Make sure the cleaned data is stored in the 'data' object
# Extract relevant features from the date column
processed_data <- data %>%
  mutate(
    day = day(date),
    month = month(date),
    year = year(date),
    hour = hour(date),
    minute = minute(date),
    second = second(date),
    # Extract milliseconds using format()
    millisecond = as.numeric(format(date, "%OS3")) %% 1 * 1000,
    # Extract AM/PM using format()
    am_pm = format(date, "%p")
  )

# Remove outliers/errors in 'result'
# Use the Interquartile Range (IQR) method to detect and remove outliers
# q1 <- quantile(data$result, 0.25)
# q3 <- quantile(data$result, 0.75)
# iqr <- q3 - q1
# lower_bound <- q1 - (1.5 * iqr)
# upper_bound <- q3 + (1.5 * iqr)
# data <- data %>% filter(result >= lower_bound & result <= upper_bound)

# Feature Engineering (optional)
# Create new features based on existing data that may help provide more insights during EDA
# Example: calculate the rolling mean of result
# library(zoo)
# data <- data %>%
#   arrange(date) %>%
#   mutate(rolling_mean_result = rollapply(result, width = 5, FUN = mean, align = "right", fill = NA))

# Validate and inspect newly processed data
str(processed_data)
summary(processed_data)
head(processed_data)

# The processed and transformed data is stored in the 'data' object for further analysis in the 'eda.R' script

# Create the output directory if it doesn't exist
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# Save the data to a CSV file
write.csv(processed_data, file = "data/processed/processed_data.csv", row.names = FALSE)
