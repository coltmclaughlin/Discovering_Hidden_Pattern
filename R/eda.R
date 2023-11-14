# Load required libraries
library(dplyr)
library(ggplot2)
# install.packages("xts")
library(xts)
# install.packages("lubridate")
library(lubridate)
# install.packages("forecast")
library(forecast)

##### Load the processed data from data_processing.R #####

# Make sure to use the correct file name or variable name from your data_processing.R
# Assuming the processed data is saved in a CSV file called "processed_data.csv"
data <- read.csv("data/processed/processed_data.csv")

##### Explore and summarize the data #####

# Summary
summary(data)

# First few observations
head(data)

# Check the structure of the data
str(data)

##### Descriptive Statistics and Visualization #####

# Histogram of the result variable
histogram_plot <- ggplot(data, aes(x = result)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Results", x = "Result", y = "Count")

histogram_plot

# Boxplot of results by month
boxplot_month <- ggplot(data, aes(x = factor(month), y = result)) +
  geom_boxplot(fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Results by Month", x = "Month", y = "Result")

boxplot_month

##### Save Plots #####

# Create the output directory if it doesn't exist
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# Save the plots to the files
ggsave(filename = "output/figures/histogram_plot.png", plot = histogram_plot, width = 8, height = 6, dpi = 300)
ggsave(filename = "output/figures/boxplot_month.png", plot = boxplot_month, width = 8, height = 6, dpi = 300)
