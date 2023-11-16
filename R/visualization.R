# Load necessary libraries
library(ggplot2)

# Assuming bet is already loaded or sourced from `data_preparation.R` file

# Get index in max_values with n(>=4) or more <= 1.99 results after first result >= 2
get_bet_result <- function(n){
  potential_patterns <- numeric()
  for (i in 2:(length(max_values) - n + 1)) {
    if (max_values[i - 1] >= 2 && sum(max_values[i:(i + n - 1)] <= 1.99) >= n) {
      potential_patterns <- c(potential_patterns, i)
    }
  }
  bet = max_values[potential_patterns + n]

  # Prepare data for visualization
  bet_data <- data.frame(bet = bet, bet_category = factor(ifelse(bet > 2, "Success", "Fail"), levels = c("Success", "Fail")))
  
  # Visualization
  
  # Bar chart
  bar_chart <- ggplot(bet_data, aes(x = bet_category)) +
    geom_bar(width = 0.2) +
    labs(x = "Category", y = "Count")
  bar_chart
  ggsave(paste0("output/figures/bar_chart", n-3, ".png"), bar_chart)
  
  # Pie chart
  pie_chart_data <- data.frame(category = unique(bet_data$bet_category),
                               count = as.numeric(table(bet_data$bet_category)),
                               stringsAsFactors = FALSE)
  pie_chart_data$percentage <- round((pie_chart_data$count / sum(pie_chart_data$count)) * 100, 2)
  ## Modify the order of categories by placing "Fail" first
  pie_chart_data <- pie_chart_data[order(factor(pie_chart_data$category, levels = c("Fail", "Success"))),]
  
  ## Calculate the cumulative sum and mid-points
  pie_chart_data$position <- cumsum(pie_chart_data$count) - pie_chart_data$count / 2
  
  pie_chart <- ggplot(pie_chart_data, aes(x = "", y = count, fill = category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    geom_text(aes(y = position, label = paste(percentage, "%")), color = "white", size = 5)
  pie_chart
  ggsave(paste0("output/figures/pie_chart", n-3, ".png"), pie_chart)
  
  # Histogram
  histogram_plot <- ggplot(bet_data, aes(x = bet, fill = bet_category)) +
    geom_histogram(binwidth = 0.2, position = "identity", alpha = 0.5) +
    labs(x = "Value", y = "Frequency")
  histogram_plot
  ggsave(paste0("output/figures/histogram", n-3, ".png"), histogram_plot)
  
  # Box Plot
  box_plot <- ggplot(bet_data, aes(x = bet_category, y = bet)) +
    geom_boxplot(width = 0.2) +
    labs(x = "Category", y = "Value")
  box_plot
  ggsave(paste0("output/figures/box_plot", n-3, ".png"), box_plot)
  
  # Scatter plot
  bet_data$index <- seq_along(bet)
  scatter_plot <- ggplot(bet_data, aes(x = index, y = bet, color = bet_category)) +
    geom_point() +
    labs(x = "Index", y = "Result")
  scatter_plot
  ggsave(paste0("output/figures/scatter_plot", n-3, ".png"), scatter_plot)
  
  # Violin plot
  violin_plot <- ggplot(bet_data, aes(x = bet_category, y = bet)) +
    geom_violin(width = 0.2, fill = "skyblue") +
    labs(x = "Category", y = "Value")
  violin_plot
  ggsave(paste0("output/figures/violin_plot", n-3, ".png"), violin_plot)
}

for (i in 0:7) {
  get_bet_result(4 + i)
}

