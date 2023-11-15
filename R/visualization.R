# Load necessary libraries
library(ggplot2)

# Assuming bet1 is already loaded or sourced from `data_preparation.R` file
# Prepare data for visualization
bet1_data <- data.frame(bet1 = bet1, bet1_category = factor(ifelse(bet1 > 2, "Success", "Fail"), levels = c("Success", "Fail")))

# Visualization

# Bar chart
bar_chart <- ggplot(bet1_data, aes(x = bet1_category)) +
  geom_bar(width = 0.2) +
  labs(x = "Category", y = "Count")
bar_chart
ggsave("output/figures/bar_chart.png", bar_chart)

# Pie chart
pie_chart_data <- data.frame(category = unique(bet1_data$bet1_category),
                             count = as.numeric(table(bet1_data$bet1_category)),
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
ggsave("output/figures/pie_chart.png", pie_chart)

# Histogram
histogram_plot <- ggplot(bet1_data, aes(x = bet1, fill = bet1_category)) +
  geom_histogram(binwidth = 0.2, position = "identity", alpha = 0.5) +
  labs(x = "Value", y = "Frequency")
histogram_plot
ggsave("output/figures/histogram.png", histogram_plot)

# Box Plot
box_plot <- ggplot(bet1_data, aes(x = bet1_category, y = bet1)) +
  geom_boxplot(width = 0.2) +
  labs(x = "Category", y = "Value")
box_plot
ggsave("output/figures/box_plot.png", box_plot)

# Scatter plot
bet1_data$index <- seq_along(bet1)
scatter_plot <- ggplot(bet1_data, aes(x = index, y = bet1, color = bet1_category)) +
  geom_point() +
  labs(x = "Index", y = "Result")
scatter_plot
ggsave("output/figures/scatter_plot.png", scatter_plot)

# Violin plot
violin_plot <- ggplot(bet1_data, aes(x = bet1_category, y = bet1)) +
  geom_violin(width = 0.2, fill = "skyblue") +
  labs(x = "Category", y = "Value")
violin_plot
ggsave("output/figures/violin_plot.png", violin_plot)
