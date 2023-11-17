# Load necessary libraries
library(ggplot2)

visualize_category_data <- function(data, filename){
  # Visualization
  # data : that is dataframe.
  # Bar chart
  bar_chart <- ggplot(data, aes(x = category)) +
    geom_bar(width = 0.2) +
    labs(x = "Category", y = "Count")
  ggsave(paste0("output/figures/bar_chart_", filename, ".png"), bar_chart)
  
  # Pie chart
  pie_chart_data <- data.frame(category = unique(data$category),
                               count = as.numeric(table(data$category)),
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
  ggsave(paste0("output/figures/pie_chart_", filename, ".png"), pie_chart)
  
  # Histogram
  histogram_plot <- ggplot(data, aes(x = data_list, fill = category)) +
    geom_histogram(binwidth = 0.2, position = "identity", alpha = 0.5) +
    labs(x = "Value", y = "Frequency")
  ggsave(paste0("output/figures/histogram_", filename, ".png"), histogram_plot)
  
  # Box Plot
  box_plot <- ggplot(data, aes(x = category, y = data_list)) +
    geom_boxplot(width = 0.2) +
    labs(x = "Category", y = "Value")
  ggsave(paste0("output/figures/box_plot_", filename, ".png"), box_plot)
  
  # Scatter plot
  data$index <- seq_along(data$data_list)
  scatter_plot <- ggplot(data, aes(x = index, y = data_list, color = category)) +
    geom_point() +
    labs(x = "Index", y = "Result")
  ggsave(paste0("output/figures/scatter_plot_", filename, ".png"), scatter_plot)
  
  # Violin plot
  violin_plot <- ggplot(data, aes(x = category, y = data_list)) +
    geom_violin(width = 0.2, fill = "skyblue") +
    labs(x = "Category", y = "Value")
  ggsave(paste0("output/figures/violin_plot_", filename, ".png"), violin_plot)
}

visualize_number_of_bettings <- function(combinations, y_description){
  # create a scatter plot
  scatter_plot <- ggplot(combinations, aes(x = auto_cashout, y = number_of_bettings, color = identify_num)) +
    geom_point() +
    scale_color_discrete(name = "Identify Number") +
    labs(x = "Auto Cashout", y = y_description) +
    ggtitle(paste0("Scatter plot of Auto Cashout and ", y_description))
  ggsave(paste0("output/figures/scatter_plot_", y_description, ".png"), scatter_plot)
  
  # create a line plot
  line_plot <- ggplot(combinations, aes(x = auto_cashout, y = number_of_bettings, color = identify_num, group = identify_num)) +
    geom_line() +
    scale_color_discrete(name = "Identify Number") +
    labs(x = "Auto Cashout", y = y_description) +
    ggtitle(paste0("Line plot of Auto Cashout and ", y_description))
  ggsave(paste0("output/figures/line_plot_", y_description, ".png"), line_plot)
}

visualize_strategy_evaluation <- function(combinations, y_description){
  # create a scatter plot
  scatter_plot <- ggplot(combinations, aes(x = auto_cashout, y = strategy_evaluation, color = identify_num)) +
    geom_point() +
    scale_color_discrete(name = "Identify Number") +
    labs(x = "Auto Cashout", y = y_description) +
    ggtitle(paste0("Scatter plot of Auto Cashout and ", y_description))
  ggsave(paste0("output/figures/scatter_plot_", y_description, ".png"), scatter_plot)
  
  # create a line plot
  line_plot <- ggplot(combinations, aes(x = auto_cashout, y = strategy_evaluation, color = identify_num, group = identify_num)) +
    geom_line() +
    scale_color_discrete(name = "Identify Number") +
    labs(x = "Auto Cashout", y = y_description) +
    ggtitle(paste0("Line plot of Auto Cashout and ", y_description))
  ggsave(paste0("output/figures/line_plot_", y_description, ".png"), line_plot)
}
