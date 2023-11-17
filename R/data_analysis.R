
# Assuming bet is already loaded or sourced from `data_preparation.R`, `data_visualization.R` file
source('R/data_preparation.R')
source('R/data_visualization.R')
source('R/strategy_evaluation.R')

# Conditional percentage graph about success/fail
conditional_sf <- function(max_values, auto_cashout, bet_count, identify_num){
  potential_patterns <- numeric()
  for (i in 2:(length(max_values) - identify_num - bet_count + 2)) {
    if (max_values[i - 1] >= auto_cashout && sum(max_values[i:(i + identify_num + bet_count - 2)] < auto_cashout) >= (identify_num + bet_count - 1)) {
      potential_patterns <- c(potential_patterns, i)
    }
  }
  bet = max_values[potential_patterns + identify_num + bet_count - 1]
  
  # Prepare data for visualization
  bet_data <- data.frame(data_list = bet, category = factor(ifelse(bet > auto_cashout, "Success", "Fail"), levels = c("Success", "Fail")))
  visualize_category_data(bet_data, paste0("Conditional_SF_", bet_count))
}


for (bet_count in 1:(getOption("bet_limit"))) {
  conditional_sf(max_values, getOption("auto_cashout"), bet_count, getOption("identify_num"))
}

# Analysis "number of bettings" and "strategy_evaluation" : auto_cashout, identify_num
combinations <- expand.grid(auto_cashout = getOption("auto_cashout_range"),
                            identify_num = getOption("identify_num_range"))
combinations$number_of_bettings <- mapply(number_of_betting, 
                                          MoreArgs=list(max_values = max_values),
                                          auto_cashout = combinations$auto_cashout, 
                                          identify_num = combinations$identify_num)
combinations$strategy_evaluation <- mapply(strategy_evaluation, 
                                          MoreArgs=list(max_values = max_values, bet_limit = getOption("bet_limit"), initial_bet_amount = getOption("initial_bet_amount")),
                                          auto_cashout = combinations$auto_cashout, 
                                          identify_num = combinations$identify_num)
combinations$identify_num <- as.factor(combinations$identify_num)
visualize_number_of_bettings(combinations, y_description = "Number of Bettings")
visualize_strategy_evaluation(combinations, y_description = "Amount of Revenue")

# Analysis strategy evaluation : auto_cashout, identify_num, bet_limit, initial_bet_amount
