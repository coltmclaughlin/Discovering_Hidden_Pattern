# Load necessary libraries
library(dplyr)

# Define betting number function
number_of_betting <- function(max_values, auto_cashout, identify_num) {
  n_patterns <- 0
  for(i in (identify_num + 1):length(max_values)) {
    if(max_values[i - identify_num] >= auto_cashout && all(max_values[(i-identify_num+1):i] < auto_cashout)) {
      n_patterns <- n_patterns + 1
    }
  }
  return(n_patterns)
}

# Define your strategy function

revenue <- function(auto_cashout, bet_amount) {
  return(auto_cashout * bet_amount)
}
strategy_evaluation <- function(max_values, auto_cashout, identify_num, bet_limit, initial_bet_amount) {
  
  total_revenue <- 0
  total_loss <- 0
  
  for(i in (identify_num + 1):length(max_values)) {
    
    if(max_values[i - identify_num] >= auto_cashout && all(max_values[(i-identify_num+1):i] < auto_cashout)) {
      
      bet_count <- 0
      bet_amount <- initial_bet_amount
      total_loss <- total_loss + bet_amount
      
      while(((i + bet_count + 1) <= length(max_values)) && bet_count < bet_limit && max_values[i + bet_count + 1] < auto_cashout){
        bet_amount <- bet_amount * auto_cashout / (auto_cashout - 1)
        bet_count <- bet_count + 1
        total_loss <- total_loss + bet_amount
      }
      
      # Here, I assume your revenue() function takes 2 arguments: auto_cashout and bet_amount
      if(bet_count < bet_limit) { 
        total_revenue <- total_revenue + revenue(auto_cashout, bet_amount)
      }
    }
  }
  
  return(total_revenue - total_loss)
}

# Get number of betting
number_betting = number_of_betting(max_values, auto_cashout = getOption("auto_cashout"), identify_num = getOption("identify_num"))
print(paste("Number of betting: ", number_betting))

# Implement strategy function
total_revenue <- strategy_evaluation(max_values, auto_cashout = getOption("auto_cashout"), identify_num = getOption("identify_num"), bet_limit = getOption("bet_limit"), initial_bet_amount = getOption("initial_bet_amount"))
print(paste("Total revenue: ", total_revenue))

