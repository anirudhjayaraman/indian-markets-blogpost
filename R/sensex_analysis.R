require(zoo)
require(forecast)
require(tidyverse)

main_dir <- "~/Documents/git/indian-markets-blogpost/"

sensex <- read.csv(paste0(main_dir, 'data/BSESN.csv'))


sensex$Date <- as.Date(sensex$Date)
sensex[,-1] <- sapply(sensex[,-1], function(x){as.numeric(as.character(x))})
sensex$Weekday <- factor(weekdays(sensex$Date))
sensex$Year <- as.numeric(substr(sensex$Date,1,4))
sensex$Month <- months(sensex$Date)
sensex <- sensex[-which(is.na(sensex$Adj.Close)),]
logreturns <- log(sensex$Adj.Close)[2:nrow(sensex)] - 
  log(sensex$Adj.Close)[1:(nrow(sensex)-1)]
sensex$Returns <- 0
sensex$Returns[2:nrow(sensex)] <- logreturns

# Sensex Closing Values
sensex_close <- zoo(sensex$Adj.Close, order.by = sensex$Date)
# plot(sensex_close)

# Daily Logarithmic Returns
sensex_returns <- zoo(sensex$Returns, order.by = sensex$Date)
# plot(sensex_returns)
par(mfrow = c(3,3))

png(paste0(main_dir, "results/mean_annualized_returns.png"))

for(investment_horizon in 1:9){
  
  days_per_year <- 252
  start_date_index = 1
  investment_frequency = 15 
  investment_installment = 50000
  investment_indices <- seq.int(from = start_date_index, 
                                by = investment_frequency, 
                                to = (days_per_year*investment_horizon) + start_date_index)
  start_max = nrow(sensex) - (days_per_year*investment_horizon)
  average_returns <- rep(0,start_max)
  for(start_date_index in 1:start_max){
    investment_indices <- seq.int(from = start_date_index, 
                                  by = investment_frequency, 
                                  to = (days_per_year*investment_horizon) + start_date_index)
    entry_points <- sensex$Adj.Close[investment_indices][1:(length(investment_indices)-1)]
    exit_point <- sensex$Adj.Close[investment_indices[length(investment_indices)]]
    return_ratios <- exit_point / entry_points
    annualized_return_ratios <- return_ratios
    for(j in 1:length(return_ratios)){
      annualized_return_ratios[j] <- return_ratios[j]^(days_per_year/
                                                         (investment_indices[length(investment_indices)]-
                                                            investment_indices[j]))
    }
    average_returns[start_date_index] <- 100 * mean(annualized_return_ratios - 1)
  }
  
  average_annualized_returns <- mean(average_returns)
  message('Average annualized returns are ',
          round(average_annualized_returns,2),"%")
  plot(density(average_returns), main = paste0('Mean Annualized Returns of ',round(average_annualized_returns,2),'% after ',investment_horizon,'Y'))
  polygon(density(average_returns), col = 'black')
  
}

# Investing every month (20 trading days)
# Average annualized returns are 24.33%
# Average annualized returns are 19.59%
# Average annualized returns are 16.95%
# Average annualized returns are 16.93%
# Average annualized returns are 17.55%
# Average annualized returns are 18.21%
# Average annualized returns are 17.29%
# Average annualized returns are 16.8%
# Average annualized returns are 15.41%

par(mfrow = c(1,1))




