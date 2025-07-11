# 1. Loading the data. 
getwd()

setwd("~/Desktop/GWU/Fall 2024/Stochastic Foundations- Probability Models - Korel Gundem/Assignment 4")


data <- read.table("prices.txt", header = TRUE, sep = "", stringsAsFactors = FALSE)
head(data)

#Extracting stock columns from data.
msft <- data$msft
ge <- data$ge
ford <- data$ford

# calculating daily returns of msft, ge and ford.
msft_ret <- diff(msft) / msft[- length(msft)]
ge_ret <- diff(ge) / ge[- length(ge)]
ford_ret <- diff(ford) / ford[- length(ford)]

stock_returns <- data.frame(msft_ret, ge_ret, ford_ret)
head(stock_returns)

# Calculating Mean, Variance and Covariance of each stock (msft, ge & ford)
## 1. Mean
mean_stock_returns <- colMeans(stock_returns, na.rm = TRUE)
head(mean_stock_returns)

## 2. variance
stock_variances <- apply(stock_returns, 2, var, na.rm = TRUE)
head(stock_variances)

## 3. Covariance
cov_matrix_stock <- cov(stock_returns, use = "complete.obs")
head(cov_matrix_stock)

# Now, defining possible options of portfolio allocations from: 0%, 25%, 50%. 
portfolio_allocations <- c(0,0.25,0.5)

# Calculating portfolio returns, variance and coefficient of variance. 
## Defining a function. 
portfolio_stats <- function(weights, mean_stock_returns, cov_matrix_stock) { 
  
  #Portfolio returns
  portfolio_returns <- sum(weights * mean_stock_returns)
  
  # Portfolio variance 
  portfolio_variance <- t(weights) %*% cov_matrix_stock %*% weights
  
  #Portfolio standard-deviation
  portfolio_std_dev <- sqrt(portfolio_variance)
  
  #Portfolio Coefficient of Variance
  portfolio_cv <- portfolio_std_dev/portfolio_returns
  
  return(c(portfolio_returns, portfolio_std_dev, portfolio_cv))
}

# Storing results in a data-set. 
results <- data.frame(msft= numeric(), ge = numeric(), ford = numeric(),
                      return = numeric(), std_dev = numeric(), cv = numeric())

# Looping all possible combinations of allocations. 
for (w_msft in portfolio_allocations) {
  for(w_ge in portfolio_allocations) {
    for(w_ford in portfolio_allocations){
      if (w_msft + w_ge + w_ford == 1){
        weights <- c(w_msft, w_ge, w_ford)
        
        stats <- portfolio_stats(weights, mean_stock_returns, cov_matrix_stock)
        
        results <- rbind(results, c(w_msft, w_ge, w_ford, stats))
      }
    }
  }
}

# Naming the columns of results. 
colnames(results) <- c("MSFT", "GE", "Ford", "Return", "Std_Dev", "CV")

#Q3. a. Optimal mix of stocks that would maximize the expected portfolio return. 
max_expected_portfolio_return <- results[which.max(results$Return),]

#Q3. b. Optimal mix of stocks that would minimize the standard deviation of the portfolio return. 
min_std_dev_portfolio <- results[which.min(results$Std_Dev),]

#Q3. c. Optimal mix of stocks that would minimize the coefficient of variation of the portfolio return. 
min_cv_portfolio <- results[which.min(results$CV),]

# print the results 
# #Q3. a.
print("Portfolio that maximizes the expected portfolio return:")
print(max_expected_portfolio_return)

# print the results 
# #Q3. b.
print("Portfolio that minimizes the standard deviation of the portfolio return:")
print(min_std_dev_portfolio)

# print the results 
# #Q3. c.
print("Portfolio that minimizes the coefficient of variation of the portfolio return:")
print(min_cv_portfolio)

write.csv(results, "Portfolio Optimization Results.csv", row.names = FALSE)
