library(quantmod)
library(ggplot2)
library(dplyr)

monte_carlo_simulation <- function(tickers, weights, num_simulations, num_years) {
  # Get stock data for the tickers
  end_date <- Sys.Date()
  start_date <- end_date - years(num_years)
  
  stock_data <- list()
  for (ticker in tickers) {
    getSymbols(ticker, src = 'yahoo', from = start_date, to = end_date, auto.assign = TRUE)
    stock_data[[ticker]] <- Ad(get(ticker))
  }
  
  # Merge stock data into one data frame
  portfolio_data <- do.call(merge, stock_data)
  colnames(portfolio_data) <- tickers
  
  # Calculate daily returns
  daily_returns <- na.omit(ROC(portfolio_data, type = "discrete"))
  
  # Portfolio returns
  portfolio_returns <- daily_returns %*% weights
  
  # Calculate mean and standard deviation of returns
  mean_return <- mean(portfolio_returns)
  sd_return <- sd(portfolio_returns)
  
  # Simulation parameters
  num_days <- 252 * num_years
  initial_price <- as.numeric(last(portfolio_data %*% weights))
  
  # Monte Carlo Simulation
  simulation_results <- matrix(NA, nrow = num_days, ncol = num_simulations)
  for (i in 1:num_simulations) {
    prices <- numeric(num_days)
    prices[1] <- initial_price
    for (t in 2:num_days) {
      prices[t] <- prices[t-1] * exp((mean_return - 0.5 * sd_return^2) + sd_return * rnorm(1))
    }
    simulation_results[, i] <- prices
  }
  
  # Prepare data for plotting
  simulation_df <- as.data.frame(simulation_results)
  simulation_df$Date <- seq(end_date + 1, by = "day", length.out = num_days)
  simulation_df_long <- pivot_longer(simulation_df, cols = -Date, names_to = "Simulation", values_to = "Price")
  
  historical_df <- data.frame(Date = index(portfolio_data), Price = as.numeric(portfolio_data %*% weights))
  
  # Calculate median forecast price
  median_forecast <- apply(simulation_results, 1, median)
  median_forecast_df <- data.frame(Date = seq(end_date + 1, by = "day", length.out = num_days), Price = median_forecast)
  
  # Plot the results
  ggplot() +
    geom_line(data = historical_df, aes(x = Date, y = Price), color = "blue", size = 1) +
    geom_line(data = simulation_df_long, aes(x = Date, y = Price, group = Simulation), color = "grey", linetype = "dotted", alpha = 0.5) +
    geom_line(data = median_forecast_df, aes(x = Date, y = Price), color = "blue", size = 1, linetype = "dashed") +
    geom_vline(xintercept = end_date, linetype = "dashed", color = "black") +
    annotate("text", x = end_date, y = max(historical_df$Price, na.rm = TRUE), label = "Today", hjust = -0.1, vjust = -13, color = "grey") +
    labs(title = "Monte Carlo Simulation of Portfolio",
         x = "Date",
         y = "Portfolio Price") +
    theme_classic() +
    theme(legend.position = "none")
}


# Example usage:
monte_carlo_simulation(tickers = c("AAPL", "MSFT", "GOOG"), weights = c(0.4, 0.4, 0.2), num_simulations = 100, num_years = 5)


## New better version

library(quantmod)
library(ggplot2)
library(dplyr)

monte_carlo_simulation <- function(tickers, weights, num_simulations, num_years_past, num_years_future, initialPortfolio = 10000) {
  # Check if portfolio weights add to 1
  if (abs(sum(weights) - 1) > 1e-6) {
    stop("Portfolio weights do not add up to 1. Please adjust the weights.")
  }
  
  # Warn if the number of future years exceeds the number of past years
  if (num_years_future > num_years_past) {
    warning("Number of future years exceeds the number of past years. Consider reducing the future years for more reliable predictions.")
  }
  
  # Get stock data for the tickers
  end_date <- Sys.Date()
  start_date <- end_date - years(num_years_past)
  
  stock_data <- list()
  for (ticker in tickers) {
    stock_data[[ticker]] <- tryCatch({
      Ad(getSymbols(ticker, src = 'yahoo', from = start_date, to = end_date, auto.assign = FALSE))
    }, error = function(e) {
      stop(paste("Invalid ticker:", ticker, "- Please check the ticker symbol."))
    })
  }
  
  # Merge stock data into one data frame
  portfolio_data <- Reduce(function(x, y) merge(x, y, all = FALSE), stock_data)
  
  # Ensure the number of columns matches the tickers before assigning column names
  if (ncol(portfolio_data) == length(tickers)) {
    colnames(portfolio_data) <- tickers
  } else {
    stop("Mismatch in the number of columns after merging stock data. Please check the availability of data for each ticker.")
  }
  
  # Fill in missing values by carrying forward the last observation
  portfolio_data <- na.locf(portfolio_data, na.rm = FALSE)
  portfolio_data <- na.locf(portfolio_data, fromLast = TRUE, na.rm = FALSE)
  
  # Calculate the initial historical portfolio value by weighted sum of the stock prices
  portfolio_data_matrix <- as.matrix(portfolio_data)
  unscaled_historical_prices <- portfolio_data_matrix %*% weights
  
  # Convert the result to a vector to avoid shape issues
  unscaled_historical_prices <- as.vector(unscaled_historical_prices)
  
  # Back-scale the historical portfolio values so that the last value matches the initialPortfolio value
  scaling_factor <- initialPortfolio / tail(unscaled_historical_prices, 1)
  scaled_historical_prices <- unscaled_historical_prices * scaling_factor
  
  # Create historical data frame for plotting
  historical_df <- data.frame(Date = index(portfolio_data), Price = scaled_historical_prices)
  
  # Calculate daily returns from the scaled historical prices
  portfolio_returns <- ROC(scaled_historical_prices, type = "discrete", na.pad = FALSE)
  
  # Perform Monte Carlo simulation using the historical returns
  if (any(!is.na(portfolio_returns))) {
    # Calculate mean and standard deviation of returns
    mean_return <- mean(portfolio_returns, na.rm = TRUE)
    sd_return <- sd(portfolio_returns, na.rm = TRUE)
    
    # Set initial price to the given initial portfolio value
    initial_price <- initialPortfolio
    
    # Simulation parameters
    num_days <- 252 * num_years_future
    
    # Monte Carlo Simulation
    simulation_results <- matrix(NA, nrow = num_days, ncol = num_simulations)
    for (i in 1:num_simulations) {
      prices <- numeric(num_days)
      prices[1] <- initial_price
      for (t in 2:num_days) {
        prices[t] <- prices[t-1] * exp((mean_return - 0.5 * sd_return^2) + sd_return * rnorm(1))
      }
      simulation_results[, i] <- prices
    }
  } else {
    stop("No sufficient data available to perform simulations.")
  }
  
  # Calculate median forecast price
  median_forecast <- apply(simulation_results, 1, median)
  median_forecast_df <- data.frame(Date = seq(end_date + 1, by = "day", length.out = num_days), Price = median_forecast)
  
  # Prepare data for plotting
  simulation_df <- as.data.frame(simulation_results)
  simulation_df$Date <- seq(end_date + 1, by = "day", length.out = num_days)
  simulation_df_long <- pivot_longer(simulation_df, cols = -Date, names_to = "Simulation", values_to = "Price")
  
  # Plot the results
  ggplot() +
    geom_line(data = historical_df, aes(x = Date, y = Price), color = "blue", size = 0.7) +
    geom_line(data = simulation_df_long, aes(x = Date, y = Price, group = Simulation), color = "grey", linetype = "dotted", alpha = 0.3) +
    geom_line(data = median_forecast_df, aes(x = Date, y = Price), color = "blue", size = 0.7, linetype = "dashed") +
    geom_vline(xintercept = as.numeric(end_date), linetype = "dashed", color = "grey") +
    annotate("text", x = end_date, y = max(historical_df$Price, na.rm = TRUE), label = "Today", hjust = 0.5, vjust = 2, color = "grey10", size =3) +
    labs(title = "Monte Carlo Simulation of Portfolio",
         x = "Date",
         y = "Portfolio Price") +
    theme_classic() +
    theme(legend.position = "none")
}


# Example usage:
monte_carlo_simulation(tickers = c("AAPL", "MSFT", "GOOG"), weights = c(0.4, 0.4, 0.2), num_simulations = 100, num_years_past = 5, num_years_future = 3)
