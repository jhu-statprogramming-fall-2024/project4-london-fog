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

library(quantmod)
library(ggplot2)
library(dplyr)

monte_carlo_simulation <- function(tickers, weights, num_simulations, num_years_past, num_years_future) {
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
  
  # Calculate daily returns, handling NA values for newly listed stocks
  daily_returns <- ROC(portfolio_data, type = "discrete")
  
  # Split tickers into those with full history and those with partial data
  full_history_tickers <- tickers[apply(!is.na(portfolio_data), 2, all)]
  partial_history_tickers <- setdiff(tickers, full_history_tickers)
  
  # Check if more than half of the portfolio has partial data
  if (length(partial_history_tickers) / length(tickers) > 0.5) {
    stop(paste("More than half of the portfolio has partial data. Please decrease the number of years. The tickers with partial data are:", paste(partial_history_tickers, collapse = ", ")))
  }
  
  # Warning if partial data is available for some stocks
  if (length(partial_history_tickers) > 0) {
    warning(paste("Partial data is available for the following tickers:", paste(partial_history_tickers, collapse = ", ")))
  }
  
  # Perform Monte Carlo simulation with full history stocks first
  if (length(full_history_tickers) > 0) {
    full_history_data <- portfolio_data[, full_history_tickers, drop = FALSE]
    full_history_returns <- na.omit(ROC(full_history_data, type = "discrete"))
    
    # Calculate portfolio returns for full history stocks
    full_history_weights <- weights[full_history_tickers %in% tickers]
    full_history_portfolio_returns <- full_history_returns %*% full_history_weights
    
    # Calculate mean and standard deviation of returns
    mean_return <- mean(full_history_portfolio_returns, na.rm = TRUE)
    sd_return <- sd(full_history_portfolio_returns, na.rm = TRUE)
    
    # Simulation parameters
    num_days <- 252 * num_years_future
    initial_price <- as.numeric(last(full_history_data %*% full_history_weights, na.rm = TRUE))
    
    # Monte Carlo Simulation for full history stocks
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
    stop("No stocks with full history available for simulation.")
  }
  
  # Add in partial history stocks once their data becomes available
  if (length(partial_history_tickers) > 0) {
    partial_history_data <- portfolio_data[, partial_history_tickers, drop = FALSE]
    partial_history_returns <- ROC(partial_history_data, type = "discrete")
    combined_returns <- merge(full_history_returns, partial_history_returns, all = FALSE)
    combined_returns <- na.omit(combined_returns)
    
    # Calculate portfolio returns including partial history stocks
    combined_weights <- weights[match(colnames(combined_returns), tickers)]
    combined_portfolio_returns <- combined_returns %*% combined_weights
  } else {
    combined_portfolio_returns <- full_history_portfolio_returns
  }
  
  # Calculate median forecast price
  median_forecast <- apply(simulation_results, 1, median)
  median_forecast_df <- data.frame(Date = seq(end_date + 1, by = "day", length.out = num_days), Price = median_forecast)
  
  # Prepare data for plotting
  simulation_df <- as.data.frame(simulation_results)
  simulation_df$Date <- seq(end_date + 1, by = "day", length.out = num_days)
  simulation_df_long <- pivot_longer(simulation_df, cols = -Date, names_to = "Simulation", values_to = "Price")
  
  historical_df <- data.frame(Date = index(portfolio_data), Price = rowSums(portfolio_data * weights, na.rm = TRUE))
  
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
