# theme selector

available_theme <- setdiff(shinythemes:::allThemes(), c("sandstone", "slate", "darkly", "superhero"))

themeSelector <- function() {
  div(
    div(
      selectInput("shinytheme-selector", "Choose a theme",
                  c("sandstone", available_theme),
                  selectize = FALSE
      )
    ),
    tags$script(
      "$('#shinytheme-selector')
        .on('change', function(el) {
        var allThemes = $(this).find('option').map(function() {
        if ($(this).val() === 'default')
        return 'bootstrap';
        else
        return $(this).val();
        });
        // Find the current theme
        var curTheme = el.target.value;
        if (curTheme === 'default') {
        curTheme = 'bootstrap';
        curThemePath = 'shared/bootstrap/css/bootstrap.min.css';
        } else {
        curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
        }
        // Find the <link> element with that has the bootstrap.css
        var $link = $('link').filter(function() {
        var theme = $(this).attr('href');
        theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
        return $.inArray(theme, allThemes) !== -1;
        });
        // Set it to the correct path
        $link.attr('href', curThemePath);
        });"
    )
  )
}

# This function is used to distribute weights among a number of stocks
assign_weights <- function(n_stocks, tot_weight = 1, weight_choice = seq(0, 1, 0.05)) {
  diff <- (tot_weight / n_stocks) - weight_choice
  diff[diff < 0] <- 1
  avg_floor <- weight_choice[which.min(diff)]
  res <- c(rep(avg_floor, n_stocks - 1),               # floor of average
           tot_weight - (avg_floor * (n_stocks - 1)))  # last stock takes remaining weight
  names(res) <- NULL
  return(res)
}

# This function is used to plot market trend in each different sector 
industry_trend <-function(industry,start_date,end_date)
{
  industries = pull(distinct(SP500_all%>%filter(date=="2021-02-22")%>%pull(sector)%>%as_tibble()),value)
  index = match(industry,industries)
  
  industries_ETF <- c("XLK","XLY","XLC","XLF","XLV","XLP","XLE","XLI","XLU","XLB")
  ETF <- industries_ETF[index]
  
  tq_get(ETF,from = '2000-01-01',to = '2021-03-1',get = 'stock.prices') %>% select(date,adjusted) %>% drop_na() %>% filter(date >= start_date, date <=end_date) %>%
    ggplot(aes(x=date,y=adjusted)) + geom_point(size=0.1) + geom_line(size = 0.3) + 
    labs(x = "",y="SPDR Sector ETF Price",color="Stock", title = "Sector Performance")+
    theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()
}


# This function is used to analyze market trend in each different sector 

industry_analyze <-function(industry,start_date,end_date)
{ 
  industries = pull(distinct(SP500_all%>%filter(date=="2021-02-22")%>%pull(sector)%>%as_tibble()),value)
  index = match(industry,industries)
  
  industries_ETF <- c("XLK","XLY","XLC","XLF","XLV","XLP","XLE","XLI","XLU","XLB", "XLRE")
  stock <- industries_ETF[index]
  
  ## SPY Info
  price_data_SPY <- tq_get("SPY",from = '2000-01-01',to = '2021-03-1',get = 'stock.prices') %>%
    tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'ret',type = 'log') %>%
    drop_na() %>% filter(date >= start_date, date <=end_date) 
  store_spy = cumprod(price_data_SPY[-1] + 1) 
  store_ret <- (store_spy-lag(store_spy))/lag(store_spy) 
  mean_SPY <- lapply(store_ret, mean, na.rm = TRUE)
  annual_SPY <- ((mean_SPY[[1]] + 1)^252) - 1
  sd_SPY <- lapply(store_ret, sd, na.rm = TRUE)[[1]]*252^0.5
  
  ## Sector Info 
  
  price_data_sector <- tq_get(stock,from = '2000-01-01',to = '2021-03-1',get = 'stock.prices') %>%
    tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'ret',type = 'log') %>%
    drop_na() %>% filter(date >= start_date, date <=end_date) 
  store_sector = cumprod(price_data_sector[-1] + 1) 
  store_ret_sector <- (store_sector-lag(store_sector))/lag(store_sector) 
  mean_sector <- lapply(store_ret_sector, mean, na.rm = TRUE)
  annual_sector<- ((mean_sector[[1]] + 1)^252) - 1
  sd_sector <- lapply(store_ret_sector, sd, na.rm = TRUE)[[1]]*252^0.5
  
  result<- data.frame("Investment" = c("SPY","Sector"), "Annualized_Return" = c(annual_SPY,annual_sector), "Annualized_Volatility" = c(sd_SPY,sd_sector))
  
  result
}



# Read in necessary data sets 

SP500_all <- read.csv("Old_Data/SP500_all.csv")
SP_clean <- read.csv("Old_Data/SP500_Individual_clean.csv")


# K-means
SP_clean <- SP_clean %>%
  select(Stock, Volatility, Return) %>% 
  mutate(scaled_volatility = scale(Volatility), 
         scaled_return = scale(Return)) %>% 
  drop_na()
set.seed(8848)
km_SP500 <- kmeans(SP_clean %>% 
                     select(scaled_volatility, 
                            scaled_return), 
                   centers=3, nstart=20)
SP_clean <- SP_clean %>%
  mutate(Cluster = str_c("Cluster ", km_SP500$cluster))

# Make cluster plot
SP500_cluster_graph <- ggplot(SP_clean, aes(x=Volatility, y=Return, color=Cluster, tooltip=Stock)) +
  geom_point() +
  theme_bw() +
  theme(legend.position="right",plot.title = element_text(hjust = 0.5),panel.grid.major = element_line(size = 0, linetype = 'solid',
                                                                                                       colour = "white"))+
  labs(color="Stock Cluster")+ ggtitle("k-means clusters of S&P 500 stocks based on past return and volatility")

# Function for adding a star indicating selected stock on cluster plot
generate_graph_cluster <- function(stock){
  SP500_cluster_graph + 
    geom_point(aes(x = Volatility, y = Return), 
               data = SP_clean %>% filter(Stock == stock), 
               shape = 8, color = "black",size=5.5)
}


# This function is used to read data from the Portfolio Simulation Page to Compare user's Portfolio with S&P 500.

portfolio_fun <- function(stocks, weights) {
  ## Getting Portfolio Data
  price_data <- tq_get(stocks,from = '2000-01-01',to = Sys.Date(),get = 'stock.prices') %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'ret',type = 'log') %>%
    pivot_wider(names_from=symbol,values_from = ret) %>% 
    drop_na()
  ## Calculating Portfolio metrics
  store <- cumprod(price_data[-1] + 1 )
  for (i in (1:ncol(store))) {
    store[i] =  store[i]*weights[i]
  }
  store <- store %>% rowwise() %>% mutate(Portfolio = sum(c_across(1:ncol(store))))
  store['date'] <- price_data['date']
  
  ## Getting S&P 500 data
  min_port_date <- price_data %>% pull(date) %>% min()
  price_data_SPY <- tq_get("SPY",from = '2000-01-01',to = Sys.Date(),get = 'stock.prices') %>%
    tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'ret',type = 'log') %>%
    drop_na() %>% 
    filter(date >= min_port_date)
  store_spy <- cumprod(price_data_SPY[-1] + 1)
  
  ## Combining with S&P 500 data
  store["SPY500"] <- store_spy
  
  ## Generating Graph
  store %>% dplyr::select(date,Portfolio,SPY500) %>% pivot_longer(2:3,names_to = "Investment",values_to = "Return") %>%
    ggplot(aes(x=date,y=Return,color=Investment)) + geom_line() +
    labs(x = "Time",
         y="Daily Investment Return",
         color="Investment", 
         title = "Your Portfolio against S&P 500") + theme_minimal()
}


# This function is used to analyze portfolio and S&P.

port_analyze <- function(stocks, weights) { 
  ## Portfolio Info 
  price_data <- tq_get(stocks,from = '2000-01-01',to = Sys.Date(),get = 'stock.prices') %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = 'daily', col_rename = 'ret', type = 'log') %>%
    pivot_wider(names_from=symbol, values_from = ret) %>% 
    drop_na()
  port_ret <- sum(map_df(price_data[-1] + 1, prod) * weights) - 1
  port_ann_ret <- (port_ret + 1) ^ (252/nrow(price_data)) - 1
  port_daily_ret <- apply(price_data[-1], 1, function(x){sum(x * weights)})
  port_ann_vol <- sd(port_daily_ret, na.rm = T) * sqrt(252)
  # store <- cumprod(price_data[-1] + 1)
  # for (i in (1:ncol(store))) {
  #   store[i] =  store[i] * weights[i]
  # }
  # store_port <- store %>% rowwise() %>% mutate(Portfolio = sum(c_across(1:ncol(store)))) %>% dplyr::select(Portfolio)
  
  ## SPY Info
  min_port_date <- price_data %>% pull(date) %>% min()
  price_data_SPY <- tq_get("SPY",from = '2000-01-01',to = Sys.Date(),get = 'stock.prices') %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = 'daily', col_rename = 'ret', type = 'log') %>%
    drop_na() %>% 
    filter(date >= min_port_date)
  
  spy_ann_ret <- prod(price_data_SPY %>% pull(ret) + 1) ^ (252/nrow(price_data)) - 1
  spy_ann_vol <- sd(price_data_SPY %>% pull(ret), na.rm = T) * sqrt(252)
  
  # store_spy <- cumprod(price_data_SPY %>% pull(ret) + 1)
  # 
  # store_df <- data.frame(SPY = store_spy, 
  #                        Portfolio = store_port)
  # store_df <- (store_df-lag(store_df))/lag(store_df)
  # result <- map_df(store_df, function(x) {
  #   mean_ret <- mean(x, na.rm = TRUE)
  #   sd_ret <- sd(x, na.rm = TRUE)
  #   c("Annualized_Return" = ((mean_ret + 1)^252) - 1, 
  #     "Annualized_Volatility" = sd_ret * 252 ^ 0.5)
  # }) %>% mutate(Investment = colnames(store_df), .before = Annualized_Return)
  
  result <- data.frame(investment = c("S&P500", "portfolio"), 
                       annualized_return = c(spy_ann_ret, port_ann_ret), 
                       annualized_volatiliy = c(spy_ann_vol, port_ann_vol))
  
  result
}


# This function is to conduct Monte Carlo Simulations of the portfolio
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
  g <- ggplot() +
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
  return(g)
}


# Example usage:
#monte_carlo_simulation(tickers = c("AAPL", "MSFT", "GOOG"), weights = c(0.4, 0.4, 0.2), num_simulations = 100, num_years_past = 5, num_years_future = 3, initialPortfolio = 1234)
