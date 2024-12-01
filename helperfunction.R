

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


# This function is used to read data from the Portfolio Simulation Page to Compare user's Portfolio with S&P 500.

portfolio_fun <- function(stocks, weights) {
  ## Getting Portfolio Data
  price_data <- tq_get(stocks,from = '2000-01-01',to = Sys.Date(),get = 'stock.prices') %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'ret',type = 'log') %>%
    pivot_wider(names_from=symbol,values_from = ret) %>% 
    drop_na()
  ## Calculating Portfolio metrics
  store = cumprod(price_data[-1] + 1 )
  for (i in (1:ncol(store))) {
    store[i] =  store[i]*weights[i]
  }
  store <- store %>% rowwise() %>% mutate(Portfolio = sum(c_across(1:ncol(store))))
  store['date'] = price_data['date']
  
  ## Getting S&P 500 data
  min_port_date <- price_data %>% pull(date) %>% min()
  price_data_SPY <- tq_get("SPY",from = '2000-01-01',to = Sys.Date(),get = 'stock.prices') %>%
    tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'ret',type = 'log') %>%
    drop_na() %>% 
    filter(date >= min_port_date)
  store_spy = cumprod(price_data_SPY[-1] + 1) 
  
  ## Combining with S&P 500 data
  store["SP_500"] = store_spy
  
  ## Generating Graph
  store %>% select(date,Portfolio,SP_500) %>%pivot_longer(2:3,names_to = "Investment",values_to = "Return") %>%
    ggplot(aes(x=date,y=Return,color=Investment)) + geom_line() +
    labs(x = "",
         y="Daily Investment Worth",
         color="Investment", 
         title = "Your Portfolio against S&P 500")+
    theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()
}


# This function is used to analyze portfolio and S&P.

port_analyze <-function(stocks, weights) { 
  ## Portfolio Info 
  price_data <- tq_get(stocks,from = '2000-01-01',to = Sys.Date(),get = 'stock.prices') %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = 'daily', col_rename = 'ret', type = 'log') %>%
    pivot_wider(names_from=symbol, values_from = ret) %>% 
    drop_na()
  store <- cumprod(price_data[-1] + 1 )
  for (i in (1:ncol(store))) {
    store[i] =  store[i]*weights[i]
  }
  store_port <- store %>% rowwise() %>% mutate(Portfolio = sum(c_across(1:ncol(store)))) %>% select(Portfolio)
  
  ## SPY Info
  min_port_date <- price_data %>% pull(date) %>% min()
  price_data_SPY <- tq_get("SPY",from = '2000-01-01',to = Sys.Date(),get = 'stock.prices') %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = 'daily', col_rename = 'ret', type = 'log') %>%
    drop_na() %>% 
    filter(date >= min_port_date)
  store_spy <- cumprod(price_data_SPY %>% pull(ret) + 1)
  
  store_df <- data.frame(SPY = store_spy, 
                         Portfolio = store_port)
  store_df <- (store_df-lag(store_df))/lag(store_df)
  result <- map_df(store_df, function(x) {
    mean_ret <- mean(x, na.rm = TRUE)
    sd_ret <- sd(x, na.rm = TRUE)
    c("Annualized_Return" = ((mean_ret + 1)^252) - 1, 
      "Annualized_Volatility" = sd_ret * 252 ^ 0.5)
  }) %>% mutate(Investment = colnames(store_df), .before = Annualized_Return)
  
  result
  
}