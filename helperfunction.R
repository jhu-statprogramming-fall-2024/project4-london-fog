

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
  
  industries_ETF <- c("XLK","XLY","XLC","XLF","XLV","XLP","XLE","XLI","XLU","XLB")
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