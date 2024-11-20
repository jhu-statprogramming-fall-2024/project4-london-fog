# Final Project 777

# Load packages
library(tidyverse)
library(ggthemes)
library(plotly) 

# pull data 
library(quantmod)
library(rvest)
library(tidyquant)

# analysis
library(DescTools)
library(stringr)
library(lubridate)

# shiny packages
library(shiny)
library(shinyWidgets)
library(shinythemes)

library(rsconnect)



source("helperfunction.R")
source("Text.R")

# Get S&P 500 stock tickers from Wikipedia
sp500_url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
SP500_info <- read_html(sp500_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]]  # The first table contains the S&P 500 list
SP500_info


## Take a while to pull, so we saved it in advance
# write.csv(tq_get(SP500_info$Symbol, get = "stock.prices", from = Sys.Date()-1, to = Sys.Date()), 
#           file = "S&P500_all.csv")

SP500_all <- read.csv("S&P500_all.csv")

# Join the data with info and daily price
SP500_all <- left_join(SP500_all, SP500_info, by = join_by(symbol == Symbol))




## Main APP Part

# Shiny ui function

# Custom CSS for improved aesthetics
custom_css <- "
body {
  font-family: 'Lato', sans-serif;
  background-color: #f7f9fb;
  color: #333333;
}

.navbar-default {
  background-color: #34495e;
  border-color: #2c3e50;
}

.navbar-default .navbar-brand {
  color: #ecf0f1;
  font-size: 18px;
  font-weight: bold;
}

.navbar-default .navbar-nav > li > a {
  color: #ecf0f1;
}

.navbar-default .navbar-nav > .active > a {
  background-color: #2c3e50;
}

.sidebar {
  background-color: #ffffff;
  border-radius: 10px;
  padding: 15px;
}

.panel-title {
  font-weight: bold;
  font-size: 16px;
}

h2, h3, h4 {
  color: #34495e;
}

#footer {
  text-align: center;
  padding: 20px;
  background-color: #34495e;
  color: #ecf0f1;
}
"

ui <- navbarPage(
  "How to Survive in the U.S. Stock Market",
  theme = shinytheme("flatly"),
  
  # Add custom CSS
  tags$head(tags$style(HTML(custom_css))),
  
  
  # Tab Intro
  tabPanel("Introduction of this App",
           icon = icon("compass"),
           tags$br(
             p(intro_1),
             br(),
             p(intro_2),
             br(),
             p(intro_3),
             br(),
             p(intro_4)
           ),
           div(img(src='Wall_Pic.png',width="50%"), style="text-align: center;"),
           
  ),
  
  
  # Tab Data Summary
  
  tabPanel("Data Summary",
           
           icon = icon("atlas"),
           
           fluidPage(
             tags$br(br(),
                     p(data_summary_1),
                     br(),
                     p(data_summary_2),
             ),
             
             DT::DTOutput("Overall_table"),
             
             tags$br(br(),
                     p(data_summary_3),
             ),
             
             tags$a(href="https://eoddata.com/symbols.aspx?AspxAutoDetectCookieSupport=1", "Stock Reference List"),
             
             br(),
             
             textInput(inputId = "Summary_Stock_Selected",label = "Your Stock of Interest",value = "AAPL"),
             
             
             DT::DTOutput("Summary_stock"),
             
           )),
  
  
  # Tab Stock Trends
  tabPanel("Stock Trends",
           
           icon = icon("chart-line"),
           
           fluidPage(
             sidebarLayout(
               
               # First Button chooses your interested market indicator
               sidebarPanel(
                 radioButtons("market_indicator", "Select Your Interested Market Indicator",
                              c("Individual Stocks (ex. AAPL for Apple Inc.)" = "individual",
                                "Top ETFs" = "Market")),
                 br(),
                 
                 # Option reactive to the first 
                 uiOutput("option21"),
                 
                 br(),
                 
                 uiOutput("option22"),
                 
                 br(),
                 
                 uiOutput("third_option"),
                 
                 br(),
                 
                 # Time Frame 
                 sliderInput("Trend_Time",
                             "Select Your Interested Time Frame",
                             value = c(2010,2024),
                             min = 2005,
                             max = 2025, 
                             animate = T,
                             sep = ""),
                 
                 br(),
                 
                 # Threshold value for line charts
                 numericInput("target_value",h5("Threshold Value: "),value=0)
                 
               ),
               
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Instruction",
                                      br(),
                                      
                                      strong(stock_trend_1),
                                      
                                      tags$ol(
                                        br(),
                                        tags$li(stock_trend_2),
                                        br(),
                                        tags$li(stock_trend_3),
                                        br()
                                        
                                      ),
                                      p(stock_trend_4),
                                      br(),
                                      div(img(src='dow-sp500-nasdaq.png',width="60%"), style="text-align: center;"),
                                      br()
                             ),
                             tabPanel("Key Terms",
                                      
                                      br(),
                                      br(),
                                      p(stock_trend_jargon_1),
                                      br(),
                                      p(stock_trend_jargon_2),
                                      br(),
                                      p(stock_trend_jargon_3),
                                      br(),
                                      p(stock_trend_jargon_4),
                                      br(),
                                      p(stock_trend_jargon_5),
                                      br()
                             ),
                             tabPanel("Market Trend Plot", plotOutput("market_trend_plot")))
               )
             )
           )
           
  ),
  
  
  
  # Tab Market Distribution
  tabPanel("Market Distribution",
           
           icon = icon("chart-bar"),
           
           fluidPage(
             
             sidebarPanel( 
               
               radioButtons("sector_indicator", "Select Your Interested Aspect",
                            c("Sector Distribution" = "overall",
                              "Individual Sector" = "individual"
                              
                            )),
               
               
               uiOutput("option_dist_2"),
               uiOutput("option_dist_3"),
               uiOutput("option_dist_1")
               
               
             ),
             
             mainPanel(tabsetPanel(type = "tabs",
                                   tabPanel("Instruction",
                                            br(),
                                            strong(market_dis_instruction_1),
                                            tags$ol(
                                              br(),
                                              tags$li(market_dis_instruction_2),
                                              br(),
                                              tags$li(market_dis_instruction_3),
                                              br()
                                              
                                            ),
                                            p(market_dis_instruction_4),
                                            div(img(src='pexels-photo-187041.png',width="60%"), style="text-align: center;"),
                                            br()
                                            
                                            
                                            
                                   ),
                                   tabPanel("Sector Plot", plotOutput("dist_graph"), DT::DTOutput("sector_compare")))
                       
                       
             ))
           
  ),
  
  
  
  # Tab Stock Selection
  tabPanel("Stock Selection",
           
           icon = icon("diagnoses"),
           
           fluidPage(
             
             sidebarPanel(titlePanel("Pick Your Stock of Interest"),
                          
                          tags$br(br(),
                                  p(selection_0),
                                  
                          ),
                          
                          textInput(inputId = "Stock_Selected",label = "Stock of Interest",value = "AAPL"),
                          
                          tags$br(
                            br(),
                            p(selection_1),
                            br()),
                          
                          DT::DTOutput("info")),
             
             mainPanel(tabsetPanel(type="tabs", 
                                   tabPanel("Instruction",
                                            br(),
                                            
                                            strong(selection_instruction_1),
                                            
                                            tags$ol(
                                              br(),
                                              tags$li(selection_instruction_2),
                                              br(),
                                              tags$li(selection_instruction_3),
                                              br()
                                              
                                            ),
                                            p(selection_instruction_4),
                                            br(),
                                            div(img(src='image-neba-articl.png',width="60%"), style="text-align: center;"),
                                            br()
                                            
                                   ),
                                   tabPanel("Model Building",
                                            br(),
                                            br(),
                                            p(selection_model_1),
                                            br(),
                                            p(selection_model_2),
                                            br(),
                                            p(selection_model_3),
                                            br(),
                                            p(selection_model_4),
                                            br()
                                            
                                   ),
                                   tabPanel("S&P 500 Cluster", plotOutput("Cluster",click = "my_click"),
                                            br(), DT::DTOutput("cluster_info"))
             ))
             
           )
           
  ),
  
  # Tab Understand Your Portfolio
  tabPanel("Understand Your Portfolio",
           
           icon = icon("search-dollar")
           
  ),
  
  # Tab Portfolio Optimization
  tabPanel("Portfolio Optimization",
           
           icon = icon("coins")
           
  )
  
)



# Shiny Server Function
server <- function(input, output) {
  
  # Data summary Tab 
  
  output$Overall_table <- DT::renderDT(expr = SP500_info %>% rename(Company = Security),
                                       options = list(pageLength = 10, lengthChange = FALSE, searching = F))
  
  output$Summary_stock <- DT::renderDT(expr = tq_get(input$Summary_Stock_Selected, 
                                                     get = 'stock.prices',
                                                     from = Sys.Date()-365*10, to = Sys.Date()) %>% 
                                         mutate(Stock = symbol, Date = date,
                                                `Open Price`= open, `Close Price`= close, `Highest Price`= high, `Lowest Price`= low, `Volume` = volume) %>% 
                                         select(Stock, Date, `Open Price`, `Close Price`, `Highest Price`, `Lowest Price`, `Volume`) %>% 
                                         arrange(desc(Date)) %>% 
                                         mutate(across(where(is.numeric), ~ round(.x, 3))),
                                       options = list(pageLength = 12, lengthChange = FALSE, sDom  = '<"top">flrt<"bottom">ip'))
  
  # Stock Trend Tab
  
  output$option21 <- renderUI(
    if (input$market_indicator == "individual")
    {textInput(inputId = "Select_Stock_01", label="First Stock of Interest",value = "AAPL")}
    else{})
  
  output$option22 <- renderUI(
    if (input$market_indicator == "individual")
    {textInput(inputId = "Select_Stock_02", label="Second Stock of Interest",value = "TSLA")}
    else{})                
  
  
  output$third_option <- renderUI(
    if (input$market_indicator == "individual")
    {radioButtons("stock_stats", "Select specific stock market information",
                  c("Daily Investment Worth" = "open",
                    "Daily Investment Worth Comparing with S&P 500" = "Compare",
                    "Daily Transaction Volume" = "volume"))}
    else{})
  
  output$market_trend_plot <- renderPlot(
    if (input$market_indicator == "individual" & input$stock_stats == "open")
    {tq_get(c(input$Select_Stock_01,input$Select_Stock_02),from = '2000-01-01',to = Sys.Date(), get = 'stock.prices') %>%
        drop_na()%>%
        mutate(year=year(date), month = month(date))%>%
        filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])%>%
        ggplot(aes(x=date,y=close,color=symbol)) + geom_line()+
        labs(x="", y="Single Share Price", color="Stock", title = "Price of a single share of stock")+
        geom_hline(yintercept= input$target_value,color="black",size = 0.5,alpha=0.5) +
        theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()}
    
    
    else if (input$market_indicator == "individual" & input$stock_stats == "volume")
    {tq_get(c(input$Select_Stock_01,input$Select_Stock_02),from = '2000-01-01',to = Sys.Date(), get = 'stock.prices') %>%
        mutate(year=year(date), month = month(date))%>%
        filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])%>%
        ggplot(aes(x=date,y=volume/1000000,color=symbol)) + geom_line()+
        labs(x="", y="Daily Transcation (in millions)",color="Stock", title = "Daily transction volume of stock")+
        geom_hline(yintercept= input$target_value,color="black",size = 0.5,alpha=0.5) +
        theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()}
    
    
    else if (input$market_indicator == "individual" & input$stock_stats == "Compare")
    {tq_get(c(input$Select_Stock_01,input$Select_Stock_02,"SPY"),from = '2000-01-01',to = Sys.Date(),get = 'stock.prices') %>%
        mutate(year=year(date), month = month(date))%>%
        filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])%>%
        ggplot(aes(x=date,y=close,color=symbol)) + geom_line()+
        labs(x="", y="Single Share Price",color="Stock", title = "Price of a single share")+
        geom_hline(yintercept= input$target_value,color="black",size = 0.5,alpha=0.5) +
        theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()}
    
    # Market subplot
    else if (input$market_indicator == "Market")
    {tq_get(c("SPY", "QQQ", "VTI"),from = '2000-01-01',to = Sys.Date(), get = 'stock.prices') %>%
        mutate(year=year(date), month = month(date))%>%
        filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])%>%
        ggplot(aes(x=date,y=close,color=symbol)) + geom_line()+
        labs(x="", y="Single Share Price", color="Stock", title = "Price of a single share of stock")+
        theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()}
    
  )
  
  
  
  
  
  # Market Distribution Tab
  
  output$option_dist_1 <- renderUI(
    if (input$sector_indicator != "individual")
    {radioButtons("graph_type", "Select Your Preferred Visualization",
                  c("Counts of Each Sector" = "Counts",
                    "Volume of Each Sector" = "Volume"))}
    else{selectInput(inputId = "interested_sector", 
                     label = "Select Your Interested Sector",
                     choices = (distinct(SP500_all%>%filter(date=="2021-02-22")%>%pull(sector)%>%as_tibble()))$value, 
                     selected = "Information Technology")})
  
  
  output$option_dist_2 <- renderUI(
    if (input$sector_indicator == "individual")
    {textInput(inputId = "start_date",label = "Start Date in yyyy-mm-dd",value = "2000-01-01")}
    else{})
  
  output$option_dist_3<- renderUI(
    if (input$sector_indicator == "individual")
    {textInput(inputId = "end_date",label = "End Date in yyyy-mm-dd",value = "2020-01-01")}
    else{})
  
  
  output$dist_graph <- renderPlot(
    if (input$sector_indicator != "individual" & input$graph_type == "Counts")
    { 
      SP500_info %>% 
        mutate(sector = `GICS Sector`) %>% 
        group_by(sector) %>% mutate(count=n()) %>% select(sector,count) %>% distinct() %>%
        ggplot(aes(x = reorder(sector, -count), y = count, fill = sector)) +
        geom_bar(stat = "identity")+
        theme_economist() +
        labs(x = '', y = 'Counts Among S&P500', color = "Sector",
             title = "Distribution of Various Sector Among S&P500 Companies") +
        theme(legend.position="right",plot.title = element_text(hjust = 0.5),
              legend.text=element_text(size=12),
              axis.text.x = element_text(face = "bold", size = 10, angle=-50))
      
    }
    
    
    else if (input$sector_indicator != "individual" & input$graph_type == "Volume")
    {SP500_all%>%
        mutate(sector = `GICS Sector`) %>% 
        select(sector,volume)%>%
        group_by(sector)%>% 
        summarise(total_v = sum(volume))%>%
        ggplot() +
        geom_col(mapping = aes(x = reorder(sector, -total_v),y=total_v/1000000000,fill=sector))+
        theme_economist() +
        labs(x = '', y = 'Total Daily Trading Volume (in billion)', title = "Volumes of different sectors among S&P500 companies", color = "Sector") +
        scale_fill_hue(name = "Sector")+ 
        theme(legend.position="right",plot.title = element_text(hjust = 0.5),
              legend.text=element_text(size=12),
              axis.text.x = element_text(face = "bold", 
                                         size = 10, angle=-50))}
    else
    {
      industry_trend(input$interested_sector,input$start_date,input$end_date)
    }
    
  )
  
  output$sector_compare <-DT::renderDT(expr =
                                         
                                         if(input$sector_indicator == "individual")
                                         {
                                           industry_analyze(input$interested_sector,input$start_date,input$end_date)
                                         },
                                       options = list(pageLength = 5, lengthChange = FALSE,info = FALSE,dom='t',searching=F,sDom  = '<"top">lrt<"bottom">ip')
                                       
  )
  
  
}


# Complete app with ui and server components
shinyApp(ui, server)




