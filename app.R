# Final Project 777

library(tidyverse)
library(ggthemes)
library(plotly) 

library(quantmod)
library(rvest)
library(tidyquant)

library(DescTools)
library(stringr)
library(lubridate)

library(shiny)
library(shinyWidgets)
library(shinythemes)

library(rsconnect)



source("helperfunction.R")


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




# All Text 
# Tab 1 
intro_1 <- "Are you sometimes befuddled when choosing the best future investment? Are you so tired of not catching up with the fluctuation of the financial market? Are you frustrated by the unpredictableness of your stocks? Don't Worry! Our app will be a tool that aims to give you an edge to better optimize your investment strategy in the stock market. Here is a brief overview of this tool. 
"
intro_2 <- "We recommend to visit our data summary first. It provides a general sense of the data running behind the scenes, which is the backbone of all the functions there are built into this app."
intro_3 <-  "The rest of our main tabs serve to illustrate a more analytical pitcure of stocks, with detailed instructions and informative terminologies provided along with the graphs. The market distribution tab explains how different sectors play a role in S&P 500 and how each individual sector did in the past compared to the general market. The stock trend tab allows users to learn about general market trends and compare individual stock’s performance with the general market’s. The stock selection tab provides insights on individual stocks based on K-means clustering. The understand your portfolio page enables users to back test your own newly acquired investment strategies. Finally, the portfolio optimization page optimizes the user's portfolio to minimize risk based on past data. 
"
intro_4 <-  "Now, please give it a try and enjoy exploring the stock market!"


# Tab 2 
data_summary_1 <- "Our data source comes from Yahoo finance, a website that provides financial news, data and commentary including stock quotes, press releases, financial reports, and other original contents. Our app mainly uses its records of every single stock/ETF that gets traded on the U.S. stock exchanges from Jan 1, 2000 till the end of Feburary, 2021. 
For each stock, it offers some basic information such as company name, SEDOL code, industrial sector, currency etc. The main variables of interests are the stock's the daily opening, closing, highest and lowest prices, as well as the trading volume everyday."
data_summary_2 <- "Here is a snapshot of what the 500 largest companies on the U.S. stock exchanges (SP500) looks like on a particular day."
data_summary_3 <- "And of course, you have the opportunity to view a specific stock and get some basic ideas about its performance! Please type in stock symbol or abbreviation. If you are not sure about your stock's symbol, feel free to refer to this link that contains the list of all stock abbreviation! The search fucntion can be used to further refine the output by time. For example, if you want to see your stock's price on a particular date, try type in Year-Month-Date, like 2021-01-01."



# Tab 3
market_dis_instruction_1 <- "This tab mainly consists of two functions:" 
market_dis_instruction_2 <- "When “Overall Market” is selected, a graph demonstrating the sector distribution in terms of count and trading volume within the S&P 500 universe will be generated."
market_dis_instruction_3 <- "When “Individual Sector” is selected, the user could select a specific sector and a specific time frame that she is interested in. 
A graph will be generated comparing the sector ETF’s performance to the general market’s (S&P 500 index) within the specified time frame. 
Annualized return and volatility of the sector ETF and S&P 500 index will also be generated as a reference. "
market_dis_instruction_4 <- "The user could use this tab to learn more about how different sectors play a role 
in S&P 500 and how each individual sector did in the past compared to the general market. "


## Main APP Part

# Shiny ui function

ui <- navbarPage("How to Survive in the U.S. Stock Market", theme = shinytheme("superhero"),
                 
                 #Major Tab 1
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
                 
                 
                 #Major Tab 2
                 
                 tags$style(HTML("
                    /* Change font color of the DataTable body and header */
                    .dataTable tbody td {
                      color: white !important;
                    }
                    .dataTable thead th {
                      color: white !important;
                    }
                    /* Change text color for pagination and search bar */
                    .dataTables_wrapper .dataTables_paginate .paginate_button,
                    .dataTables_wrapper .dataTables_info,
                    .dataTables_wrapper .dataTables_length label,
                    .dataTables_wrapper .dataTables_filter label {
                      color: white !important;
                    }
                    .dataTables_wrapper .dataTables_filter input {
                      color: white !important;
                      background-color: black !important; /* Adjust background color of search box if needed */
                    }
                  ")),
                 
                 
                 tabPanel("Data Summary",
                          
                          icon = icon("atlas"),
                          
                          fluidPage(
                            tags$br(br(),
                                    p(data_summary_1),
                                    br(),
                                    p(data_summary_2),
                            ),
                            
                            dataTableOutput("Overall_table"),
                            
                            tags$br(br(),
                                    p(data_summary_3),
                            ),
                            
                            tags$a(href="https://eoddata.com/symbols.aspx?AspxAutoDetectCookieSupport=1", "Stock Reference List"),
                            
                            br(),
                            
                            textInput(inputId = "Summary_Stock_Selected",label = "Your Stock of Interest",value = "AAPL"),
                            
                            
                            dataTableOutput("Summary_stock"),
                            
                          )),
                 
                 
                 # Major Tab 3
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
                                                  tabPanel("Sector Plot", plotOutput("dist_graph"),dataTableOutput("sector_compare")))
                                      
                                      
                            ))
                          
                 ),
                 
                 
                 
                 # Major Tab 4
                 tabPanel("Stock Selection",
                          
                          icon = icon("diagnoses")
                          
                 ),
                 
                 # Major Tab 5
                 tabPanel("Understand Your Portfolio",
                          
                          icon = icon("search-dollar")
                          
                 ),
                 
                 # Major Tab 6
                 tabPanel("Portfolio Optimization",
                          
                          icon = icon("coins")
                          
                 )
                 
)



# Shiny Server Function
server <- function(input, output) {
  
  # Data summary Tab 
  
  output$Overall_table <- renderDataTable(SP500_info %>% rename(Company = Security),
                                            # filter(date == "2021-02-22")%>%
                                            # select(-X) %>%
                                            # rename(
                                            #   Company = Security,
                                            #   Symbol = symbol,
                                            #   Identifier = CIK,
                                            #   Sector=`GICS Sector`
                                            # ), 
                                          options = list(pageLength = 10, lengthChange = FALSE, searching = F))
  
  
  output$Summary_stock <- renderDataTable(tq_get(input$Summary_Stock_Selected, 
                                                 get = 'stock.prices',
                                                 from = Sys.Date()-365*5, to = Sys.Date()) %>% 
                                            mutate(Stock = symbol, Date = date,
                                                   `Open Price`= open, `Close Price`= close, `Highest Price`= high, `Lowest Price`= low, `Volume` = volume) %>% 
                                            select(Stock, Date, `Open Price`, `Close Price`, `Highest Price`, `Lowest Price`, `Volume`) %>% 
                                            arrange(desc(Date)) %>% 
                                            mutate(across(where(is.numeric), ~ round(.x, 3))),
                                          options = list(pageLength = 12, lengthChange = FALSE, sDom  = '<"top">flrt<"bottom">ip'))
  
  
  # Output in market_dist
  
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
        labs(x = '', y = 'Total Daily Trading Volume (in billion)', title = "Volumes of Various Sector Among SP500 Companies", color = "Sector") +
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
  
  output$sector_compare <-renderDataTable(
    
    if(input$sector_indicator == "individual")
    {
      industry_analyze(input$interested_sector,input$start_date,input$end_date)
    },
    options = list(pageLength = 5, lengthChange = FALSE,info = FALSE,dom='t',searching=F,sDom  = '<"top">lrt<"bottom">ip')
    
  )
  
  
}


# Complete app with ui and server components
shinyApp(ui, server)




