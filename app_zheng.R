# Final Project 777

# Load packages
library(tidyverse)
library(ggthemes)
library(plotly) 
library(crosstalk)

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
library(htmlwidgets)
library(DT)
library(rsconnect)



source("helperfunction.R")
source("Text.R")

# Get S&P 500 stock tickers from Wikipedia
sp500_url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
SP500_info <- read_html(sp500_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]]  # The first table contains the S&P 500 list

industries_ETF <- c("XLK","XLY","XLC","XLF","XLV","XLP","XLE","XLI","XLU","XLB", "XLRE")
sector_data <- tq_get(industries_ETF,from = "2010-01-01",to = Sys.Date(), get = 'stock.prices') 



## Take a while to pull, so we saved it in advance
# write.csv(tq_get(SP500_info$Symbol, get = "stock.prices", from = Sys.Date()-1, to = Sys.Date()), 
#           file = "S&P500_all.csv")

SP500_all <- read.csv("S&P500_all.csv")

# Join the data with info and daily price
SP500_all <- left_join(SP500_all, SP500_info, by = join_by(symbol == Symbol))




## Main APP Part

# Shiny ui function

ui <- navbarPage("How to Survive in the U.S. Stock Market", theme = shinytheme("superhero"),
                 
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
                            
                            textInput(inputId = "Summary_Stock_Selected",label = "Your Stock of Interest",value = "SHAK"),
                            
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
                                            tabPanel("Stock Trend", plotOutput("stock_trend_plot"), 
                                                     tags$br(br(),
                                                             p("Daily Stock Datatable"),
                                                     ),
                                                     DT::DTOutput("summary_stock_individual") 
                                                     ))
                              )
                            )
                          )
                          
                 ),
                 
                 
                 
                 # Tab Market Distribution
                 tabPanel("Market Distribution",
                          
                          icon = icon("chart-bar"),
                          
                          fluidPage(
                            
                            sidebarPanel( 
                              radioButtons("graph_type", "Select Your Preferred Visualization",
                                c("Price of Each Sector" = "Price",
                                "Volume of Each Sector" = "Volume")),
                    # Time Frame 
                                sliderInput("Trend_Time_market",
                                            "Select Your Interested Time Frame",
                                            value = c(2015,2023),
                                            min = 2014,
                                            max = 2024, 
                                            sep = "") 
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
                                                           br(),
                                                           p(market_dis_instruction_5),
                                                           br(),
                                                           fluidRow(
                                                            column(3, uiOutput("market_dis_instruction_6")),  
                                                            column(9, plotOutput("dist_graph")))
     
                                                  ),
                                                  tabPanel("Sector Plot", 
                                                           plotlyOutput("sector_plot"),
                                                           DT::DTOutput("filtered_table")
                                                           #DT::DTOutput("sector_compare")
                                                           ))
     
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
server <- function(input, output, session) {

sector_data_prepped <- reactive({
    sector_data %>%
    select(symbol, date, adjusted, volume) %>%
    rename(Sector = symbol, Date = date, Price = adjusted, Volume = volume) %>%
    filter(
        Date >= as.Date(paste0(input$Trend_Time_market[1], "-01-01")) & 
        Date <= as.Date(paste0(input$Trend_Time_market[2], "-01-01"))
    ) 
  })
  
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
  
  
  output$summary_stock_individual <- DT::renderDT(expr = tq_get(c(input$Select_Stock_01,input$Select_Stock_02), 
                                                     get = 'stock.prices',
                                                     ,from = '2000-01-01',to = Sys.Date()) %>% 
                                                     mutate(year=year(date), month = month(date))%>%
                                                     filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])%>%
                                                     mutate(Stock = symbol, Date = date, Volume = volume, Price = adjusted) %>% 
                                                     select(Stock, Date, Price, Volume) %>% 
                                                     arrange(desc(Date)) %>% 
                                                     mutate(across(where(is.numeric), ~ round(.x, 3))),
                                       options = list(pageLength = 12, lengthChange = FALSE, sDom  = '<"top">flrt<"bottom">ip'))
  
  
  output$stock_trend_plot <- renderPlot(
    if (input$market_indicator == "individual" & input$stock_stats == "open")
    {tq_get(c(input$Select_Stock_01,input$Select_Stock_02),from = '2000-01-01',to = Sys.Date(), get = 'stock.prices') %>%
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
  
  
  
  
  output$dist_graph <- renderPlot(
      SP500_info %>%
        mutate(sector = `GICS Sector`) %>%
        group_by(sector) %>%
        mutate(count = n()) %>%
        select(sector, count) %>%
        distinct() %>%
        ggplot(aes(x = reorder(sector, -count), y = count, fill = sector)) +
        geom_bar(stat = "identity", show.legend = FALSE, width = 0.8) +  # Adjust bar width and hide legend
        geom_text(aes(label = count), vjust = -0.5, size = 4, fontface = "bold", color = "black") +  # Add count labels on bars
        scale_fill_brewer(palette = "Set3") +  # Use a more readable color palette
        theme_minimal() +  # A cleaner background
        labs(
          x = '', 
          y = 'Counts Among S&P500', 
          title = "Distribution of Various Sectors Among S&P500 Companies"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.text.x = element_text(face = "bold", size = 10, angle = -45, hjust = 1),  # Improve text angle
          axis.title = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.margin = margin(10, 20, 10, 20)  # Adjust margins
        )
  )
  output$market_dis_instruction_6 <- renderUI({
    HTML(market_dis_instruction_6)
  })

  output$sector_plot <- renderPlotly({
    
    shared_sector_data <- SharedData$new(sector_data_prepped(), key = ~Sector)
    if(input$graph_type == "Price"){
        plot_ly(
        shared_sector_data,
        x = ~Date,
        y = ~Price,
        color = ~Sector,
        colors = "RdYlBu", 
        type = "scatter",
        mode = "lines",  # Adding markers for better hover interaction
        hoverinfo = "text",  # Show custom hover text
        text = ~paste("Sector: ", Sector, "<br>Date: ", Date, "<br>Price: $", round(Price, 2)),  # Customize the hover text
        source = "market_trend_int"
        ) %>%
        layout(
          title = "ETF Sector Price Trends",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Price"),
          legend = list(
            itemclick = "toggleothers",   # Default click: Toggle others while isolating the clicked line
            itemdoubleclick = "toggleall" # Double-click: Show all lines
          )
        ) %>%
        config(
          displaylogo = FALSE                        # Hide Plotly logo
        )
    }else{
      plot_ly(
        shared_sector_data,
        x = ~Date,
        y = ~Volume,
        color = ~Sector,
        colors = "RdYlBu", 
        type = "scatter",
        mode = "lines",  # Adding markers for better hover interaction
        hoverinfo = "text",  # Show custom hover text
        text = ~paste("Sector: ", Sector, "<br>Date: ", Date, "<br>Volume: ", round(Volume, 2)),  # Customize the hover text
        source = "market_trend_int"
        ) %>%
        layout(
          title = "ETF Sector Price Trends",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Volume"),
          legend = list(
            itemclick = "toggleothers",   # Default click: Toggle others while isolating the clicked line
            itemdoubleclick = "toggleall" # Double-click: Show all lines
          )
        ) %>%
        config(
          displaylogo = FALSE                        # Hide Plotly logo
        )
    }
  })

  # Reactive object to store the hover data
  hover_data <- reactiveVal(NULL)
  
  # Observe hover event and store the hovered sector
  observeEvent({
    event_data(event = "plotly_hover",
               source = "market_trend_int",
               session = shiny::getDefaultReactiveDomain())
  }, {
    hover_info <- event_data(event = "plotly_hover",
               source = "market_trend_int",
               session = shiny::getDefaultReactiveDomain())
    if (!is.null(hover_info) && length(hover_info$key) > 0) {
      # Extract sector from hover data
      sector_hovered <- list(key = unlist(hover_info$key), date = unlist(hover_info$x))
      hover_data(sector_hovered)
    }
  })
  
  # Render the filtered table based on hover data
  output$filtered_table <- DT::renderDT({
    if(input$graph_type == "Price"){
        filtered_data <- sector_data_prepped() %>% mutate(Price = round(Price, 2), Previous = lag(Price), Change = round((Price - Previous), 2), `Change(%)` = round(100*((Price - Previous)/Previous), 3)) %>% 
        dplyr::select(Sector, Date, Price, Change, `Change(%)`)
        # Filter data based on hovered sector
        if (length(hover_data()$key) > 0) {
          filtered_data <- filtered_data %>% filter(Sector %in% hover_data()$key, Date %in% as.Date(hover_data()$date)) 
        }
    }else{
        filtered_data <- sector_data_prepped() %>% mutate(Volume = round(Volume, 2), Previous = lag(Volume), Change = round((Volume - Previous), 2), `Change(%)` = round(100*((Volume - Previous)/Previous), 3)) %>% 
        dplyr::select(Sector, Date, Volume, Change, `Change(%)`)
        # Filter data based on hovered sector
        if (length(hover_data()$key) > 0) {
          filtered_data <- filtered_data %>% filter(Sector %in% hover_data()$key, Date %in% as.Date(hover_data()$date))
        }
    }
    
    DT::datatable(filtered_data, options = list(pageLength = 11)) 
  })
  
  output$sector_compare <- DT::renderDT(expr =
    
    if(input$sector_indicator == "individual")
    {
      industry_analyze(input$interested_sector,input$start_date,input$end_date)
    },
    options = list(pageLength = 5, lengthChange = FALSE,info = FALSE,dom='t',searching=F,sDom  = '<"top">lrt<"bottom">ip')
    
  )
  
  
}


# Complete app with ui and server components
shinyApp(ui, server)




