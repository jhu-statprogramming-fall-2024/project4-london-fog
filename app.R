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

# Get Nasdaq-100 from Wikipedia
Nasdaq100_url <- "https://en.wikipedia.org/wiki/Nasdaq-100#Components"
Nasdaq_info <- read_html(Nasdaq100_url) %>%
  html_table(fill = TRUE) %>%
  .[[5]]  # The fifth table

dow_info <- tq_index("DOW-GLOBAL") %>% dplyr::select(-c(sector, weight)) 


industries_ETF <- c("XLK","XLY","XLC","XLF","XLV","XLP","XLE","XLI","XLU","XLB", "XLRE")
sector_data <- tq_get(industries_ETF,from = "2010-01-01",to = Sys.Date(), get = 'stock.prices') 

# Take a while to pull, so we saved it in advance
# write.csv(tq_get(SP500_info$Symbol, get = "stock.prices", from = Sys.Date()-1, to = Sys.Date()), 
#           file = "S&P500_all.csv")

SP500_all <- read.csv("S&P500_all.csv")

# Join the data with info and daily price
SP500_all <- left_join(SP500_all, SP500_info, by = join_by(symbol == Symbol))

# Read in all stock symbols and descriptions
stock_symbols <- readRDS("allstock.rds") %>% mutate(country = as.factor(country))

## Main APP Part

# Shiny ui function

ui <- navbarPage("How to Survive in the U.S. Stock Market", theme = shinytheme("sandstone"),
                 
                 # tags$style(HTML("
                 #    /* Change font color of the DataTable body and header */
                 #    .dataTable tbody td {
                 #      color: white !important;
                 #    }
                 #    .dataTable thead th {
                 #      color: white !important;
                 #    }
                 #    /* Change text color for pagination and search bar */
                 #    .dataTables_wrapper .dataTables_paginate .paginate_button,
                 #    .dataTables_wrapper .dataTables_info,
                 #    .dataTables_wrapper .dataTables_length label,
                 #    .dataTables_wrapper .dataTables_filter label {
                 #      color: white !important;
                 #    }
                 #    .dataTables_wrapper .dataTables_filter input {
                 #      color: white !important;
                 #      background-color: black !important; /* Adjust background color of search box if needed */
                 #    }
                 #  ")),
                 # 
                 
                 # Tab Intro
                 tabPanel("Introduction of this App",
                          icon = icon("compass"),
                          themeSelector(),
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
                            
                            selectInput("ETF_choice", "Select ETF", choices = c("SPY-500", "Nasdaq-100", "DOW-GLOBAL")),
                            
                            uiOutput("overall_table_ui"),
                            
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
                                uiOutput("stock_trends_stock_sel_ui"), 
                                
                                br(),
                                
                                uiOutput("third_option"),
                                
                                br(),
                                
                                # Time Frame 
                                sliderInput("Trend_Time",
                                            "Select Your Interested Time Frame",
                                            value = c(2014,2024),
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
                                            tabPanel("Stock Trend", 
                                                     uiOutput("stock_trend_res_ui")
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
                                                           DT::DTOutput("filtered_table")))
                                      
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
                                           br())),
                                         
                                         # DT::DTOutput("info")),
                            
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
                                                  tabPanel("S&P 500 Cluster", 
                                                           plotOutput("Cluster",click = "my_click"),
                                                           br(), 
                                                           DT::DTOutput("cluster_info")
                                                           )
                            ))
                            
                          )
                          
                 ),
                 
                 # Tab Understand Your Portfolio
                 tabPanel(
                   "Understand Your Portfolio",
                   
                   icon = icon("search-dollar"), 
                   
                   fluidPage(
                     sidebarPanel(
                       titlePanel("Choose Your Own Portfolio"),
                       tags$br(
                         br(),
                         p(portfolio_pick_stocks),
                       ),
                       fluidRow(
                         column(
                           8, 
                           textInput(
                             inputId = "port_stocks_txt",
                             label = NULL,
                             value = "AAPL,MSFT,TSLA"
                           )
                         ), 
                         column(
                           4, 
                           actionButton(
                             "port_stocks_selector", 
                             "Stock selector"
                           )
                         )
                       ),
                       htmlOutput("port_sel_stocks_text"), 
                       tags$br(
                         br(),
                         p(portfolio_stock_weights),
                       ),
                       uiOutput("port_weights_ui"), 
                       actionButton(
                         "eval_port", 
                         "Evaluate your portfolio"
                       ), 
                       span(textOutput("port_eval_message"), style="color:red")
                     ),
                     
                     mainPanel(
                       tabsetPanel(
                         type="tabs", 
                         tabPanel(
                           "Instruction",
                           br(),
                           p(portfolio_instruction),
                           br(),
                           br(),
                           div(img(src='Trading_Graph_Chart.png',width="60%"), 
                               style="text-align: center;"),
                           br()
                         ),
                         tabPanel(
                           "Summary of Your Portfolio", 
                           br(), 
                           h3("Portfolio Composition"),
                           dataTableOutput("port_info"), 
                           br(),
                           h3("Portfolio Summary"),
                           dataTableOutput("port_summary")
                         ), 
                         tabPanel(
                           "Your Chosen Portfolio Against S&P500", 
                           br(),
                           h3("Comparison between Your Portfolio and S&P 500 (SPY)"),
                           plotlyOutput("port_spy_plot")
                         ), 
                         tabPanel(
                           "Prediction of Portfolio Performance", 
                           br(), 
                           p(portfolio_prediction_intro), 
                           br(), 
                           uiOutput("port_pred_ui")
                         )
                       ),
                     )
                   )
                 ),
                 
                 # # Tab Portfolio Optimization
                 # tabPanel("Portfolio Optimization",
                 #          
                 #          icon = icon("coins")
                 #          
                 # )
                 
)



# Shiny Server Function
server <- function(input, output, session) {
  
  ### Stock selector: outputs user-selected stock(s) symbols to textbox
  
  # Initialize parameters
  stock_sel_param <- reactiveVal(list(max_stocks = 1)) # max number of stocks that can be selected
  
  # Output textbox
  stock_sel_textbox <- reactiveVal(NULL)
  
  # Stock symbols table
  output$stock_symbols_table <- DT::renderDataTable(stock_symbols, 
                                                    rownames = FALSE, 
                                                    filter = list(position = 'top', clear = FALSE), 
                                                    selection = "multiple",
                                                    escape = FALSE)
  
  # Selector modal dialogue
  observeEvent(stock_sel_textbox(), {
    showModal(
      modalDialog(
        h3("Select your stocks"),
        br(), 
        p("Click on table to select stocks. "),
        textOutput("stock_sel_text"),
        br(),
        fluidRow(
          column(
            3, 
            actionButton(
              "stock_symbols_table_sel_none", 
              "Deselect all", 
              width = "100%"
            )
          )
        ), 
        br(), 
        dataTableOutput("stock_symbols_table"),
        span(textOutput("stock_sel_warning"), style="color:red"),
        br(),
        actionButton("update_stock_sel", "Confirm selection"),
        actionButton("cancel_stock_sel", "Cancel"),
        easyClose = FALSE, 
        size = "l",
        footer = NULL
      )
    )
  })
  
  # Current selection text
  output$stock_sel_text <- renderText({
    if (length(input$stock_symbols_table_rows_selected)) {
      paste("You have selected", 
            paste(stock_symbols$symbol[input$stock_symbols_table_rows_selected], 
                  collapse = ", "))
    } else {
      "You have not selected any stocks. "
    }
  })
  
  # Make proxy for controlling selected rows
  stock_symbols_table_proxy <- DT::dataTableProxy("stock_symbols_table")
  
  # Deselect all of gtex trait table
  observeEvent(input$stock_symbols_table_sel_none, {
    stock_symbols_table_proxy %>% selectRows(NULL)
  })
  
  # Warn users that they must select at least 1 stock
  output$stock_sel_warning <- renderText({
    if (length(input$stock_symbols_table_rows_selected) < 1) {
      return("You must select at least 1 stock. ")
    } 
  })
  
  # Restrict max number of stock selections
  observeEvent(input$stock_symbols_table_rows_selected, {
    if (length(input$stock_symbols_table_rows_selected) > stock_sel_param()$max_stocks) {
      stock_symbols_table_proxy %>% selectRows(input$stock_symbols_table_rows_selected[1:stock_sel_param()$max_stocks])
    }
  })
  
  # Update stock selection
  observeEvent(input$update_stock_sel, {
    # Check that at least 2 stocks selected
    if ((length(input$stock_symbols_table_rows_selected) >= 1) & 
        (length(input$stock_symbols_table_rows_selected) <= stock_sel_param()$max_stocks)) {
      updateTextInput(session, 
                      stock_sel_textbox(), 
                      value = paste(stock_symbols$symbol[input$stock_symbols_table_rows_selected], 
                                    collapse = ","))
      stock_sel_textbox(NULL)
      removeModal()
    }
  })
  
  # Cancel stock selection
  observeEvent(input$cancel_stock_sel, {
    stock_sel_textbox(NULL)
    removeModal()
  })
  
  # This function is for parsing user-input stock selection text
  parse_stock_text <- function(text, max_stocks) {
    stocks <- strsplit(text, ",")[[1]] %>%  str_trim() %>% toupper() %>% unique()
    stocks <- stocks[stocks != ""]
    valid_stocks <- stocks[stocks %in% stock_symbols$symbol]
    if (length(valid_stocks)) {
      valid_stocks <- valid_stocks[1:min(length(valid_stocks), max_stocks)]
    }
    invalid_stocks <- setdiff(stocks, valid_stocks)
    list(valid = valid_stocks, invalid = invalid_stocks)
  }
  
  sector_data_prepped <- reactive({
    sector_data %>%
      dplyr::select(symbol, date, adjusted, volume) %>%
      rename(Sector = symbol, Date = date, Price = adjusted, Volume = volume) %>%
      filter(
        Date >= as.Date(paste0(input$Trend_Time_market[1], "-01-01")) & 
          Date <= as.Date(paste0(input$Trend_Time_market[2], "-01-01"))
      ) 
  })
  
  # Data summary Tab 
  output$overall_table_ui <- renderUI({
    if (input$ETF_choice == "SPY-500") {
      dataTableOutput("sp500_table")
    } else if (input$ETF_choice == "DOW-GLOBAL") {
      dataTableOutput("dow_table")   
    }
    else {
      dataTableOutput("nasdaq_table")
    }
  })
  
  output$sp500_table <- DT::renderDT(expr = SP500_info,
                                     options = list(pageLength = 10, lengthChange = FALSE, searching = F))
  
  output$nasdaq_table <- DT::renderDT(expr = Nasdaq_info,
                                      options = list(pageLength = 10, lengthChange = FALSE, searching = F))
  
  output$dow_table <- DT::renderDT(expr = dow_info,
                                      options = list(pageLength = 10, lengthChange = FALSE, searching = F))
  
  
  output$Summary_stock <- DT::renderDT(expr = tq_get(input$Summary_Stock_Selected, 
                                                     get = 'stock.prices',
                                                     from = Sys.Date()-365*10, to = Sys.Date()) %>% 
                                         mutate(Stock = symbol, Date = date,
                                                `Open Price`= open, `Close Price`= close, `Highest Price`= high, `Lowest Price`= low, `Volume` = volume) %>% 
                                         dplyr::select(Stock, Date, `Open Price`, `Close Price`, `Highest Price`, `Lowest Price`, `Volume`) %>% 
                                         arrange(desc(Date)) %>% 
                                         mutate(across(where(is.numeric), ~ round(.x, 3))),
                                       options = list(pageLength = 12, lengthChange = FALSE, sDom  = '<"top">flrt<"bottom">ip'))
  
  # Stock Trend Tab
  
  output$stock_trends_stock_sel_ui <- renderUI(
    if (input$market_indicator == "individual") {
      tagList(
        p("Please input up to 5 stock symbols, separated by commas (e.x AAPL,TSLA)"),
        fluidRow(
          column(
            8, 
            textInput(inputId = "stock_trend_stock_sel_txt", 
                      label = NULL, 
                      value = "AAPL,TSLA")
          ), 
          column(
            4, 
            actionButton("stock_trend_stock_selector", 
                         label = "Stock selector")
          )
        ), 
        htmlOutput("stock_trend_sel_stocks_text")
      )
    })
  
  stock_trend_max_stocks <- 5
  
  # Use stock selector to select stocks
  observeEvent(input$stock_trend_stock_selector, {
    stock_sel_param(list(max_stocks = stock_trend_max_stocks))
    stock_sel_textbox("stock_trend_stock_sel_txt")
  })
  
  # Stock text parse output
  stock_trend_stock <- reactive({
    parse_stock_text(input$stock_trend_stock_sel_txt, stock_trend_max_stocks)
  })
  
  # Show currently selected stocks from text input
  output$stock_trend_sel_stocks_text <- renderText({
    selected_text <- paste("You have selected the following stocks:", 
                           paste(stock_trend_stock()$valid, 
                                 collapse = ", "))
    if (length(stock_trend_stock()$valid) == 0) {
      selected_text <- "You have not selected any stocks. "
    }
    invalid_text <- paste("The following selection is invalid:", 
                          paste(stock_trend_stock()$invalid, collapse = ", "))
    if (length(stock_trend_stock()$invalid) == 0) {
      invalid_text <- NULL
    }
    paste(c(selected_text, invalid_text), collapse = "<br/>")
  })
  
  output$third_option <- renderUI(
    if (input$market_indicator == "individual")
    {radioButtons("stock_stats", "Select specific stock market information",
                  c("Daily Investment Worth" = "price",
                    # "Daily Investment Change in Percentage" = "percent",
                    "Daily Investment Worth Comparing with S&P 500" = "Compare",
                    "Daily Transaction Volume" = "volume"))}
    else{})
  
  output$stock_trend_res_ui <- renderUI({
    # No output graph if user selected 0 stocks
    if (input$market_indicator == "individual" && length(stock_trend_stock()$valid) == 0) {
      return(
        tagList(
          br(), 
          p("You have not selected any stocks. Please select 1-5 stocks to view trends. ")
        )
      )
    }
    tagList(
      br(), 
      h3("Stock trend plot"),
      plotlyOutput("stock_trend_plot"), 
      br(), 
      h3("Daily Stock Datatable"),
      DT::DTOutput("summary_stock_individual")
    )
  })
  
  output$summary_stock_individual <- DT::renderDT(
    if (input$market_indicator == "individual"& length(stock_trend_stock()$valid)) {
      expr = tq_get(stock_trend_stock()$valid, 
                    get = 'stock.prices', from = '2000-01-01',to = Sys.Date()) %>% 
        mutate(year=year(date), month = month(date))%>%
        filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])%>%
        mutate(Stock = symbol, Date = date, Volume = volume, Price = adjusted) %>% 
        dplyr::select(Stock, Date, Price, Volume) %>% 
        arrange(desc(Date)) %>% 
        mutate(across(where(is.numeric), ~ round(.x, 3)))
    } else if (input$market_indicator == "Market") {
      expr = tq_get(c("SPY","QQQ", "VTI"), 
                    get = 'stock.prices', from = '2000-01-01',to = Sys.Date()) %>% 
        mutate(year=year(date), month = month(date))%>%
        filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])%>%
        mutate(Stock = symbol, Date = date, Volume = volume, Price = adjusted) %>% 
        dplyr::select(Stock, Date, Price, Volume) %>% 
        arrange(desc(Date)) %>% 
        mutate(across(where(is.numeric), ~ round(.x, 3)))
    }, options = list(pageLength = 12, lengthChange = FALSE, sDom  = '<"top">flrt<"bottom">ip')
  )
  
  
  output$stock_trend_plot <- renderPlotly(
    if (input$market_indicator == "individual" & length(stock_trend_stock()$valid)) {
      if (input$stock_stats == "price")
        # {tq_get(c(input$Select_Stock_01,input$Select_Stock_02),from = '2000-01-01',to = Sys.Date(), get = 'stock.prices') %>%
        #     mutate(year=year(date), month = month(date))%>%
        #     filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])%>%
        #     ggplot(aes(x=date,y=close,color=symbol)) + geom_line()+
        #     labs(x="", y="Single Share Price", color="Stock", title = "Price of a single share of stock")+
        #     geom_hline(yintercept= input$target_value,color="black",size = 0.5,alpha=0.5) +
        #     theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()}
      {stock_data = tq_get(stock_trend_stock()$valid,from = '2000-01-01',to = Sys.Date(), get = 'stock.prices') %>%
        mutate(year=year(date), month = month(date))%>%
        filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])
      stock_data %>% plot_ly(
        x = ~date, y = ~close, color = ~symbol, colors = "Set2", type = 'scatter',mode = 'lines', 
        text = ~paste('Date:', date, '<br>Close Price:', round(close,3), 'USD', '<br>Stock:', symbol),
        hoverinfo = 'text'
      ) %>%
        layout(
          title = "Price of a Single Share of Stock", xaxis = list(title = ""), yaxis = list(title = "Single Share Price"),
          shapes = list(
            type = "line",
            x0 = min(stock_data$date), x1 = max(stock_data$date),
            y0 = input$target_value, y1 = input$target_value,
            line = list(color = "black", dash = "dash")
          ),
          legend = list(title = list(text = "Stock"))
        ) %>% config(displaylogo = FALSE)}
      
      
      else if (input$stock_stats == "volume")
        # {tq_get(c(input$Select_Stock_01,input$Select_Stock_02),from = '2000-01-01',to = Sys.Date(), get = 'stock.prices') %>%
        #     mutate(year=year(date), month = month(date))%>%
        #     filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])%>%
        #     ggplot(aes(x=date,y=volume/1000000,color=symbol)) + geom_line()+
        #     labs(x="", y="Daily Transaction (in millions)",color="Stock", title = "Daily transaction volume of stock")+
        #     geom_hline(yintercept= input$target_value,color="black",size = 0.5,alpha=0.5) +
        #     theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()}
      {stock_data = tq_get(stock_trend_stock()$valid,from = '2000-01-01',to = Sys.Date(), get = 'stock.prices') %>%
        mutate(year=year(date), month = month(date), volume_adj = volume/1000000)%>%
        filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])
      stock_data %>% plot_ly(
        x = ~date, y = ~volume_adj, color = ~symbol, colors = "Set2", type = 'scatter',mode = 'lines',
        text = ~paste('Date:', date, '<br>Daily Transcation:', round(volume_adj,3), 'millions', '<br>Stock:', symbol),
        hoverinfo = 'text'
      ) %>%
        layout(
          title = "Daily transaction volume of stock", xaxis = list(title = ""), yaxis = list(title = "Daily Transaction (in millions)"),
          shapes = list(
            type = "line",
            x0 = min(stock_data$date), x1 = max(stock_data$date),
            y0 = input$target_value, y1 = input$target_value,
            line = list(color = "black", dash = "dash")
          ),
          legend = list(title = list(text = "Stock"))
        ) %>% config(displaylogo = FALSE)}
      
      
      else if (input$stock_stats == "percent")
      {stock_data = tq_get(stock_trend_stock()$valid[1],from = '2000-01-01',to = Sys.Date(), get = 'stock.prices') %>%
        mutate(year=year(date), month = month(date), volume_adj = volume/1000000)%>%
        mutate(perc_change = abs(diff(close)/lag(close)*100)) %>% 
        filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2], date < Sys.Date()-7) 
      stock_data %>% plot_ly(
        x = ~date, y = ~perc_change, color = ~symbol, colors = "Set2", type = 'scatter',mode = 'lines',
        text = ~paste('Date:', date, '<br>Percentage Change:', round(perc_change,3), '%', '<br>Stock:', symbol),
        hoverinfo = 'text'
      ) %>%
        layout(
          title = "Daily change of stock in percentage", xaxis = list(title = ""), yaxis = list(title = "Change in percentage"),
          shapes = list(
            type = "line",
            x0 = min(stock_data$date), x1 = max(stock_data$date),
            y0 = input$target_value, y1 = input$target_value,
            line = list(color = "black", dash = "dash")
          ),
          legend = list(title = list(text = "Stock"))
        ) %>% config(displaylogo = FALSE)}
      
      else if (input$stock_stats == "Compare")
        # {tq_get(c(input$Select_Stock_01,input$Select_Stock_02,"SPY"),from = '2000-01-01',to = Sys.Date(),get = 'stock.prices') %>%
        #     mutate(year=year(date), month = month(date))%>%
        #     filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])%>%
        #     ggplot(aes(x=date,y=close,color=symbol)) + geom_line()+
        #     labs(x="", y="Single Share Price",color="Stock", title = "Price of a single share")+
        #     geom_hline(yintercept= input$target_value,color="black",size = 0.5,alpha=0.5) +
        #     theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()}
      {stock_data = tq_get(c(stock_trend_stock()$valid,"SPY"),from = '2000-01-01',to = Sys.Date(), get = 'stock.prices') %>%
        mutate(year=year(date), month = month(date))%>%
        filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])
      stock_data %>% plot_ly(
        x = ~date, y = ~close, color = ~symbol, colors = "Set2", type = 'scatter',mode = 'lines',
        text = ~paste('Date:', date, '<br>Close Price:', round(close,3), 'USD', '<br>Stock:', symbol),
        hoverinfo = 'text'
      ) %>%
        layout(
          title = "Price of a Single Share of Stock", xaxis = list(title = ""), yaxis = list(title = "Single Share Price"),
          shapes = list(
            type = "line",
            x0 = min(stock_data$date), x1 = max(stock_data$date),
            y0 = input$target_value, y1 = input$target_value,
            line = list(color = "black", dash = "dash")
          ),
          legend = list(title = list(text = "Stock"))
        ) %>% config(displaylogo = FALSE)}
    }
    
    # Market subplot
    else if (input$market_indicator == "Market")
      # {tq_get(c("SPY", "QQQ", "VTI"),from = '2000-01-01',to = Sys.Date(), get = 'stock.prices') %>%
      #     mutate(year=year(date), month = month(date))%>%
      #     filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])%>%
      #     ggplot(aes(x=date,y=close,color=symbol)) + geom_line()+
      #     labs(x="", y="Single Share Price", color="Stock", title = "Price of a single share of stock")+
      #     theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()}
    {stock_data = tq_get(c("SPY", "QQQ", "VTI"),from = '2000-01-01',to = Sys.Date(), get = 'stock.prices') %>%
      mutate(year=year(date), month = month(date))%>%
      filter(year >= input$Trend_Time[1] & year <= input$Trend_Time[2])
    stock_data %>% plot_ly(
      x = ~date, y = ~close,
      color = ~symbol, colors = "Set2", type = 'scatter',mode = 'lines',
      text = ~paste('Date:', date, '<br>Close Price:', round(close,3), 'USD', '<br>Stock:', symbol),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Price of a Single Share of Stock", xaxis = list(title = ""), yaxis = list(title = "Single Share Price"),
        shapes = list(
          type = "line",
          x0 = min(stock_data$date), x1 = max(stock_data$date),
          y0 = input$target_value, y1 = input$target_value,
          line = list(color = "black", dash = "dash")
        ),
        legend = list(title = list(text = "Stock"))
      ) %>% config(displaylogo = FALSE)}
    
    
    
    
  )
  
  
  
  # Market Distribution Tab
  output$dist_graph <- renderPlot(
    SP500_info %>%
      mutate(sector = `GICS Sector`) %>%
      group_by(sector) %>%
      mutate(count = n()) %>%
      dplyr::select(sector, count) %>%
      distinct() %>%
      ggplot(aes(x = reorder(sector, -count), y = count, fill = sector)) +
      geom_bar(stat = "identity", show.legend = FALSE, width = 0.8) +  # Adjust bar width and hide legend
      geom_text(aes(label = count), vjust = -0.3, size = 4, fontface = "bold", color = "black") +  # Add count labels on bars
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
  
  # Selection Tab
  
  output$Cluster <- renderPlot({generate_graph_cluster(input$Stock_Selected)})
  
  
  output$info <- renderDataTable(nearPoints(SP500_all_single, input$my_click,threshold = 10) %>%
                                   select(-X) %>%
                                   rename(
                                     Stock_cluster = cluster_km5
                                   ), 
                                 options = list(pageLength = 5, lengthChange = FALSE,info = FALSE,dom='t',searching=F,sDom  = '<"top">lrt<"bottom">ip'))
  
  Cluster <- c(1,2,3,4,5)
  
  Color <- c("Red","Dark Green","Green","Blue","Purple")
  
  Return <- c("High","Low","High","Medium","Low")
  
  Volatility <- c("High","High","Low","Medium","Low")
  
  
  cluster_info <- data.frame(Cluster, Color,Return, Volatility)
  
  
  output$cluster_info <- renderDataTable(cluster_info, 
                                         options = list(lengthChange = FALSE,info = FALSE,dom='t',searching=F,sDom  = '<"top">lrt<"bottom">ip'))
  
  
  
  ### Understand your portfolio tab
  
  # Portfolio evaluation message
  output$port_eval_message <- renderText({port_eval_message()})
  
  # Portfolio evaluation message
  port_eval_message <- reactiveVal(NULL)
  
  # Default stocks
  port_stocks <- reactiveVal(c("AAPL", "MSFT", "TSLA"))
  
  # Default weights
  port_weights <- reactiveVal(c(0.3, 0.3, 0.4))
  
  # Max number of stocks to select
  port_max_stocks <- 5
  
  # Use stock selector to select portfolio stocks
  observeEvent(input$port_stocks_selector, {
    stock_sel_param(list(max_stocks = port_max_stocks))
    stock_sel_textbox("port_stocks_txt")
  })
  
  # Stock text parse output
  port_stocks_temp <- reactive({
    parse_stock_text(input$port_stocks_txt, port_max_stocks)
  })
  
  # Show currently selected stocks from text input
  output$port_sel_stocks_text <- renderText({
    selected_text <- paste("You have selected the following stocks:", 
                           paste(port_stocks_temp()$valid, 
                                 collapse = ", "))
    if (length(port_stocks_temp()$valid) == 0) {
      selected_text <- "You have not selected any stocks. "
    }
    invalid_text <- paste("The following selection is invalid:", 
                          paste(port_stocks_temp()$invalid, collapse = ", "))
    if (length(port_stocks_temp()$invalid) == 0) {
      invalid_text <- NULL
    }
    paste(c(selected_text, invalid_text), collapse = "<br/>")
  })
  
  # UI for stock weights selection
  output$port_weights_ui <- renderUI({
    sel_stocks <- port_stocks_temp()$valid
    if (length(sel_stocks) == 0) {
      return(NULL)
    }
    default_weights <- assign_weights(length(sel_stocks))
    tagList(
      lapply(1:length(sel_stocks), function(i) {
        sliderInput(
          paste0("port_", sel_stocks[i], "_weight"), 
          label = paste("Select weight for", sel_stocks[i]), 
          min = 0, max = 1, step = 0.05,
          value = default_weights[i]
        )
      })
    )
  })
  
  # Last weight state
  port_weight_state <- reactiveVal(NULL)
  
  # Last weight slider that was changed
  port_weight_updated <- reactiveVal(NULL)
  
  # Whether change in slider values happened because of user action
  port_user_updated_weight <- reactiveVal(TRUE)
  
  # Update last weight slider changed record
  observe({
    sel_stocks <- port_stocks_temp()$valid
    weight_id <- paste0("port_", sel_stocks, "_weight")
    for (id in weight_id) {
      req(input[[id]])
    }
    curr_state <- sapply(weight_id, function(id) {
      input[[id]]
    })
    names(curr_state) <- sel_stocks
    isolate({
      if ((length(sel_stocks) != length(port_weight_state())) || 
          any(names(port_weight_state()) != sel_stocks)) {
        # Selected stocks have changed
        port_weight_state(curr_state)
        port_weight_updated(NULL)
      } else if (any(port_weight_state() != curr_state)) {
        if (port_user_updated_weight()) {
          # User changed slider values - check which slider changed
          port_weight_updated(sel_stocks[which(port_weight_state() != curr_state)[1]])
          port_weight_state(curr_state)
        } else {
          port_user_updated_weight(TRUE)
          port_weight_state(curr_state)
          port_weight_updated(NULL)
        }
      }
    })
  })
  
  # Update slider values when weights change
  observeEvent(port_weight_updated(), {
    sel_stocks <- port_stocks_temp()$valid
    weight_id <- paste0("port_", sel_stocks, "_weight")
    # Check special case of only 1 selected stock
    if ((length(sel_stocks) == 1) && (input[[weight_id]] != 1)) {
      port_user_updated_weight(FALSE)
      updateSliderInput(session = session,
                        inputId = weight_id,
                        value = 1)
    }
    for (id in weight_id) {
      req(input[[id]])
    }
    weights <- sapply(weight_id, function(id) {input[[id]]})
    tot_weight <- sum(weights)
    updated_weight <- weights[sel_stocks == port_weight_updated()]
    adjustable_weights <- weights[sel_stocks != port_weight_updated()]
    adjustable_stocks <- sel_stocks[sel_stocks != port_weight_updated()]
    adjustable_weight_id <- weight_id[sel_stocks != port_weight_updated()]
    cum_weights <- cumsum(adjustable_weights)
    if (tot_weight > 1) {
      port_user_updated_weight(FALSE)
      idx_change <- min(which(cum_weights > (1 - updated_weight)))
      remaining_weight <- 1 - updated_weight - sum(adjustable_weights[- c(idx_change:length(adjustable_weights))])
      new_weights <- assign_weights(length(adjustable_stocks) - idx_change + 1, 
                                    tot_weight = remaining_weight)
      for (i in idx_change:length(adjustable_stocks)) {
        new_weight_i <- i - idx_change + 1
        updateSliderInput(session = session,
                          inputId = adjustable_weight_id[i],
                          value = new_weights[new_weight_i])
      }
    } else if (tot_weight < 1) {
      port_user_updated_weight(FALSE)
      new_weight <- 1 - updated_weight - sum(adjustable_weights[- length(adjustable_stocks)])
      names(new_weight) <- NULL
      updateSliderInput(session = session,
                        inputId = adjustable_weight_id[length(adjustable_stocks)],
                        value = new_weight)
    }
    port_weight_updated(NULL)
  })
  
  # Update portfolio upon button click
  observeEvent(input$eval_port, {
    # Check that selected stocks and weights are valid
    if (length(port_stocks_temp()$valid) == 0) {
      port_eval_message("Please input at least 1 valid stock symbol. ")
    } else {
      weights <- sapply(port_stocks_temp()$valid, function(stock) {
        input[[paste0("port_", stock, "_weight")]]
      })
      names(weights) <- NULL
      if (sum(weights) != 1 || anyNA(weights)) {
        port_eval_message("Please make sure the weights are valid and sum to 1. ")
      } else {
        port_eval_message(NULL)
        port_stocks(port_stocks_temp()$valid)
        port_weights(weights)
      }
    }
  })
  
  # Portfolio information
  output$port_info <- renderDataTable(
    datatable(data.frame(stocks = port_stocks(), 
                         weights = port_weights()), 
              selection = "none", 
              rownames = FALSE, 
              options = list(pageLength = 5,
                             lengthChange = FALSE,
                             info = FALSE,
                             dom='t',
                             searching=F,
                             sDom  = '<"top">lrt<"bottom">ip'))
  )
  
  # Portfolio summary
  output$port_summary <- renderDataTable(
    datatable(port_analyze(port_stocks(), port_weights()) %>% 
                mutate_if(is.numeric, round, digits = 3), 
              selection = "none", 
              rownames = FALSE, 
              options = list(pageLength = 5,
                             lengthChange = FALSE,
                             info = FALSE,
                             dom='t',
                             searching=F,
                             sDom  = '<"top">lrt<"bottom">ip'))
    
  )
  
  # Portfolio against S&P500 plot
  output$port_spy_plot <- renderPlotly({
    ggplotly(portfolio_fun(port_stocks(),port_weights()))
  })
  
  # Portfolio prediction parameters
  output$port_pred_ui <- renderUI({
    ipo <- tq_get(port_stocks()) %>% 
      group_by(symbol) %>% 
      summarise(ipo.date = min(date, na.rm=T)) %>% 
      pull(ipo.date)
    max_nyear_past <- year(Sys.Date()) - year(max(ipo))
    if ((Sys.Date() - years(max_nyear_past)) <= max(ipo)) {
      max_nyear_past <- max_nyear_past - 1
    }
    if (max_nyear_past == 0) {
      # Display warning when any selected stock is initially offered less than a year ago
      return(
        tagList(
          p(paste("You have selected the following stocks that are initially offered within the past year:", 
                  paste(port_stocks()[ipo >= (Sys.Date() - years(1))], collapse = ", "))), 
          p("This function can only be used to predict performance of stocks with more than a year of history. 
          Please remove these stocks from your portfolio to use this function. ")
        )
      )
    } else {
      tagList(
        fluidRow(
          column(
            4, 
            sliderInput(
              "port_pred_nyear_past", 
              "Number of past years to use to generate prediction: ", 
              min = 1, 
              max = max_nyear_past, 
              value = max_nyear_past
            )
          ), 
          column(
            4, 
            sliderInput(
              "port_pred_nyear_future", 
              "Number of future years make prediction for: ", 
              min = 1, 
              max = 5, 
              value = 3
            )
          ), 
          column(
            4, 
            sliderInput(
              "port_pred_nsimulations", 
              "Number of Monte-Carlo simulations: ", 
              min = 100, 
              max = 300, 
              step = 10,
              value = 100
            )
          )
        ), 
        plotOutput("port_pred_plot")
      )
    }
  })
  
  output$port_pred_plot <- renderPlot({
    monte_carlo_simulation(tickers = port_stocks(), 
                           weights = port_weights(), 
                           num_simulations = input$port_pred_nsimulations, 
                           num_years_past = input$port_pred_nyear_past, 
                           num_years_future = input$port_pred_nyear_future)
  })
}


# Complete app with ui and server components
shinyApp(ui, server)




