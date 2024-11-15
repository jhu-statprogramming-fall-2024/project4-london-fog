# Final Project 777

library(tidyverse)
library(ggthemes)
library(plotly) 

library(DescTools)
library(stringr)
library(lubridate)

library(shiny)
library(shinyWidgets)
library(shinythemes)

library(rsconnect)


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
                 tabPanel("Stock Trends",
                          
                          icon = icon("chart-line")
                          
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
  
}


# Complete app with ui and server components
shinyApp(ui, server)


