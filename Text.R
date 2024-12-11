# All Text 

# Tab 1 
intro_1 <- "Are you sometimes befuddled when choosing the best future investment? Are you so tired of not catching up with the fluctuation of the financial market? Are you frustrated by the unpredictableness of your stocks? Don't Worry! Our app will be a tool that aims to give you an edge to better optimize your investment strategy in the stock market. Here is a brief overview of this tool. 
"
intro_2 <- "We recommend to visit our data summary first. It provides a general sense of the data running behind the scenes, which is the backbone of all the functions there are built into this app."
intro_3 <-  "The rest of our main tabs serve to illustrate a more analytical pitcure of stocks, with detailed instructions and informative terminologies provided along with the graphs. The market distribution tab explains how different sectors play a role in S&P 500 and how each individual sector did in the past compared to the general market. The stock trend tab allows users to learn about general market trends and compare individual stock’s performance with the general market’s. The stock selection tab provides insights on individual stocks based on K-means clustering. The understand your portfolio page enables users to back test your own newly acquired investment strategies. Finally, the portfolio optimization page optimizes the user's portfolio to minimize risk based on past data. 
"
intro_4 <-  "Now, please give it a try and enjoy exploring the stock market!"


# Tab 2 
data_summary_1 <- "Our data source comes from Yahoo finance, a website that provides financial news, data and commentary including stock quotes, press releases, financial reports, and other original contents. 
Our app mainly uses its records of every single stock/ETF that gets traded on the U.S. stock exchanges on a given day. 
For each stock, there is some basic information such as company name, sector, headquater location, CIK code, year founded and etc. 
The main variables of interests from these stockss are the daily opening, closing, highest and lowest prices, as well as the trading volume everyday."
data_summary_2 <- "Here is a snapshot of some of the largest companies on the U.S. and global stock exchanges (S&P-500, Nasdaq-100 and the Global Dow) looks like."
data_summary_3 <- "And of course, you have the opportunity to view a specific stock and get some basic ideas about its performance! 
Please type in stock symbol or abbreviation. If you are not sure about your stock's symbol, feel free to refer to the link below that contains the list of all stock abbreviation! 
The search fucntion can be used to further refine the output by time. For example, if you want to see your stock's price on a particular date, try type in Year-Month-Date, like 2023-12-20.
Note that the US stock market does not open every day, so be careful about which date to select."


# Tab 2.2
stock_trend_jargon_1 <- "S&P 500 (SPY): A stock market index that measures the stock performance of 500 large companies 
listed on stock exchanges in the United States. It is one of the most commonly followed equity indices.
"
stock_trend_jargon_2 <- "The Vanguard Total Stock Market ETF (VTI) tracks the performance of the CRSP U.S. Total Market Index.
 The fund is a market capitalization-weighted index that measures the entire investable U.S. equity market. It includes small-, mid-, and large-cap companies. The fund is managed in a passive manner and uses an index-sampling strategy.
"
stock_trend_jargon_3 <- "Nasdaq 100 (QQQ): A stock market index made up of 102 equity securities issued by 100 of the largest non-financial companies listed on the Nasdaq stock market, 
which primarily focuses on technology."

stock_trend_jargon_4 <- "Investment worth: How much an one-dollar investment in a specific investment vehicle 
appreciates/depreciates over a period of time. It is used to standardize the return of different investment vehicles. 
"
stock_trend_jargon_5 <- "ETF: An exchange traded fund (ETF) is a basket of securities that trade on an exchange, just like a stock."
stock_trend_1 <- "This tab mainly consists of two functions:"
stock_trend_2 <- "When “Individual Stocks” is selected, the user could input two stocks of interest. 
Then, the user will be given the chance of selecting whether she wants to analyze the stock’s daily investment worth ordaily transaction volume. An option is also given to compare the investment with the S&P to give the user a better sense of whether the stock of interest has outperformed or underperformed the market. 
User could also use the slidebar to select her time frame of interest "
stock_trend_3 <- "When the three indexes are selected, a graph plotting the investment worth of the index will be generated based on the time frame of interest.
A threshold value could also be entered to set targeted values"
stock_trend_4 <- "The user could use this tab to learn about general market trends and compare individual stock’s performance with the general market’s. "


# Tab 3
market_dis_instruction_1 <- "This tab mainly consists of two functions:" 
market_dis_instruction_2 <- "When the 'Price of Each Sector' option is selected, users can track the stock price changes over the years for each sector within the S&P 500."
market_dis_instruction_3 <- "When the 'Volume of Each Sector' option is selected, users can monitor the changes in stock volume over the years for each sector within the S&P 500."
market_dis_instruction_4 <- "The user could use this tab to learn more about how different sectors play a role 
in S&P 500. "
market_dis_instruction_5 <- "The stock symbols for each sector are listed below:"
market_dis_instruction_6 <- "<ul>
                    <li>XLC: Communication Services</li>
                    <br>
                    <li>XLY: Consumer Discretionary</li>
                    <br>
                    <li>XLP: Consumer Staples</li>
                    <br>
                    <li>XLE: Energy </li>
                    <br>
                    <li>XLF: Financials </li>
                    <br>
                    <li>XLV: Health Care </li>
                    <br>
                    <li>XLI: Industrials </li>
                    <br>
                    <li>XLB: Materials </li>
                    <br>
                    <li>XLRE: Real Estate </li>
                    <br>
                    <li>XLK: Technology </li>
                    <br>
                    <li>XLU: Utilities </li>
                    <br>
                  </ul>"


# Tab 4

portfolio_pick_stocks <- "Please enter up to 5 stock symbols seperated by commas (e.x AAPL,MSFT)"
portfolio_stock_weights <- "Please select the corresponding weights"
portfolio_instruction <- "This tab allows users to input different stocks with different corresponding weights. 
A portfolio will be generated based on the stock and weight inputs. 
The portfolio will be compared with the S&P 500 from Jan.1 2000 to the present date, or from the start date of the newest stock to the present date. 
The annualized return and volatility of the portfolio and the S&P 500 market index will be calculated and displayed in a table. 
Users can use this tab to back test their investment strategy with the S&P 500 index."
portfolio_prediction_intro <- "This tab makes prediction of future portfolio price based on Monte-Carlo simulations generated from selected years of past portfolio returns. "


# Tab 5
selection_0 <- "A stock of interest could be entered and a black locator will locate your stock on the graph. "

selection_1 <- "Feel free to click on the graph to view specific stock information. The table below will automatically provide you with stocks with the closet characteristics of your selection."

selection_2 <- "Cluster 1 (Red) = High Return and High Volatility."
selection_3 <- "Cluster 2 (Dark Green) = Low Return and Low Volatility."
selection_4 <- "Cluster 3 (Green) = High Return and low Volatility."
selection_5 <- "Cluster 4 (Blue) = Medium Return and Medium Volatility."
selection_6 <- "Cluster 5 (Purple) = Medium/Low Return and low Volatility."

selection_instruction_1 <- "This tab mainly consist of two functions: "
selection_instruction_2<- "The user could input a stock of interest within the S&P 500 universe, a black pointer will be generated on the cluster graph identifying the stock of interest. The user could easily the cluster that the stock belongs to"
selection_instruction_3<-  "The user could also click on any point on the graph to find out what company it represents along with its neighbors. "
selection_instruction_4<- "The user could use this tab to learn about which cluster her stock of interest belongs to and investigate stocks who are in a similar cluster. The details of how the clusters are generated is detailed in the model building tab. "

selection_model_1<- "This model uses the k-means unsupervised algorithm to separate the S&P 500 stocks 
into 5 different clusters based on return and volatility from 2013-2018. "
selection_model_2<- "Return:  A net gain or loss of an investment over a specified time period, expressed as a percentage of the investment’s initial cost. "
selection_model_3<- "Volatility:  A statistical measure of the dispersion of returns for a given security or market index. In most cases, the higher the volatility, the riskier the security."
selection_model_4<- "K-mean clustering: A method of vector quantization, originally from signal processing, that aims to partition n observations into k clusters in which each observation belongs to the cluster with the nearest mean"

sp500_cluster_interpretation <- "The S&P 500 stocks are clustered into 3 groups by k-means clustering on their past return and volatility. <br>
<br>
<b>Cluster 1</b>: this cluster has medium to high returns but may have higher volatility. While these stocks generally offer high returns, it is important to consider your individual risk tolerance before investing in them. <br>
<b>Cluster 2</b>: this cluster has low returns and somewhat high volatility. Some reasons to invest in these stocks may include diversification of your portfolio or profit from the short-term price movements. <br>
<b>Cluster 3</b>: this cluster has low to medium returns and low volatility. These stocks are a good choice when you have a low risk tolerance. "



