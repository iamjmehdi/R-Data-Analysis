#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)

monthly_returns <-function(ticker, base_year)
{
    #Obtain stock price data
    stock <-getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
    #Remove missing values
    stock <- na.omit(stock)
    #Keep only adjusted closing prices
    stock <- stock[,6]
    
    #Begin at 2021 as base year and end at the last available trading day
    horizon <-paste0(as.character(base_year), "/", as.character(Sys.Date())))
    stock <- stock[horizon]
    
    #Calculate monthly arithmetic returns
    data <-periodReturn(stock, period ="monthly", type ="arithmetic")
    
    #Assign to the gloval environment to be accessible
    assign(ticker, data, envir = .GlobalEnv)
}

#Call our function for each stock
monthly_returns("FB", 2021)
monthly_returns("AMZN", 2021)
monthly_returns("NFLX", 2021)
monthly_returns("GOOG", 2021)
monthly_returns("AAPL", 2021)
monthly_returns("MSFT", 2021)

#Get S&P500 Data
monthly_returns("SPY", 2021)

#Merge All the Data and Rename Columns
returns <-merge.xts(FB, AMZN, NFLX, GOOG, AAPL, MSFT)
colnames(returns) <- c("FB", "AMZN", "NFLX", "GOOG", "AAPL", "MSFT", "SP500")

#Produce Interactive Chart
dygraph(returns, main = "FB vs AMZN vs NFLX vs GOOG vs AAPL vs MSFT vs SP500") %>%
    dyAxis("y", label = "Return", valueRange = c(-1, 0.5)) %>%
    dyRangeSelector(dateWindow = c("2021-01-04", "2021-06-11")) %>%
    dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2"))

#Round
round(tail(returns, n = 5), 4)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Market Making and Statistical Arbitrage"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
