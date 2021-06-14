library("shiny")
library("shinydashboard")
library('stringr')
library('shinyBS')
library("zoo")
library("dplyr")
library("lubridate")
library("DT")
library("utils")
library("highcharter")
library("corrplot")



AAPL <- read.csv("AAPL.csv")
AMZN <- read.csv("AMZN.csv")
FB <- read.csv("FB.csv")
GOOG <-read.csv("GOOG.csv")
MSFT <- read.csv("MSFT.csv")
NFLX <- read.csv("NFLX.csv")
Russell_3000 <- read.csv("Russell_3000.csv",skip=8)
df <- cbind(Apple=AAPL$Close, Amazon=AMZN$Close, Facebook=FB$Close, 
            Google=GOOG$Close, Microsoft=MSFT$Close, Netflix=NFLX$Close) %>% na.omit()
df_plot <- data.frame(cbind(date=AAPL$Date, df))

shinyUI(dashboardPage(
  dashboardHeader(title = "Stock Analysis",titleWidth = 450),
  dashboardSidebar(
    uiOutput("fname"),
    sidebarMenu(
      menuItem("Russell_3000", tabName = "russell3000"),
      menuItem("S & P 500", tabName = "snp500"),
      menuItem("Correlation Matrix", tabName = "cormat"),
      menuItem("Big All", tabName = "bigall"),
      menuItem("Big2", tabName = "big2"),
      menuItem("Custom Text", tabName = "customtext")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("russell3000",
              fluidRow(
                column(12,dataTableOutput('table_russell')))),
      tabItem("snp500",
              fluidRow(
                column(12,dataTableOutput('table_snp500')))),
      tabItem("cormat",
              fluidRow(
                column(12,plotOutput('plot_correlation')))),
      tabItem("bigall",
              fluidRow(
                column(3,dateRangeInput('dateRange_bigall',
                                      label = 'Date range input: yyyy-mm-dd',
                                      start = mdy(min(df_plot$date)), end = mdy(max(df_plot$date)),
                                      min = mdy(min(df_plot$date)), max = mdy(max(df_plot$date)))),
                column(9,highchartOutput('plot_bigall')))),
      
      tabItem("big2",
              fluidRow(
                column(3,dateRangeInput('dateRange_big2',
                                        label = 'Date range input: yyyy-mm-dd',
                                        start = mdy(min(df_plot$date)), end = mdy(max(df_plot$date)),
                                        min = mdy(min(df_plot$date)), max = mdy(max(df_plot$date))),
                        selectInput('big1', "big1", c("Apple","Amazon","Facebook","Google","Microsoft","Netflix"), selected = "Amazone"),
                        uiOutput("big2")),
                column(9,highchartOutput('plot_big2')))),

      tabItem("customtext",
              fluidRow(column(12,includeMarkdown("conclusion.md"))))

  ))))