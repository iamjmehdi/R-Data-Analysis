library("shiny")
library("dplyr")
library("ggplot2")
library("DT")
library("lubridate")
library("highcharter")
library("corrplot")


AAPL <- read.csv("AAPL.csv")
AMZN <- read.csv("AMZN.csv")
FB <- read.csv("FB.csv")
GOOG <-read.csv("GOOG.csv")
MSFT <- read.csv("MSFT.csv")
NFLX <- read.csv("NFLX.csv")
Russell_3000 <- read.csv("Russell_3000.csv",skip=8)
SnP_500 <- read.csv("S_and_P_500.csv")

df <- cbind(Apple=AAPL$Close, Amazon=AMZN$Close, Facebook=FB$Close, 
            Google=GOOG$Close, Microsoft=MSFT$Close, Netflix=NFLX$Close) %>% na.omit()

df_plot <- data.frame(cbind(date=AAPL$Date, df))


shinyServer(function(input,output,session){
  
  output$big2 <- renderUI({
    selectInput('big2', "big2",setdiff(c("Apple","Amazon","Facebook","Google","Microsoft","Netflix"), input$big1), selected = "Facebook")
  })
    
    output$table_russell =renderDataTable({
      datatable(Russell_3000)})
    
    output$table_snp500 =renderDataTable({
      datatable(SnP_500)})
    
    output$plot_correlation = renderPlot({
      d <- cor(df)
      corrplot(d, method = "number", type="upper")
    })

    output$plot_bigall = renderHighchart({
      
      plot_data <- df_plot %>%
        filter(mdy(date)>=input$dateRange_bigall[1] & mdy(date)<=input$dateRange_bigall[2]) %>%
        mutate(date=mdy(date),
               Amazon = as.numeric(Amazon),
               Facebook = as.numeric(Facebook),
               Apple = as.numeric(Apple),
               Google = as.numeric(Google),
               Microsoft = as.numeric(Microsoft),
               Netflix  =as.numeric(Netflix)
               )
      
      plot_data_bigall = plot_data[order(plot_data$date),]
      highchart() %>%
        hc_title(text = "All 6 Stocks",style = list(fontWeight = "bold")) %>%
        hc_xAxis(categories = plot_data_bigall$date ) %>%
        hc_add_series(name = 'Amazon',data = plot_data_bigall$Amazon, type = "line") %>%
        hc_add_series(name = 'Facebook',data = plot_data_bigall$Facebook, type = "line") %>%
        hc_add_series(name = 'Apple',data = plot_data_bigall$Apple, type = "line") %>%
        hc_add_series(name = 'Netflix',data = plot_data_bigall$Netflix, type = "line") %>%
        hc_add_series(name = 'Microsoft',plot_data_bigall$Microsoft, type = "line") %>%
        hc_add_series(name = 'Google',data = plot_data_bigall$Google, type = "line")
      
    })
    
    output$plot_big2 <- renderHighchart({
      plot_data <- df_plot %>%
        filter(mdy(date)>=input$dateRange_big2[1] & mdy(date)<=input$dateRange_big2[2]) %>%
        mutate(date=mdy(date),
               Amazon = as.numeric(Amazon),
               Facebook = as.numeric(Facebook),
               Apple = as.numeric(Apple),
               Google = as.numeric(Google),
               Microsoft = as.numeric(Microsoft),
               Netflix  =as.numeric(Netflix)
        )
      
      plot_data_big2 = plot_data[order(plot_data$date),]
      highchart() %>%
        hc_title(text = "2 Stocks",style = list(fontWeight = "bold")) %>%
        hc_xAxis(categories = plot_data_big2$date ) %>%
        hc_add_series(name = input$big1,data = eval(parse(text=paste("plot_data_big2$",input$big1,sep=""))), type = "line") %>%
        hc_add_series(name = input$big2,data = eval(parse(text=paste("plot_data_big2$",input$big2,sep=""))), type = "line")
      
    })
})
