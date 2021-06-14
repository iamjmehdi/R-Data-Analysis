#import libraries
library(tidyverse)
library(corrplot)
library(dplyr)
library(ggplot2)
library("lubridate")

AAPL <- read.csv("AAPL.csv")
AMZN <- read.csv("AMZN.csv")
FB <- read.csv("FB.csv")
GOOG <-read.csv("GOOG.csv")
MSFT <- read.csv("MSFT.csv")
NFLX <- read.csv("NFLX.csv")

#Form FANGAM Stocks vector data frame
df <- cbind(Apple=AAPL$Close, Amazon=AMZN$Close, Facebook=FB$Close, 
            Google=GOOG$Close, Microsoft=MSFT$Close, Netflix=NFLX$Close) %>%
  na.omit()

#Calculate Correlation between all 6 FANGAM Stocks
d <- cor(df)
corrplot(d, method = "number", type="upper")

df_plot <- data.frame(cbind(date=AAPL$Date, df))
head(df_plot)

df_plot %>%
  pivot_longer(cols=-1,
               names_to="stock",
               values_to="value") %>%
  mutate(date=mdy(date),
         value=parse_number(value))%>%
  filter(stock %in% c("Amazon", "Facebook"))%>%
  ggplot(aes(x=date, y=value,color=stock))+
  geom_point()+
  geom_line()+
  theme_light()+
  labs(title="Pairs Trading",
       y="Stock Price",
       x="Date")

myplot <- function(df,a,b) {
    df_plot %>%
    pivot_longer(cols=-1,
                 names_to="stock",
                 values_to="value") %>%
    mutate(date=mdy(date),
           value=parse_number(value))%>%
    filter(stock %in% c(a, b))%>%
    ggplot(aes(x=date, y=value,color=stock))+
    geom_point()+
    geom_line()+
    theme_light()+
    labs(title="Pairs Trading",
         y="Stock Price",
         x="Date")
}

#Pair Trading Graphs
myplot(df_plot,"Facebook","Amazon")
myplot(df_plot,"fb","nflx")
myplot(df_plot,"fb","goog")
myplot(df_plot,"fb","aapl")
myplot(df_plot,"fb","msft")

myplot(df_plot,"amazon","nflx")
myplot(df_plot,"amazon","goog")
myplot(df_plot,"amazon","aapl")
myplot(df_plot,"amazon","msft")

myplot(df_plot,"nflx","goog")
myplot(df_plot,"nflx","aapl")
myplot(df_plot,"nflx","msft")

myplot(df_plot,"fb","goog")
myplot(df_plot,"fb","msft")

myplot(df_plot,"fb","goog")

#FANGAM Stocks YTD Graph
df_plot %>%
  pivot_longer(cols=-1,
               names_to="stock",
               values_to="value") %>%
  mutate(date=mdy(date),
         value=parse_number(value))%>%
  #filter(stock %in% c("amazon", "fb"))%>%
  ggplot(aes(x=date, y=value,color=stock))+
  geom_point()+
  geom_line()+
  theme_light()+
  labs(title="FANGAM Stocks YTD",
       y="$ Closing Price",
       x="Date")

plot_data_bigall <- df_plot %>%
  mutate(date=mdy(date))

plot_data_bigall = plot_data_bigall[order(plot_data_bigall$date),]

print(names(plot_data_bigall))

highchart() %>%
  hc_title(text = "All 6 Stocks",style = list(fontWeight = "bold")) %>%
  hc_xAxis(categories = plot_data_bigall$date) %>%
  hc_add_series(data = as.numeric(plot_data_bigall$Amazon))


Russell_3000 <- read_csv("Russell_3000.csv",skip=8)
DT::datatable(Russell_3000)

