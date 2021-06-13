#import libraries
library(tidyverse)
library(corrplot)
library(dplyr)
library(ggplot2)

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
  filter(stock %in% c("amazon", "fb"))%>%
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
myplot(df_plot,"fb","amazon")
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

Russell_3000 <- read_csv("Russell_3000.csv",skip=8)
DT::datatable(Russell_3000)
