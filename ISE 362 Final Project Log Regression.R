### Loading packages for R Studio as well as the data set we will be using for the graphs
library(pacman)
library(datasets)
pacman::p_load(pacman,dplyr,GGally,ggplot2,ggthemes,ggvis,httr,lubridate,plotly,rio,rmarkdown,shiny,stringr,tidyr)
ethereum <- import("~/Desktop/ETH-USD.csv")

### Editing data so it is in order of dates
colnames(ethereum) <- tolower(colnames(ethereum))
ethereum <- ethereum[c((nrow(ethereum)-366) :nrow(ethereum)), ]
ethereum$date <- as.Date(ethereum$date)
ethereum <- ethereum[order(ethereum$date),]

### Establishing variables where x is dates and y are closing prices of said day
date <- ethereum$date
x <- 1:366
y <- ethereum$close

### Initial scatter plot
plot(x,y)

### Making a model for logarithmic regression
model <- lm(y~log(x))
lm(formula = y ~ log(x))

### Display a regression table of the data
summary(model)

### Plot logarithmic regression onto the existing plot
x=seq(from=1,to=366,length.out=10000)
y=predict(model,newdata=list(x=seq(from=1,to=366,length.out=10000)),
          interval="confidence")
matlines(x,y, lwd=2)