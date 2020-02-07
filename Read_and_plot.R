library(TSA)
library(mgcv)
library(tseries)

fname <- file.choose()
data <- read.csv(fname)

# Example 1
colnames(data) = c('Month', 'Level')
level.ts = ts(data$Level, start=c(1921,1), frequency = 12)

# Example 2
data <- data[,2]
#Convert to TS data in proper frame
rate <- ts(data,start=c(2000,1),freq=52)

# Example 3
library(TSA)
library(data.table)
library(vars)
fname <- file.choose()
data <- read.csv(fname)
train <- data[0:(length(data[,1])-8),]
train.ts <- ts(train[,c(2,3,4,5)],start=c(2014,1),freq=52)
# Keeping last 8 points for testing
test <- data[(length(data[,1])-7):length(data[,1]),]
#You'll need this for plotting predictions
whole <- ts(data$USD.EU,start=c(2014,1),freq=52)
times = time(whole)
times.test = tail(times,8)
rm(whole)
