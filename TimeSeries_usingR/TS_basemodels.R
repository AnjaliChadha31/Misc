###1.Set Libraries###

library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(e1071)
library(wesanderson)
library(stringr)
library(prophet)
library(forecast)

### 2. Read Files & sample ###
train<- read.csv('F:/ASU/Courses/R/Case Study/Kaggle Files/train.csv') 
test <- read.csv('F:/ASU/Courses/R/Case Study/Kaggle Files/test.csv')
train_sample <- setDT(train)[store == 1 & item == 1]
## Create a daily Date object - helps my work on dates
inds <- seq(as.Date(min(train_sample$date)), as.Date(max(train_sample$date)), by = "day")
## Create a time series object
set.seed(25)
myts <- ts(train_sample[,c("sales")],     
           start = c(2013,1),
           frequency = 365)
summary(train_sample$sales)
###3.Naive Plot###
fcnv <- naive(myts, h = 90 )
autoplot(fcnv,xlab = 'Year', ylab = 'Forecast')
summary(fcnv)

###4. Seasonal Naive###
fcsnv <- snaive(myts, h = 90)
autoplot(fcsnv,xlab = 'Year', ylab = 'Forecast')
summary(fcnv)

###5. Simple Exponenetial Smoothning###
fcses <- ses(myts, h = 90)
autoplot(fcses, xlab = 'Year', ylab = 'Forecast')
summary(fcses)

#Check Residuals for white noise
checkresiduals(fcses)

### Accuracy of SES, Seasonal Naive & Naive

# Create a training set using subset()
train <- subset(myts, end = length(myts) - 365)

# Compute SES and naive forecasts, save to fcses and fcnaive
fcses <- ses(train, h = 365)
fcsnaive <- snaive(train, h = 365)

# Calculate forecast accuracy measures
accuracy(fcses, myts)


###6. Holts Method ###
fcholt <- holt(myts, h =90)
autoplot(fcholt, xlab = 'Year', ylab = 'Forecast')

# check Residuals
checkresiduals(fcholt)

### Arima MOdel
fcarima <- auto.arima(myts)

fcarima %>% forecast(h = 90) %>% autoplot()
checkresiduals(fcarima)



###6.

