library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(e1071)
library(wesanderson)
library(stringr)
library(prophet)
library(forecast)

train<- read.csv('F:/ASU/Courses/R/Case Study/Kaggle Files/train.csv') 
test <- read.csv('F:/ASU/Courses/R/Case Study/Kaggle Files/test.csv')

#Generate Store Item 
test$store_item = paste(str_pad(test$store, 2, pad = "0"),str_pad(test$item, 2, pad = "0"))
train$store_item = paste(str_pad(train$store, 2, pad = "0"),str_pad(train$item, 2, pad = "0"))

#Introduce Sales in Test for final prediction
test$sales = 0 
head(test)

#Write Prediction function 
for(n_item in 1:50)
  for(n_store in 1:10)
  {
  sample = train[which(train$store == n_store & train$item == n_item),]
  stats=data.frame(y=log1p(sample$sales)
                   ,ds=sample$date)
  stats=aggregate(stats$y,by=list(stats$ds),FUN=sum)
  colnames(stats)<- c("ds","y")
 
  playoffs <- data_frame(
    holiday = 'playoff',
    ds = as.Date(c('2013-07-12', '2014-07-12', '2014-07-19',
                   '2014-07-02', '2015-07-11', '2016-07-17',
                   '2016-07-24', '2016-07-07','2016-07-24')),
    lower_window = 0,
    upper_window = 1
  )
  superbowls <- data_frame(
    holiday = 'superbowl',
    ds = as.Date(c('2013-01-01', '2013-12-25', '2014-01-01', '2014-12-25','2015-01-01', '2015-12-25','2016-01-01', '2016-12-25',
                   '2017-01-01', '2017-12-25')),
    lower_window = 0,
    upper_window = 1
  )
  holidays <- bind_rows(playoffs, superbowls)
  #Including additional regressors
  nfl_sunday <- function(ds) {
    dates <- as.Date(ds)
    month <- as.numeric(format(dates, '%m'))
    as.numeric((weekdays(dates) == "Sunday") & (month > 8 | month < 2))
  }
  stats$nfl_sunday <- nfl_sunday(stats$ds)
  
  #Prophet Grid
  prophetGrid <- expand.grid(
    yearly.seasonality = c(1,2,3,4,5),
    changepoint.prior.scale =c(0.001,0.002,0.003,0.004,0.005,0.006),
    holidayspriorscale =c(0.1,0.2,0.3,0.4,0.5),
    capacity = c(6043, 6500, 7000, 8000),
    growth = 'logistic')
  # Search best parameters
  
  for (i in seq_len(nrow(prophetGrid))) {
    parameters <- prophetGrid[i, ]
    if (parameters$growth == 'logistic') {train$cap <- parameters$capacity}
  
  model_prophet <- prophet()
  model_prophet <- prophet(holidays = holidays,holidays.prior.scale = 0.5, yearly.seasonality = 4,
                           interval.width = 0.95,changepoint.prior.scale = 0.006,daily.seasonality = T,growth = parameters$growth)
  model_prophet <- add_regressor(model_prophet, 'nfl_sunday')
  model_prophet <- add_seasonality(model_prophet, name='daily', period=60, fourier.order=5)
  
  model_prophet <- fit.prophet(model_prophet, stats)
  future = make_future_dataframe(model_prophet, periods = 90, freq = 'days')
  future$nfl_sunday <- nfl_sunday(future$ds)
  forecast = predict(model_prophet, future)
  forecast_final<-  xts::last(forecast[, c("ds","yhat")],90)

  #Attach yhat to Test data 
  item_store = paste(str_pad(n_store, 2, pad = "0"),str_pad(n_item, 2, pad = "0"))
  test[test$store_item==item_store,]$sales = expm1(forecast_final$yhat)
  }
test_output = test[,c('id','sales')]
write.csv(test_output,'submission_logistic.csv',row.names = FALSE)