library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(e1071)
library(wesanderson)
library(stringr)
library(prophet)
library(forecast)

### 5. Prophet Model###
train_final_store1_item1=subset(train_data,train_data$store==1 & train_data$item==1)

stats=data.frame(y=log1p(train_final_store1_item1$sales)
                 ,ds=train_final_store1_item1$date)
stats=aggregate(stats$y,by=list(stats$ds),FUN=sum)
colnames(stats)<- c("ds","y")
head(stats)


model_prophet = prophet(stats)
summary(model_prophet)
future = make_future_dataframe(model_prophet, periods = 90)
forecast = predict(model_prophet, future)

#Visuliaze Changepoints#
add_changepoints_to_plot <- function(m, threshold = 0.01, cp_color = "red",
                                     cp_linetype = "dashed", trend = TRUE, ...) {
  layers <- list()
  if (trend) {
    trend_layer <- ggplot2::geom_line(
      ggplot2::aes_string("ds", "trend"), color = cp_color, ...)
    layers <- append(layers, trend_layer)
  }
  signif_changepoints <- m$changepoints[abs(m$params$delta) >= threshold]
  cp_layer <- ggplot2::geom_vline(
    xintercept = as.integer(signif_changepoints), color = cp_color,
    linetype = cp_linetype, ...)
  layers <- append(layers, cp_layer)
  return(layers)
}
plot(model_prophet, forecast)+ add_changepoints_to_plot(model_prophet)

#Inspecting Model Components#
prophet_plot_components(model_prophet, forecast)

# Including Holidays & superbowls
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


model_prophet <- prophet()
model_prophet <- prophet(holidays = holidays,holidays.prior.scale = 0.5, yearly.seasonality = 4,
                         interval.width = 0.95,changepoint.prior.scale = 0.006,daily.seasonality = T)
model_prophet <- add_regressor(model_prophet, 'nfl_sunday')
model_prophet <- add_seasonality(model_prophet, name='daily', period=60, fourier.order=5)

model_prophet <- fit.prophet(model_prophet, stats)
future = make_future_dataframe(model_prophet, periods = 90, freq = 'days')
future$nfl_sunday <- nfl_sunday(future$ds)
forecast = predict(model_prophet, future)

# plot to see changepoints
plot(model_prophet, forecast) + add_changepoints_to_plot(model_prophet)
prophet_plot_components(model_prophet, forecast)



    


