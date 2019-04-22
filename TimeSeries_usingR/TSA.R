###1.Load Libraries###
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(e1071)
library(wesanderson)
library(prophet)
library(forecast)

###2.Read Data - Structure & Summary ###
train_data <- read.csv('F:/ASU/Courses/R/Case Study/Kaggle Files/train.csv') 
test_data <- read.csv('F:/ASU/Courses/R/Case Study/Kaggle Files/test.csv')
head(train_data,20)
str(train_data)
str(test_data)
summary(train_data)
summary(test_data)

#Extraction of Year & Month of Year in Train Data#
train_data<- train_data %>%
              mutate(year = year(date), month_year = format(as.Date(date),"%Y-%m"),log_sales = log1p(sales))

###3.Missing Value Detection###
colSums(is.na(train_data))
# Function 1 : To plott missing value
plot_missing <- function(data, title = NULL, ggtheme = theme_gray(), theme_config = list("legend.position" = c("bottom"))) {
  ## Declare variable first to pass R CMD check
  feature <- num_missing <- pct_missing <- group <- NULL
  ## Check if input is data.table
  is_data_table <- is.data.table(data)
  ## Detect input data class
  data_class <- class(data)
  ## Set data to data.table
  if (!is_data_table) data <- data.table(data)
  ## Extract missing value distribution
  missing_value <- data.table(
    "feature" = names(data),
    "num_missing" = sapply(data, function(x) {sum(is.na(x))})
  )
  missing_value[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
  missing_value[, pct_missing := num_missing / nrow(data)]
  missing_value[pct_missing < 0.05, group := "Good"]
  missing_value[pct_missing >= 0.05 & pct_missing < 0.4, group := "OK"]
  missing_value[pct_missing >= 0.4 & pct_missing < 0.8, group := "Bad"]
  missing_value[pct_missing >= 0.8, group := "Remove"][]
  ## Set data class back to original
  if (!is_data_table) class(missing_value) <- data_class
  ## Create ggplot object
  output <- ggplot(missing_value, aes_string(x = "feature", y = "num_missing", fill = "group")) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(100 * pct_missing, 2), "%"))) +
    scale_fill_manual("Group", values = c("Good" = "#1a9641", "OK" = "#a6d96a", "Bad" = "#fdae61", "Remove" = "#d7191c"), breaks = c("Good", "OK", "Bad", "Remove")) +
    scale_y_continuous(labels = NULL) +
    coord_flip() +
    xlab("Features") + ylab("Number of missing rows") +
    ggtitle(title) +
    ggtheme + theme_linedraw()+
    do.call(theme, theme_config)
  ## Print plot
  print(output)
  ## Set return object
  return(invisible(missing_value))
}
plot_missing(train_data)

###4. Histogram of Sales Price###
ggplot(train_data,aes(x=sales),title="Histogram of Sales")+geom_histogram(fill="#a6d96a", alpha=.9)
ggplot(train_data,aes(x=log1p(sales)),title="Histogram of Sales")+geom_histogram(fill="#a6d96a", alpha=.9)
#check if Sales Price is normally distributed

#QQ Plot & Shaiparo Test - Data not normally distributed#
qqnorm(train_data$log_sales,pch=1,frame=FALSE)
qqline(train_data$log_sales,col="steelblue",lwd=2)
sam_train_data <- sample(train_data$log_sales, size = 5000,replace=FALSE)
shapiro.test(sam_train_data)
skewness(train_data$sales)
kurtosis(train_data$sales)

###5. Growth by rate(by day) - Seasonality as well as trend ###
#Mean of sales each day
gbp1<-wes_palette("GrandBudapest2")[1]
MSP <- aggregate(sales ~date, train_data, mean)
ggplot(MSP,aes(x=as.factor(date),y=sales))+
  geom_line(color=gbp1, aes(group=1), size=1.5)+
  geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="The Growth of Sale Prices by date", x=NULL, y="Sale Price")+
  theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

#Growth Rate each day
MSP$rate = c(0, 100*diff(MSP$sales)/MSP[-nrow(MSP),]$sales)
ggplot(MSP, aes(x=as.factor(date), y=rate))+
  geom_line(color= "gray50", aes(group=1), size=1)+
  #geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="Change rate of Sale Price", x="date", y="rate of change")+
  geom_hline(yintercept = 0, color = gbp1 )+
  theme(plot.title=element_text(size=15))+ theme_minimal()

#Certain days have spike let's look at grwoth rate each month/year

#Grwoth rate (by month)

MSP <- aggregate(sales ~month_year, train_data, mean)
ggplot(MSP, aes(x=as.factor(month_year), y=sales))+
  geom_line(color=gbp1, aes(group=1), size=1.5)+
  geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="The Growth of Sale Prices by Month of Year", x=NULL, y="Sale Price")+
  theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

MSP$rate = c(0, 100*diff(MSP$sales)/MSP[-nrow(MSP),]$sales)

ggplot(MSP, aes(x=as.factor(month_year), y=rate))+
  geom_line(color= "gray50", aes(group=1), size=1)+
  #geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="Change rate of Sale Price", x="Month", y="rate of change")+
  geom_hline(yintercept = 0, color = gbp1 )+
  theme(plot.title=element_text(size=15))+ theme_minimal()


# Growth Rate by Year 
MSP <- aggregate(sales ~Year, train_data, mean)
# MSP <-na.omit(ddply(data, 'date', summarise, mean(Sale_Prices, na.rm=T)))

sl1 <-ggplot(MSP, aes(x=as.factor(Year), y=sales))+
  geom_line(color=gbp1, aes(group=1), size=1.5)+
  geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="The Growth of Sale Prices by Year", x=NULL, y="Sale Price")+
  theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

MSP$rate = c(0, 100*diff(MSP$sales)/MSP[-nrow(MSP),]$sales)

sl2 <-ggplot(MSP, aes(x=as.factor(Year), y=rate))+
  geom_line(color= "gray50", aes(group=1), size=1)+
  #geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="Change rate of Sale Price", x="Year", y="rate of change")+
  geom_hline(yintercept = 0, color = gbp1 )+
  theme(plot.title=element_text(size=15))+ theme_minimal()

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

#SMAPE Calculation for store 1 Item 1 if u wish to do 

# Genralising for all stores & item
#Rename Cols for Train & Test
head(train_data)
colnames(test) = c('id','ds','store','item')
colnames(train) = c('ds','store','item','y')

prediction<-function(df)
{
  playoffs <- data_frame(
    holiday = 'playoff',
    ds = as.Date(c('2013-07-12', '2014-07-12', '2014-07-19',
                   '2014-07-02', '2015-07-11', '2016-07-17',
                   '2016-07-24', '2016-07-07','2016-07-24')),
    lower_window = 0,
    upper_window = 1
  )
  
  #######  I have inlcuded the Holiday Sales fofr Festive seasons like New Year & Christmas for different Years in Superbowls.
  superbowls <- data_frame(
    holiday = 'superbowl',
    ds = as.Date(c('2013-01-01', '2013-12-25', '2014-01-01', '2014-12-25','2015-01-01', '2015-12-25','2016-01-01', '2016-12-25',
                   '2017-01-01', '2017-12-25')),
    lower_window = 0,
    upper_window = 1
  )
  holidays <- bind_rows(playoffs, superbowls)
  
  
  
  
  model_prophet <- prophet()
  model_prophet <- add_seasonality(model_prophet, name='daily', period=60, fourier.order=5)
  model_prophet <- prophet(df, holidays = holidays,holidays.prior.scale = 0.5, yearly.seasonality = 4,
                           interval.width = 0.95,changepoint.prior.scale = 0.006,daily.seasonality = T)
  
  
  future = make_future_dataframe(model_prophet, periods = 90)
  forecast = predict(model_prophet, future)
  forecast_final<-  xts::last(forecast[, c("ds","yhat")],90)
  return(forecast_final)
  
}





