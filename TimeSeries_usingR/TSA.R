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

