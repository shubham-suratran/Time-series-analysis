library(lubridate)
library(forecast)
library(imputeTS)
library(TTR)
library(tseries)
library(Metrics)
library(ggfortify)
library(ggplot2)
library(GGally)
library(fBasics)
library(MLmetrics)

##Importing Dataset & Initial editing
Data <- read.csv(file.choose())
View(Data)
dim(Data)

Data = Data[c(1,5)]
Data
View(Data)

str(Data)

Date = as.Date(Data$Date,"%Y-%m-%d")
Date

data1 = data.frame(Date, Data$Close)
head(data1)
View(data1)
str(data1)

##Making TS Object & Plotting It
TS <- ts(data1$Data.Close, frequency = 12, start = c(2000,1),
         end = c(2020,12))
TS
View(TS)

plot9 = plot(TS, main = "Close Variable",xlab = "Year", ylab = "Close",
             type = "l",
             lwd = 2.5, col = "Blue")

#Decomposing TS Object and checking acf before making it stationary
is.null(TS)

components = decompose(TS)
ggseasonplot(TS)

acf(TS,main="ACF") #data is non stationary

##Checking if data is stationary
#ADF test
adf.test(data1$Data.Close)# p-value = 0.07667 non stationary

#Box test when lag value is 6
lag.length1=6
Box.test(data1$Data.Close, lag=lag.length1, type="Ljung-Box") #p-value=2.2e-16

##Converting to stationary
stn = diff(data1$Data.Close)
stn
plot(stn, type = "l", col = "purple", lwd = 2,
     main = c(paste("Time Series Values"), 
              paste("(after converting to stationary)")),
     xlab = "Months", ylab = "TS Values")

adf.test(stn) #p-value = 0.01 Stationary

#when lag value is 6
lag.length1=6
Box.test(stn, lag=lag.length1, type="Ljung-Box") #p-value= 0.9013 Stationary


##Acf and pacf plot
acf(stn,main="ACF")
pacf(stn,main="PACF")

length(stn)

##Auto arima
Arima = auto.arima(stn, trace = TRUE,d = 1,stepwise = FALSE,
                   approximation = FALSE)
Arima #(5,1,0)
autoplot(Arima)

##Train-Test split
n=51
train <- head(stn, length(stn)-n)
train
length(train) #200

test <- tail(stn, n)
length(test) #51

## ARIMA model
ARIMA <- arima(train , order = c(5,1,0))
ARIMA
summary(ARIMA)

ARIMA_for = forecast(ARIMA, h = 51)
summary(ARIMA_for)
ARIMA_for = as.data.frame(ARIMA_for)
predicted = ARIMA_for$`Point Forecast`
predicted
summary(ARIMA_for)
mape(predicted, test) # 4.161563

#Forcasting
ARIMA_F <- arima(stn, order = c(5,1,0))
ARIMA_F

ARIMA_f = forecast(ARIMA_F, h = 12)
ARIMA_f
autoplot(ARIMA_f)
plot3 = plot(ARIMA_f, main = "Forecast from ARIMA(5,1,0)",xlab = "Months", 
             ylab = "Close", type = "l",
             lwd = 1.9, col = "Blue")

##Double expo smoothing - des
data_ts_des=holt(TS,h=12)
data_ts_des
plot3 = plot(data_ts_des, main = "Forecast from Double Exponential Smoothing",
             xlab = "Year", 
             ylab = "Close", type = "l",
             lwd = 2, col = "Blue")
data_ts_des$model #Smoothing parameters:alpha = 0.9999,beta  = 1e-04 


