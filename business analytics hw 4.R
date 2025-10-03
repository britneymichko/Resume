
library(tidyverse)
library(lubridate)
library(forecast)


apple <- read.csv(file.choose(),header = TRUE)


names(apple) <- trimws(names(apple))
# Convert Date to proper format (Day-Month-Year, 2-digit year)
apple$Date <- as.Date(apple$Date, format = "%d-%b-%y")


# Create time series (monthly data)
start_year <- year(min(apple$Date))
start_month <- month(min(apple$Date))

apple_ts <- ts(apple$Close, start = c(start_year, start_month), frequency = 12)

# View time series info
summary(apple_ts)
str(apple_ts)
start(apple_ts)
end(apple_ts)
frequency(apple_ts)
plot(apple_ts)
boxplot(apple_ts~cycle(apple_ts))


# Load zoo for interpolation
library(zoo)

apple$Close <- na.approx(apple$Close)

start_year <- year(min(apple$Date))
start_month <- month(min(apple$Date))
apple_ts <- ts(apple$Close, start = c(start_year, start_month), frequency = 12)

# Decompose
apple_decom <- decompose(apple_ts, type = "multiplicative")

plot(apple_decom$seasonal)
plot(apple_decom$trend)
plot(apple_decom$random)
plot(apple_decom)
apple_decom$figure

plot(apple_decom$figure, type = 'b', xlab = "month", ylab = "seasonality index", col = 'blue')


#forecasted next year
model1 <- HoltWinters(apple$Adj.Close., alpha = 0.2, beta = FALSE, gamma = FALSE)
model1.pred <- predict(model1, n.ahead = 12, prediction.interval = TRUE)
model1
model1.pred

plot.ts(apple_ts, main = "Apple Closing Price Forecast (Holt-Winters)", ylab = "Price", col = "black")
lines(model1$fitted[,1], col = "orange")
lines(model1.pred[,1], col = "blue")       
lines(model1.pred[,2], col = "red", lty=2)
lines(model1.pred[,3], col = "red", lty=2)


