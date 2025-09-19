
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













#(might not need these)

model2 <- HoltWinters(apple_ts, alpha = 0.2, beta = 0.1, gamma = FALSE) # Adding smoothing trend component
model2.pred <- predict(model2, n.ahead = 24, prediction.interval = TRUE)
model2
model2.pred

plot.ts(apple_ts)
lines(model2$fitted[,1], col = "orange")
lines(model2.pred[,1], col = "blue")
lines(model2.pred[,2], col = "red")
lines(model2.pred[,3], col = "red")



model3 <- HoltWinters(apple_ts,  gamma = FALSE)
model3.pred <- predict(model3, n.ahead = 24, prediction.interval = TRUE)
model3
model3.pred

plot.ts(apple_ts)
lines(model3$fitted[,1], col = "orange")
lines(model3.pred[,1], col = "blue")
lines(model3.pred[,2], col = "red")
lines(model3.pred[,3], col = "red")


model4 <- HoltWinters(apple_ts, alpha = 0.2, beta = 0.1, gamma = 0.1) 
model4.pred <- predict(model4, n.ahead = 24, prediction.interval = TRUE)
model4
model4.pred

plot.ts(apple_ts)
lines(model4$fitted[,1], col = "orange")
lines(model4.pred[,1], col = "blue")
lines(model4.pred[,2], col = "red")
lines(model4.pred[,3], col = "red")


model5 <- HoltWinters(apple_ts)
model5.pred <- predict(model5, n.ahead = 24, prediction.interval = TRUE)
model5

plot.ts(apple_ts)
lines(model5$fitted[,1], col = "orange")
lines(model5.pred[,1], col = "blue")
lines(model5.pred[,2], col = "red")
lines(model5.pred[,3], col = "red")

model6 <- HoltWinters(apple_ts, seasonal = "multiplicative")
model6.pred <- predict(model6, n.ahead = 24, prediction.interval = TRUE)
model6

plot.ts(apple_ts)
lines(model6$fitted[,1], col = "orange")
lines(model6.pred[,1], col = "blue")
lines(model6.pred[,2], col = "red")
lines(model6.pred[,3], col = "red")


#(doesnt work)
Box.test(model1$residuals, lag = 1, type = "Ljung-Box")
Box.test(model1$residuals, lag = 5, type = "Ljung-Box")
Box.test(model1$residuals, lag = 10, type = "Ljung-Box")
Box.test(model1$residuals, lag = 12, type = "Ljung-Box")
Box.test(model1$residuals, lag = 15, type = "Ljung-Box")










summary(apple)
str(apple)
start(apple)
end(apple)
frequency(apple)
plot(apple)
boxplot(apple~cycle(apple))







# Ensure the 'Date' column is in Date format
apple_data <- apple_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# Arrange data by date
apple_data <- apple_data %>%
  arrange(Date)

# Create a time series object
apple_ts <- ts(apple_data$Close, start = c(year(min(apple_data$Date)), month(min(apple_data$Date))), frequency = 12)
