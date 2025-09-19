library(forecast)
library(fpp2)

#question 1b
x <- c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5)
y <- c(3.9, 4.4, 5.8, 6.6, 7.0, 7.1, 7.3, 7.7)

data <- data.frame(Fertilizer = x, Yield = y)
model <- lm(Yield ~ Fertilizer, data = data)
summary(model)
regression <- lm(y~x)
plot(x,y)
abline(regression)

#question 2
#2a
maple <- read.csv(file.choose(),header = TRUE)
head(maple)

model_lat <- lm(LeafIndex ~ Latitude, data = maple)

summary(model_lat) 

plot(maple$Latitude, maple$LeafIndex,
     main = "LeafIndex vs Latitude",
     xlab = "Latitude", ylab = "Leaf Index", pch = 19)
abline(model_lat)


#2b
model_temp <- lm(LeafIndex ~ JulyTemp, data = maple)
summary(model_temp)
plot(maple$JulyTemp, maple$LeafIndex,
     main = "LeafIndex vs JulyTemp",
     xlab = "July Temperature", ylab = "Leaf Index", pch = 19)
abline(model_temp)

#2c
model_multi <- lm(LeafIndex ~ Latitude + JulyTemp, data = maple)
summary(model_multi)
par(mfrow = c(2, 2))
plot(model_multi)


#question 3
myopia <-  read.csv(file.choose(),header = TRUE)
head(myopia)

#3a
dim(myopia)

#3b
model <- lm(MYOPIC ~ SPHEQ, data = myopia)
summary(model)
coef(model)  

#3c
model2 <- predict(model, newdata = data.frame(SPHEQ = 1.1765)) 
fc <- forecast(model, newdata = data.frame(SPHEQ = 1.1765))
print(fc)
summary(model2) 

#3d
model_multi <- lm(MYOPIC ~ AGE + GENDER + SPHEQ + VCD, data = myopia)
summary(model_multi)
coef(model_multi)

#3e
predict(model_multi, newdata = data.frame(AGE = 8, GENDER = 0, SPHEQ = 1.2, VCD = 16)) 
fc1 <- forecast(model_multi, newdata = data.frame (AGE = 8, GENDER = 0, SPHEQ = 1.2, VCD = 16))
print(fc1)
summary(fc1)

#question 4

#4a
model_mommy_age <- lm(MOMMY ~ AGE, data = myopia)
summary(model_mommy_age)
coef(model_mommy_age)

#4b
predict(model_mommy_age, newdata = data.frame(AGE = 9))
fc2 <- forecast(model_mommy_age, newdata = data.frame(AGE = 9))
print(fc2)
summary(fc2)

#4c
model_mommy_multi <- lm(MOMMY ~ AL + LT + READHR, data = myopia)
summary(model_mommy_multi)
coef(model_mommy_multi)

#4d
predict(model_mommy_multi, newdata = data.frame(AL = 24, LT = 3.5, READHR = 4)) 
fc3 <- forecast(model_mommy_multi, newdata = data.frame (AL = 24, LT = 3.5, READHR = 4))
print(fc3)
summary(fc3)

#question 5
library(ISLR2)
head(Carseats)

#5a
model <- lm(Sales ~ CompPrice + Income + Advertising + Population, data = Carseats)
summary(model)

#5b 
coef(model)
print("Sales = 6.5755650 + (-0.0566973 * CompPrice) + (0.0121198 * Income) + (0.1122992 * Advertising) + (0.0001917 * Population)")

#5c
fc3 <- forecast(model, newdata = data.frame (CompPrice = 120, Income = 75,Advertising = 8, Population = 140))
print(fc3)
summary(fc3)
               