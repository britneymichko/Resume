library(ISLR2)
library(MASS)
library(ggplot2)
data("Weekly")
head(Weekly)
View(Weekly)
dim(Weekly)
summary(Weekly)

#question 1
#1a
summary(Weekly)
ggplot(Weekly, aes(x = Lag1, y = Lag2, color = Direction)) + geom_point()
ggplot(Weekly, aes(x = Lag2, y = Lag3, color = Direction)) + geom_point()
ggplot(Weekly, aes(x = Lag4, y = Lag5, color = Direction)) + geom_point()
ggplot(Weekly, aes(x = Volume, y = Today, color = Direction)) + geom_point()

#1b
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef

#1c 
glm.probs <- predict(glm.fit, type = "response")
contrasts(Weekly$Direction) 
lm.pred <- rep("Down", length(glm.probs))
lm.pred [glm.probs > 0.5] <- "Up"
table(lm.pred, Weekly$Direction)
mean(lm.pred == Weekly$Direction)

#1d
train_data <- Weekly[Weekly$Year <= 2008, ]
test_data <- Weekly[Weekly$Year > 2008, ]
log_fit2 <- glm(Direction ~ Lag2, data = train_data, family = binomial)
log_probs2 <- predict(log_fit2, newdata = test_data, type = "response")
log_preds2 <- ifelse(log_probs2 > 0.5, "Up", "Down")
table(Predicted = log_preds2, Actual = test_data$Direction)
mean(log_preds2 == test_data$Direction)

#1e
lda.fit <- lda(Direction ~ Lag2, data = train_data)
lda.fit
lda.pred <- predict(lda.fit,  newdata = test_data)
names(lda.pred)
lda.class <- lda.pred$class
table(Predicted = lda.pred$class, Actual = test_data$Direction)
mean(lda.pred$class == test_data$Direction)

#1f
qda.fit <- qda(Direction ~ Lag2, data = train_data)
qda.fit
qda.pred <- predict(qda.fit, newdata = test_data)
table(Predicted = qda.pred$class, Actual = test_data$Direction)
mean(qda.pred$class == test_data$Direction)

#question 2
#2c 
x <- c(1.0, 1.5, 2.0, 4.0, 3.0, 5.2)
y <- as.factor(c(1, 1, 1, 0, 0, 0))
data <- data.frame(x = x, y = y)
lda_fit <- lda(y ~ x, data = data)
lda_fit
new_obs <- data.frame(x = 3.5)
predict(lda_fit, new_obs)






