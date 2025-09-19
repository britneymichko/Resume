techsales <- read.csv(file.choose(),header = TRUE)

library(ISLR2)
library(boot)
library(caret)

class(techsales)   
names(techsales)   
techsales <- na.omit(techsales)
techsales$HighNPS <- as.integer(techsales$NPS >= 9)


techsales$Business    <- factor(techsales$Business)
techsales$College     <- factor(techsales$College, levels = c("No","Yes"))
techsales$Personality <- factor(techsales$Personality)
techsales$Female      <- factor(techsales$Female, levels = c(0,1), labels = c("Male","Female"))

table(techsales$HighNPS)
techsales <- techsales [-1]
set.seed(2)
train_idx <- sample(1:nrow(techsales), nrow(techsales)/2)
train <- techsales[train_idx, ]
test  <- techsales[-train_idx, ]

#logistics regression
glm.fit <- glm(
  HighNPS ~ Business + Age + Female + Years + College + Personality +
    Certficates + Feedback + Salary,
  data = train, family = binomial
)
summary(glm.fit)  

probs <- predict(glm.fit, newdata = test, type = "response")
pred  <- ifelse(probs > 0.5, 1, 0) 
table(Predicted = pred, Actual = test$HighNPS)
mean(pred == test$HighNPS)


#loocv
library(boot)

glm.full <- glm(
  HighNPS ~ Business + Age + Female + Years + College + Personality +
    Certficates + Feedback + Salary,
  data = techsales, family = binomial)

cv_loocv <- cv.glm(techsales, glm.full, K =10)
cv_loocv$delta[1]
1 - cv_loocv$delta[1]

#k fold
glm.full <- glm(
  HighNPS ~ Business + Age + Female + Years + College + Personality +
    Certficates + Feedback + Salary,
  data = techsales, family = binomial)

cv_10 <- cv.glm(techsales, glm.full, K =10)
cv_10$delta[1]
1-cv_10$delta[1]























