library(ISLR2)

data("College")
attach(College)
fix(College)
dim(College)
names(Credit)
library(leaps)


#1a
set.seed(1)
train <- sample (c(TRUE, FALSE), nrow(College), 
                 replace = TRUE)

test <- (!train)
train_data <- College[train, ]
test_data <- College[test, ]


#1b 
model1 <- lm(Apps ~ ., data = train_data)
lm.pred <- predict(model1, newdata = test_data)
lm.error <- mean ((lm.pred - test_data$Apps)^2) 
lm.error


#1c
library(glmnet) 
set.seed(1)
train.mat = model.matrix(Apps~., data = train_data)
test.mat = model.matrix(Apps~., data = test_data)
y.train <- train_data$Apps
y.test <- test_data$Apps
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(train.mat,y.train,alpha=0,lambda=grid)
cv.out = cv.glmnet(train.mat,y.train, alpha=0)
predict(cv.out, type = "coefficients", s = bestlam)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
ridge.pred <- predict (ridge.mod, s = bestlam,
                        newx= test.mat)
mean((ridge.pred- y.test)^2)


#1d 
lasso.mod=glmnet(train.mat,y.train,alpha=1,lambda=grid)
grid=10^seq(10,-2,length=100)
plot(lasso.mod)
summary(lasso.mod)


cv.out <- cv.glmnet (train.mat, y.train, alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod, s = bestlam, 
                      newx =  test.mat)
mean((lasso.pred - y.test)^2)


out <- glmnet (train.mat,y.train, alpha = 1, lambda =grid)
lasso.coef <- predict (out, type = "coefficients", s = bestlam)[1:18,]
lasso.coef
lasso.coef <- predict (out, type = "coefficients", s = bestlam)[1:10,]
lasso.coef
lasso.coef[lasso.coef != 0]










