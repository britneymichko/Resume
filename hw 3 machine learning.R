#question 1
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
library(pls)
pcr.fit <-pcr(Apps~ ., data = College, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
set.seed(1)
pcr.fit <-pcr(Apps ~ ., data = College, subset = train,
              scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
pcr.pred = predict(pcr.fit, newdata = test_data, ncomp = 10)
mean((pcr.pred - test_data$Apps)^2)
pcr.fit <- pcr(Apps ~ ., data = College, subset = train, scale = TRUE, ncomp = 10)
summary(pcr.fit)

#1c
set.seed(1)
pls.fit <- plsr(Apps~ ., data = College, subset= train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
pls.pred=predict(pls.fit,newdata = test_data,ncomp=9) 
mean((pls.pred-test_data$Apps)^2) 
pls.pred=predict(pls.fit,newdata = test_data,ncomp=8) 
mean((pls.pred-test_data$Apps)^2) 
pls.fit <- plsr(Apps~ ., data = College, scale = TRUE,
                ncomp=9)
summary(pls.fit)




