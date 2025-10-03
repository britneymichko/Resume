url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data"
download.file(url, destfile = "spambase.csv")
spambase <- read.csv("spambase.csv", header = FALSE)

library(neuralnet)
#1a
spambase <- na.omit(spambase)

#1b
trainingdata = 0.80 * nrow(spambase)
set.seed(80)
index = sample( seq_len ( nrow ( spambase ) ), size = trainingdata )

#1c 
max = apply(spambase , 2 , max)
min = apply(spambase, 2 , min)
scaled = as.data.frame(scale(spambase, center = min, scale = max - min))
trainNN = scaled[index , ]
testNN = scaled[-index , ]

set.seed(2)
NN = neuralnet(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 +
                 V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 +
                 V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 +
                 V31 + V32 + V33 + V34 + V35 + V36 + V37 + V38 + V39 + V40 +
                 V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 +
                 V51 + V52 + V53 + V54 + V55 + V56 + V57,
               data = trainNN, hidden = 10, linear.output = FALSE)
  
summary(NN)
plot(NN)

#1d
predict_trainNN = compute(NN, trainNN[,c(1:57)])
predict_trainNN = (predict_trainNN$net.result * (max(spambase$V58) - min(spambase$V58))) + min(spambase$V58)

plot(trainNN$V58, predict_trainNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)

RMSE.NN = (sum((trainNN$V58 - predict_trainNN)^2) / nrow(trainNN)) ^ 0.5
print(RMSE.NN)


#1e
predict_testNN = compute(NN, testNN[,c(1:57)])
predict_testNN = (predict_testNN$net.result * (max(spambase$V58) - min(spambase$V58))) + min(spambase$V58)

plot(testNN$V58, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)
RMSE.NN = (sum((testNN$V58 - predict_testNN)^2) / nrow(testNN)) ^ 0.5
print(RMSE.NN)


#1f
set.seed(2)
NN_multi <- neuralnet(
  V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 +
    V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 +
    V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 +
    V31 + V32 + V33 + V34 + V35 + V36 + V37 + V38 + V39 + V40 +
    V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 +
    V51 + V52 + V53 + V54 + V55 + V56 + V57,
  data = trainNN,
  hidden = c(5, 3, 4),
  linear.output = FALSE
)
summary(NN_multi)
plot(NN_multi)


#1g
predict_trainNN_multi <- compute(NN_multi, trainNN[, c(1:57)]) 
predict_trainNN_multi <- (predict_trainNN_multi$net.result * 
                            (max(spambase$V58) - min(spambase$V58))) + 
  min(spambase$V58)

plot(trainNN$V58, predict_trainNN_multi, col = 'blue', pch = 16,
     ylab = "Predicted rating NN (multi)", xlab = "Real rating")
abline(0, 1)

RMSE_train_multi <- sqrt(sum((trainNN$V58 - predict_trainNN_multi)^2) / nrow(trainNN))
print(RMSE_train_multi)


#1f
predict_testNN_multi <- compute(NN_multi, testNN[, c(1:57)]) 
predict_testNN_multi <- (predict_testNN_multi$net.result * 
                            (max(spambase$V58) - min(spambase$V58))) + 
  min(spambase$V58)

plot(testNN$V58, predict_testNN_multi, col = 'blue', pch = 16,
     ylab = "Predicted rating NN (multi)", xlab = "Real rating")
abline(0, 1)


RMSE_test_multi <- sqrt(sum((testNN$V58 - predict_testNN_multi)^2) / nrow(testNN))
print(RMSE_test_multi)









