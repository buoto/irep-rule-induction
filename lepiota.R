# IREP algotithm implementation
#' @author Michał Błotniak
#' @author Magdalena Rusiecka
source('irep.R')
library(pROC)
library(caret)
library(e1071)

set.seed(1337)

data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header=FALSE)

msk <- sample.split(data[,1], SplitRatio = 1/2)
train <- subset(data, msk == TRUE)
test <- subset(data, msk == FALSE)

train.split <- split(train, train$V1)
pos <- select(train.split$e, -V1)
neg <- select(train.split$p, -V1)

model <- irep(pos, neg, 1/2, failAccuracyValue = 1/2)

predicted <- predict(model, test[-1])
predictedLabels <- factor(!predicted, labels = c('e', 'p'))
confusionMatrix(predictedLabels, test$V1, positive = 'e')

refModel <- naiveBayes(train[-1], train$V1)

refPredicted <- predict(refModel, test[-1])
refPredictedLabels <- factor(predicted, labels = c('e', 'p'))
refConfusionMatrix(predictedLabels, test$V1, positive = 'e')

# trim failAccuracyValue
scores <- predictionThresholds(function(pos, neg, x) irep(pos, neg, 1/2, failAccuracyValue = x), 1:10/10, pos, neg, test[-1])
roc(test$V1, scores, levels=c('p', 'e'), plot = TRUE)

# trim splitRatio
scores <- predictionThresholds(function(pos, neg, x) irep(pos, neg), 1:10/10, pos, neg, test[-1])
roc(test$V1, scores, levels=c('p', 'e'), plot = TRUE)