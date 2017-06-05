# IREP algotithm implementation
#' @author Michał Błotniak
#' @author Magdalena Rusiecka
source('irep.R')
library(pROC)
library(caret)
library(e1071)

set.seed(1337)

data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header=FALSE)
#colnames(data) <- c('class', 'cap-shape', 'cap-surface', 'cap-color', 'bruises?', 'odor',
#                    'gill-attachment', 'gill-spacing', 'gill-size', 'gill-color', 'stalk-shape',
#                    'stalk-root', 'stalk-surface-above-ring', 'stalk-surface-below-ring', 'stalk-color-above-ring',
#                    'stalk-color-below-ring', 'veil-type', 'veil-color', 'ring-number', 'ring-type', 'spore-print-color',
#                    'population', 'habitat')

msk <- sample.split(data[,1], SplitRatio = 1/2)
train <- subset(data, msk == TRUE)
test <- subset(data, msk == FALSE)

train.split <- split(train, train$V1)
pos <- select(train.split$e, -V1)
neg <- select(train.split$p, -V1)

model <- irep(pos, neg, 1/2)

predicted <- predict(model, test[-1])
predictedLabels <- factor(!predicted, labels = c('e', 'p'))
confusionMatrix(predictedLabels, test$V1, positive = 'e')

refModel <- naiveBayes(train[-1], train$V1)

refPredicted <- predict(refModel, test[-1])
refPredictedLabels <- factor(predicted, labels = c('e', 'p'))
refConfusionMatrix(predictedLabels, test$V1, positive = 'e')

labels <- match(data[[1]], c('p','e'))-1
scores <- apply(data[-1],1,function(example) predict(classifier, 1, 0, example))
plot(roc(labels, scores))