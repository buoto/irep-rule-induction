source('irep.R')
library(pROC)

data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header=FALSE)
#colnames(data) <- c('class', 'cap-shape', 'cap-surface', 'cap-color', 'bruises?', 'odor',
#                    'gill-attachment', 'gill-spacing', 'gill-size', 'gill-color', 'stalk-shape',
#                    'stalk-root', 'stalk-surface-above-ring', 'stalk-surface-below-ring', 'stalk-color-above-ring',
#                    'stalk-color-below-ring', 'veil-type', 'veil-color', 'ring-number', 'ring-type', 'spore-print-color',
#                    'population', 'habitat')

data.split <- split(data, data$V1)
pos <- select(data.split$e, -V1)
neg <- select(data.split$p, -V1)

classifier <- irep(pos, neg, 1/2)

predict(classifier, list(c(1:4, 'n', 'f', 1:9, 'p', 'w', 1:5))) # positive example
matchRules(classifier, c(1:4, 'n', 'f', 1:9, 'p', 'x', 1:5)) # negative example


labels <- match(data[[1]], c('p','e'))-1
scores <- apply(data[-1],1,function(example) predict(classifier, 1, 0, example))
plot(roc(labels, scores))