

#krzywe ROC
#Michał Błotniak, Magdalena Rusiecka
source('irep.R') #tu nie jestem pewna, czy irep, czy lepiota i czy w ogóle coś trzeba
library(pROC)
labels <- match(data[[1]], c('p','e'))-1
scores <- apply(data[-1],1,function(example) predict(classifier, 1, 0, example))
plot(roc(labels, scores))
