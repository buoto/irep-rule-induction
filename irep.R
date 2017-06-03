library(caTools)
library(dplyr)

set.seed(1337)

cover <- function(clause, data) {
  query <- c()
  for (i in 1:length(clause)) {
    if(!is.na(clause[i])) {
      query <- c(query, paste(colnames(data)[i], '==', clause[i]))
    }
  }
  #print(filter_(data, .dots=query))
  filter_(data, .dots=query)
}

addLiteral <- function(clause, pos, neg) {
  bestAccuracy <- -1
  newClause <- clause
  #print(neg)
  for (i in 1:length(clause)) {
    if (is.na(clause[i])) {
      values <- c(pos[[i]], neg[[i]]) %>% unique %>% unlist
      values <- values[!is.na(values)]
      
      for (v in values) {
        TP <- sum(pos[[i]] == v, na.rm = TRUE)
        FN <- sum(neg[[i]] == v, na.rm = TRUE)
        TN <- nrow(neg) - FN
        
        accuracy <- (TP + TN) / (nrow(pos) + nrow(neg))
        print(accuracy)
        if (accuracy >= bestAccuracy) {
          #print(c(v, i, accuracy))
          bestAccuracy <- accuracy
          newClause <- clause
          newClause[i] <- v
        }
      }
    }
  }
  newClause
}

pruneClause <- function(clause, pos, neg) {
  best <- clauseAccuracy(clause, pos, neg)
  accuracies <- rep(0, length(clause))
  repeat {
    for (i in 1:length(clause)) {
      accuracies[i] <- clauseAccuracy(clause[-i], pos, neg)
    }
    #print(accuracies)
    newMax <- max(accuracies)
    #print(c(newMax, best, length(clause)))
    if (sum(!is.na(clause)) > 1 && best <= newMax) {
      best <- newMax
      clause[-which.max(accuracies)] <- NA
    } else {
      return(clause)
    }
  }
}

clauseAccuracy <- function(clause, pos, neg) {
  TP <- clause %>% cover(pos) %>% nrow
  FN <- clause %>% cover(neg) %>% nrow
  TN <- nrow(neg) - FN
  
  (TP + TN) / (nrow(pos) + nrow(neg))
}

failAccuracy <- function(pos, neg) {
  P <- nrow(pos)
  N <- nrow(neg)
  
  N / (P + N)
}

irep <- function(pos, neg, splitRatio, accuracy) {
  clauses <- c()
  failAccuracyValue <- failAccuracy(pos, neg)
  
  while (nrow(pos) > 0) {
    pos.sample <- sample.split(pos[[1]], SplitRatio = splitRatio)
    posGrow <- subset(pos, pos.sample == TRUE)
    posPrune <- subset(pos, pos.sample == FALSE)
    
    neg.sample <- sample.split(neg[[1]], SplitRatio = splitRatio)
    negGrow <- subset(neg, neg.sample == TRUE)
    negPrune <- subset(neg, neg.sample == FALSE)
    
    clause <- rep(NA, ncol(neg))
    #print('negGrow')
    print(posGrow)
    print(negGrow)
    while (nrow(negGrow) > 0) {
      clause <- addLiteral(clause, posGrow, negGrow)
      posGrow <- cover(clause, posGrow)
      negGrow <- cover(clause, negGrow)
    }
    
    #print(c('preprune', clause, negGrow))
    clause <- pruneClause(clause, posPrune, negPrune)
    #print(c('pruned', clause))
    
    if (clauseAccuracy(clause, pos, neg) <= failAccuracyValue) {
      return(clauses)
    } else {
      pos <- pos %>% setdiff(cover(clause, pos))
      neg <- neg %>% setdiff(cover(clause, neg))
      clauses <- c(clauses, list(clause))
      print(clause)
    }
  }
  return(clauses)
}



data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header=FALSE)
#colnames(data) <- c('class', 'cap-shape', 'cap-surface', 'cap-color', 'bruises?', 'odor',
#                    'gill-attachment', 'gill-spacing', 'gill-size', 'gill-color', 'stalk-shape',
#                    'stalk-root', 'stalk-surface-above-ring', 'stalk-surface-below-ring', 'stalk-color-above-ring',
#                    'stalk-color-below-ring', 'veil-type', 'veil-color', 'ring-number', 'ring-type', 'spore-print-color',
#                    'population', 'habitat')

data.split <- split(data, data$V1)

irep(select(data.split$e, -V1), select(data.split$p, -V1), 1/4, 0.1)

#small.data <- data.frame(c(1, 1, 0, 1, 0, 0), c(1, 1, 1, 0, 0, 0), c(0, 1, 0, 1, 0, 0), c(0, 0, 1, 0, 0, 1))
#colnames(small.data) <- c('c', 'a1', 'a2', 'a3')
#small.data.split <- split(small.data, small.data$c)


#pruneClause(c(1,1,0), small.data.split$`1`[-1], small.data.split$`0`[-1])
#irep(small.data.split$`1`[-1], small.data.split$`0`[-1], 1/4, 0.1)



