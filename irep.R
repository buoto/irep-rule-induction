library(caTools)
library(dplyr)

set.seed(1337)

cover <- function(clause, data) filter_(data, .dots=clause)

findLiteral <- function(clause, pos, neg) {
  current <- length(clause) + 1
  values <- c(pos[[current]], neg[[current]]) %>% unique
  coveredNeg <- values %>% lapply(function(x) sum(neg[[current]] == x))
  best <- which.min(coveredNeg)
  literal <- paste(colnames(neg)[current], '==', best)
  print(literal)
}

pruneClause <- function(clause, pos, neg) {
  # TODO
}

clauseAccuracy <- function(clause, pos, neg) {
  TP <- clause %>% cover(pos) %>% nrow
  FN <- clause %>% cover(neg) %>% nrow
  TN <- nrow(neg) - FN
  
  (TN + TN) / (nrow(pos) + nrow(neg))
}

failAccuracy <- function(pos, neg) {
  P <- nrow(pos)
  N <- nrow(neg)
  
  N / (P + N)
}

irep <- function(pos, neg, splitRatio, accuracy) {
  clauses <- c()
  failAccuracyValue <- failAccuracy(pos, neg)
  print(splitRatio)
  
  while (nrow(pos) > 0) {
    pos.sample <- sample.split(pos, SplitRatio = splitRatio)
    posGrow <- subset(pos, pos.sample == TRUE)
    posPrune <- subset(pos, pos.sample == FALSE)
    
    neg.sample <- sample.split(neg, SplitRatio = splitRatio)
    negGrow <- subset(neg, neg.sample == TRUE)
    negPrune <- subset(neg, neg.sample == FALSE)
    
    clause <- list()
    while (nrow(negGrow)) {
      clause <- c(clause, findLiteral(clause, posGrow, negGrow))
      posGrow <- cover(clause, posGrow)
      negGrow <- cover(clause, negGrow)
    }
    
    clause <- pruneClause(clause, posPrune, negPrune)
    
    if (clauseAccuracy(clause, pos, neg) <= failAccuracyValue) {
      return(clauses)
    } else {
      pos <- pos %>% setdiff(cover(clause, pos))
      neg <- neg %>% setdiff(cover(clause, neg))
      clauses <- c(clauses, clause)
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

small.data <- data.frame(c(1, 1, 0, 1, 0, 0), c(1, 1, 1, 0, 0, 0), c(0, 1, 0, 1, 0, 0), c(0, 0, 1, 0, 0, 1))
colnames(small.data) <- c('c', 'a1', 'a2', 'a3')
small.data.split <- split(small.data, small.data$c)


findLiteral(list(), small.data.split$`1`[-1], small.data.split$`0`[-1])
irep(small.data.split$`1`[-1], small.data.split$`0`[-1], 1/4, 0.1)



