library(caTools)
library(dplyr)
library(lazyeval)
library(pROC)

set.seed(1337)

cover <- function(clause, data) {
  filterConds <- c()
  for (i in 1:length(clause)) {
    if(!is.na(clause[i])) {
      filterConds <- c(filterConds, interp(~data[[i]] == x, i = i, x = clause[i]))
    }
  }
  filter_(data, .dots=filterConds)
}

addLiteral <- function(clause, pos, neg) {
  bestAccuracy <- -1
  newClause <- clause
  for (i in 1:length(clause)) {
    if (is.na(clause[i])) {
      values <- list(pos[[i]], neg[[i]]) %>% unlist %>% unique
      values <- values[!is.na(values)]
      
      for (v in values) {
        TP <- sum(pos[[i]] == v, na.rm = TRUE)
        FN <- sum(neg[[i]] == v, na.rm = TRUE)
        TN <- nrow(neg) - FN
        
        accuracy <- (TP + TN) / (nrow(pos) + nrow(neg))
        if (accuracy > bestAccuracy) {
          print(c(v, i, accuracy))
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
  print(c("PRUNING best is", best))
  repeat {
    accuracies <- rep(0, length(clause))
    for (i in 1:length(clause)) {
      if (!is.na(clause[i])) {
        newClause <- clause
        newClause[i] <- NA
        accuracies[i] <- clauseAccuracy(newClause, pos, neg)
      }
    }
    print(accuracies)
    newMax <- max(accuracies)
    #print(c(newMax, best, length(clause)))
    if (sum(!is.na(clause)) > 1 && best < newMax) {
      best <- newMax
      clause[which.max(accuracies)] <- NA
    } else {
      print(c("now best is", best))
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
    #print(posGrow)
    #print(negGrow)
    while (nrow(negGrow) > 0) {
      clause <- addLiteral(clause, posGrow, negGrow)
      posGrow <- cover(clause, posGrow)
      negGrow <- cover(clause, negGrow)
      print(clause)
    }
    
    #print(c('preprune', clause, negGrow))
    clause <- pruneClause(clause, posPrune, negPrune)
    print(c('pruned', clause))
    
    if (clauseAccuracy(clause, pos, neg) <= failAccuracyValue) {
      return(clauses)
    } else {
      pos <- pos %>% setdiff(cover(clause, pos))
      neg <- neg %>% setdiff(cover(clause, neg))
      clauses <- c(clauses, list(clause))
    }
  }
  return(clauses)
}

predict <- function(rules, posLabel, negLabel, example) {
  matching <- matchRules(rules, example)
  if (matching) posLabel else negLabel
}

matchRules <- function(rules, example) rules %>% sapply(function(rule) matchRule(rule, example)) %>% any

matchRule <- function(rule, example) all(rule == example, na.rm = TRUE)

roc <- function (labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
  
}
