library(caTools)
library(dplyr)
library(lazyeval)
library(pROC)

set.seed(1337)

#' Filter dataframe with provided rule.
#' 
#' @param rule Vector of attributes.
#' @param df Dataframe to filter.
#' @return Filtered dataframe.
cover <- function(rule, df) {
  filterConds <- c()
  for (i in 1:length(rule)) {
    if(!is.na(rule[i])) {
      filterConds <- c(filterConds, interp(~df[[i]] == x, i = i, x = rule[i]))
    }
  }
  filter_(df, .dots=filterConds)
}

addLiteral <- function(rule, pos, neg) {
  bestAccuracy <- -1
  newRule <- rule
  for (i in 1:length(rule)) {
    if (is.na(rule[i])) {
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
          newRule <- rule
          newRule[i] <- v
        }
      }
    }
  }
  newRule
}

pruneRule <- function(rule, pos, neg) {
  best <- ruleAccuracy(rule, pos, neg)
  print(c("PRUNING best is", best))
  repeat {
    accuracies <- rep(0, length(rule))
    for (i in 1:length(rule)) {
      if (!is.na(rule[i])) {
        newRule <- rule
        newRule[i] <- NA
        accuracies[i] <- ruleAccuracy(newRule, pos, neg)
      }
    }
    print(accuracies)
    newMax <- max(accuracies)
    #print(c(newMax, best, length(rule)))
    if (sum(!is.na(rule)) > 1 && best < newMax) {
      best <- newMax
      rule[which.max(accuracies)] <- NA
    } else {
      print(c("now best is", best))
      return(rule)
    }
  }
}

ruleAccuracy <- function(rule, pos, neg) {
  TP <- rule %>% cover(pos) %>% nrow
  FN <- rule %>% cover(neg) %>% nrow
  TN <- nrow(neg) - FN
  
  (TP + TN) / (nrow(pos) + nrow(neg))
}

failAccuracy <- function(pos, neg) {
  P <- nrow(pos)
  N <- nrow(neg)
  
  N / (P + N)
}

irep <- function(pos, neg, splitRatio, accuracy) {
  rules <- c()
  failAccuracyValue <- failAccuracy(pos, neg)
  
  while (nrow(pos) > 0) {
    pos.sample <- sample.split(pos[[1]], SplitRatio = splitRatio)
    posGrow <- subset(pos, pos.sample == TRUE)
    posPrune <- subset(pos, pos.sample == FALSE)
    
    neg.sample <- sample.split(neg[[1]], SplitRatio = splitRatio)
    negGrow <- subset(neg, neg.sample == TRUE)
    negPrune <- subset(neg, neg.sample == FALSE)
    
    rule <- rep(NA, ncol(neg))
    #print('negGrow')
    #print(posGrow)
    #print(negGrow)
    while (nrow(negGrow) > 0) {
      rule <- addLiteral(rule, posGrow, negGrow)
      posGrow <- cover(rule, posGrow)
      negGrow <- cover(rule, negGrow)
      print(rule)
    }
    
    #print(c('preprune', rule, negGrow))
    rule <- pruneRule(rule, posPrune, negPrune)
    print(c('pruned', rule))
    
    if (ruleAccuracy(rule, pos, neg) <= failAccuracyValue) {
      return(rules)
    } else {
      pos <- setdiff(pos, cover(rule, pos))
      neg <- setdiff(neg, cover(rule, neg))
      rules <- c(rules, list(rule))
    }
  }
  return(rules)
}

predict <- function(rules, posLabel, negLabel, example) {
  matching <- matchRules(rules, example)
  if (matching) posLabel else negLabel
}

matchRules <- function(rules, example) rules %>% sapply(function(rule) matchRule(rule, example)) %>% any

matchRule <- function(rule, example) all(rule == example, na.rm = TRUE)


