# IREP algotithm implementation
#' @author Michał Błotniak
#' @author Magdalena Rusiecka

library(caTools)
library(dplyr)
library(lazyeval)

#' Extract rules from provided dataset using IREP algorithm
#' based on Furnkranz and Widmer paper.
#' 
#' @param pos Dataframe containing positive examples.
#' @param neg Dataframe containing negative examples.
#' @param splitRatio A number.
#' @param failAccuracyValue Minimal rule accuracy.
#' @return List of rules represented as vectors of attributes.
irep <- function(pos, neg, splitRatio, failAccuracyValue = failAccuracy(pos, neg)) {
  rules <- list()
  
  while (nrow(pos) > 0) {
    pos.sample <- sample.split(pos[[1]], SplitRatio = splitRatio)
    posGrow <- subset(pos, pos.sample == TRUE)
    posPrune <- subset(pos, pos.sample == FALSE)
    
    neg.sample <- sample.split(neg[[1]], SplitRatio = splitRatio)
    negGrow <- subset(neg, neg.sample == TRUE)
    negPrune <- subset(neg, neg.sample == FALSE)
    
    rule <- rep(NA, ncol(neg))
    while (nrow(negGrow) > 0) {
      rule <- addLiteral(rule, posGrow, negGrow)
      posGrow <- cover(rule, posGrow)
      negGrow <- cover(rule, negGrow)
    }
    
    rule <- pruneRule(rule, posPrune, negPrune)
    
    if (ruleAccuracy(rule, pos, neg) <= failAccuracyValue) {
      class(rules) <- 'irep' 
      return(rules)
    } else {
      pos <- setdiff(pos, cover(rule, pos))
      neg <- setdiff(neg, cover(rule, neg))
      rules <- c(rules, list(rule))
    }
  }
  
  class(rules) <- 'irep' 
  return(rules)
}

#' Returns a vector of predicted responses from a fitted irep object.
#' 
#' @param object Fitted model object of class "irep".
#' @param newdata Dataframe of new data to predict.
#' @return Vector of predicted responses (TRUE/FALSE)
predict.irep <- function(object, newdata) apply(newdata, 1, function (example) matchRules(object, example))

#' Match example against many rules.
#' 
#' @param rules Fitted rules from "irep" model.
#' @param example Single example as vector of attributes.
#' @return Rules prediction (TRUE/FALSE).
matchRules <- function(rules, example) {
  if (length(rules) == 0) return(TRUE)
  rules %>% sapply(function(rule) matchRule(rule, example)) %>% any
}

#' Match example against one rule.
#' 
#' @param rules Single rule represented as vector of attributes.
#' @param example Single example as vector of attributes.
#' @return Rules prediction (TRUE/FALSE).
matchRule <- function(rule, example) all(rule == example, na.rm = TRUE)

#' Grow rule with one literal.
#' 
#' @param rule Rule represented as vector of attributes.
#' @param pos Dataframe containing positive examples.
#' @param neg Dataframe containing negative examples.
#' @return Rule with new literal added.
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
          bestAccuracy <- accuracy
          newRule <- rule
          newRule[i] <- v
        }
      }
    }
  }
  newRule
}

#' Prune maximizing rule accuracy.
#' 
#' @param rule Rule represented as vector of attributes.
#' @param pos Dataframe containing positive examples.
#' @param neg Dataframe containing negative examples.
#' @return Pruned rule.
pruneRule <- function(rule, pos, neg) {
  best <- ruleAccuracy(rule, pos, neg)
  repeat {
    accuracies <- rep(0, length(rule))
    for (i in 1:length(rule)) {
      if (!is.na(rule[i])) {
        newRule <- rule
        newRule[i] <- NA
        accuracies[i] <- ruleAccuracy(newRule, pos, neg)
      }
    }
    newMax <- max(accuracies)
    if (sum(!is.na(rule)) > 1 && best < newMax) {
      best <- newMax
      rule[which.max(accuracies)] <- NA
    } else {
      return(rule)
    }
  }
}

#' Get fail accuracy for provided dataset.
#' 
#' @param pos Dataframe containing positive examples.
#' @param neg Dataframe containing negative examples.
#' @return A number - fail accuracy.
failAccuracy <- function(pos, neg) {
  P <- nrow(pos)
  N <- nrow(neg)
  
  N / (P + N)
}

#' Get accuracy of rule on provided dataset.
#' 
#' @param rule Rule represented as vector of attributes.
#' @param pos Dataframe containing positive examples.
#' @param neg Dataframe containing negative examples.
#' @return A number - rule accuracy.
ruleAccuracy <- function(rule, pos, neg) {
  TP <- rule %>% cover(pos) %>% nrow
  FN <- rule %>% cover(neg) %>% nrow
  TN <- nrow(neg) - FN
  
  (TP + TN) / (nrow(pos) + nrow(neg))
}

#' Filter dataframe with provided rule.
#' 
#' @param rule Rule represented as vector of attributes.
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
