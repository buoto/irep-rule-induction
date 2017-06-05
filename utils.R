#' @author Michał Błotniak
#' @author Magdalena Rusiecka

#' Get thresholds for provided classification method.
#' 
#' @param constructor Function accepting positive/negative sets and value
#' @param values Vector of values to check.
#' @param pos Positive data.
#' @param neg Negative data.
#' @param test Test data.
#' @return Vector of value thresholds above which model classifies example as positive.
predictionThresholds <- function(constructor, values, pos, neg, test) {
  values <- sort(values)
  result <- rep(max(values)[1], nrow(pos) + nrow(neg))
  for (x in values) {
    model <- constructor(pos, neg, x)
    scores <- predict(model, test)
    thresholds <- ifelse(scores, x, NA)
    result <- pmin(result, thresholds, na.rm = TRUE)
  }
  result
}
