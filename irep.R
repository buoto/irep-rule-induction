library(caTools)
set.seed(1337)

irep <- function(pos, neg, splitRatio, accuracy) {
  clauses <- c()
  if (nrow(pos) > 0) {
    sample <- sample.split(pos$anyColumn, SplitRatio = splitRatio)
  }
}

data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"))
data.split <- split(data, data$p)

irep(data.split$e, data.split$p, 1/4, 0.1)
