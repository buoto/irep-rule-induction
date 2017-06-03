source('irep.R')

data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header=FALSE)
#colnames(data) <- c('class', 'cap-shape', 'cap-surface', 'cap-color', 'bruises?', 'odor',
#                    'gill-attachment', 'gill-spacing', 'gill-size', 'gill-color', 'stalk-shape',
#                    'stalk-root', 'stalk-surface-above-ring', 'stalk-surface-below-ring', 'stalk-color-above-ring',
#                    'stalk-color-below-ring', 'veil-type', 'veil-color', 'ring-number', 'ring-type', 'spore-print-color',
#                    'population', 'habitat')

data.split <- split(data, data$V1)
pos <- select(data.split$e, -V1)
neg <- select(data.split$p, -V1)

irep(select(data.split$e, -V1), select(data.split$p, -V1), 1/4)