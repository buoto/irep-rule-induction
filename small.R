# IREP algotithm implementation
#' @author Michał Błotniak
#' @author Magdalena Rusiecka
source('irep.R')

small.data <- data.frame(c(1, 1, 0, 1, 0, 0), c(1, 1, 1, 0, 0, 0), c(0, 1, 0, 1, 0, 0), c(0, 0, 1, 0, 0, 1))
colnames(small.data) <- c('c', 'a1', 'a2', 'a3')
small.data.split <- split(small.data, small.data$c)


irep(small.data.split$`1`[-1], small.data.split$`0`[-1], 1/4)
