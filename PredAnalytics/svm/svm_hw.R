library(e1071)
library(caret)

data <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/tic-tac-toe/tic-tac-toe.data',
                 col.names = c('top-left-square', 'top-middle-square', 'top-right-square', 'middle-left-square',
                               'middle-middle-square','middle-right-square','bottom-left-square','bottom-middle-square',
                               'bottom-right-square', 'Class'))

#data[data$top.left.square == 'x',]$top.left.square <- 1


#data[,1] <- as.numeric( factor(data[,1]))
#data[,2] <- as.numeric( factor(data[,2]))
#data[,3] <- as.numeric( factor(data[,3]))
#data[,4] <- as.numeric( factor(data[,4]))
#data[,5] <- as.numeric( factor(data[,5]))
#data[,6] <- as.numeric( factor(data[,6]))
#data[,7] <- as.numeric( factor(data[,7]))
#data[,8] <- as.numeric( factor(data[,8]))
#data[,9] <- as.numeric( factor(data[,9]))


change_to_nums <- function(dp) {
  if (dp == 'x') {
    return(0)
  } 
  else if (dp == 'o') {
    return(1)
  }
  else if (dp == 'b') {
    return(2)
  }
  else {
    return(dp)
  }
}

apply(data, c(957,10), change_to_nums)


print_v <- function(x) {print(x)}
apply(data, c(957,10), print_v(x))




