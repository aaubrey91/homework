ds <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
               header=FALSE, col.names= c('class', 'Alcohol', 'Malic acid', 'Ash', 'Alcalinity of ash', 'Magnesium',
                                          'Total phenols', 'Flavanoids', 'Nonflavanoid phenols', 'Proanthocyanins',
                                          'Color intensity', 'Hue', 'OD280/OD315 of diluted wines', 'Proline'))

plot(ds$Proline, ds$OD280.OD315.of.diluted.wines, col=ds$class)

k <- 3

### KMEANS ###
for (i in 1:100) {
  s <- ds[sample(nrow(ds), k*30),]
  km <- kmeans(s[,c(2:14)], k)
  c <- confusionMatrix(table(s$class, km$cluster))
  acc[i] <- c$overall[1]
}
mean(acc)
max(acc)
min(acc)

### PAMK ###
library(fpc)
for (i in 1:100) {
  s <- ds[sample(nrow(ds), k*30),]
  pk <- pamk(s[,c(2:14)], krange = k)
  c <- confusionMatrix(table(s$class, pk$pamobject$clustering))
  acc[i] <- c$overall[1]
}
mean(acc)

### DBSCAN ###
library(dbscan)
kNNdistplot(s[,c(2:14)], 18)
for (i in 1:100) {
  s <- ds[sample(nrow(ds), k*30),]
  db <- dbscan(s[,c(2:14)], eps=4.5, MinPts=18)
  #db <- dbscan(s[,c(13:14)], eps=90, MinPts=3) --- Gets Three Clusters
  c <- confusionMatrix(table(s$class, pk$pamobject$clustering))
  acc[i] <- c$overall[1]
  ### Can talk about how it varies and unable to compute a p-value.
  ### Use "good_dbscan_plot.png" plot in paper as well as bad plot
}
mean(acc)

### SPEC ###
library(kernlab)
acc <- NULL
for (i in 1:100) {
  s <- ds[sample(nrow(ds), k*30),]
  sc <- specc(matrix(unlist(s[,c(2:14)]), ncol=13, byrow=T), centers=k)
  c <- confusionMatrix(table(s$class, sc))
  acc[i] <- c$overall[1]
}
dmultinom( c( i, j, k ), prob = givenProbabilities ) 
givenProbabilities <- ( 1/N )*classification

## Using Built in function
multinomial.test(c(56, 72, 49), c(58/178, 71/178, 48/178))

mean(acc)
min(acc)
max(acc)

plot(ds$Proline, ds$OD280.OD315.of.diluted.wines, col=sc)
