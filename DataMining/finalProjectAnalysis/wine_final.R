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
  c <- confusionMatrix(table(s$class, pk$pamobject$clustering))
  acc[i] <- c$overall[1]
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
mean(acc)
min(acc)
max(acc)

plot(ds$Proline, ds$OD280.OD315.of.diluted.wines, col=sc)
