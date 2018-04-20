library(datasets)
data(iris)

iris_numeric <- iris[,1:4]

#define k
k <- 3
  
#Assign random data points as cendroids
ran_samples <- sample(dim(iris_numeric)[1], k)

#Assign centroids based on random entries
centroids <- iris_numeric[ran_samples,]

#Calc distances
distances <- dist(rbind(iris_numeric, centroids))

