library(datasets)
data(iris)

iris_numeric <- iris[,1:4]

#define k
k <- 3
  
#Assign random data points as cendroids
ran_samples <- sample(dim(iris_numeric)[1], k)

#Assign centroids based on random entries
centroids <- iris_numeric[ran_samples,]

############################# FUNCTION DEFINITIONS #############################

## Calc Euclidean distances ##
calc_distances <- function(data, centroids) {
  distances <- matrix(c(0), ncol=nrow(data), nrow=nrow(centroids))
  for (i in 1:nrow(data)) {
    for (j in 1:k) {
      distances[j,i] <- dist(rbind(data[i,], centroids[j,]))
    }
  }
  return(distances)
  
  #OLD 
  #Calc distances
  #distances <- dist(rbind(iris_numeric, centroids))
  distances <- matrix(c(0), ncol=nrow(iris_numeric), nrow=k)
  for (i in 1:nrow(iris_numeric)) {
    for (j in 1:k) {
      distances[j,i] <- dist(rbind(iris_numeric[i,], centroids[j,]))
    }
  }
}

## Assign groups based on distances ##
assign_group <- function(euc_distances) {
  groups <- matrix(c(0), ncol=nrow(iris_numeric), nrow=k)
  for (i in 1:ncol(groups)) {
    group <- which(min(euc_distances[,i]) == euc_distances[,i], arr.ind = T)
    groups[group,i] <- 1
  }
  return(groups)
  
  #OLD 
  ##Assign groups
  #groups <- matrix(c(0), ncol=nrow(iris_numeric), nrow=k)
  #for (i in 1:ncol(groups)) {
  #  group <- which(min(distances[,i]) == distances[,i], arr.ind = T)
  # groups[group,i] <- 1
  #}
  
}

############################# FUNCTION DEFINITIONS #############################

distances <- calc_distances(iris_numeric, centroids)
groups <- assign_group(distances)


