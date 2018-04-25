library(datasets)
data(iris)

pdf("myplots.pdf")

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

## Assign new centroids ##
assign_centroid <- function(data, groups) {
  #iris_numeric[t(groups)[,1] == 1,]
  #dim(iris_numeric[t(groups)[,1] == 1,])[1]
  #colMeans(iris_numeric)[1]
  
  new_centroids <- matrix(c(0), nrow=nrow(groups), ncol=ncol(data))
  
  for (i in 1:dim(new_centroids)[1]) {
    group <- data[t(groups)[,i] == 1,]
    group_size <- dim(group)[1]
    new_centroids[i,] <- c(colMeans(group)[1], colMeans(group)[2], colMeans(group)[3], colMeans(group)[4])
  }
  
  #group_1 <- data[t(groups)[,1] == 1,]
  #group_1_size <- dim(group_1)[1]
  #group_1_dims <- c((colMeans(group_1)[1] / group_1_size), (colMeans(group_1)[2] / group_1_size), (colMeans(group_1)[3] / group_1_size), (colMeans(group_1)[4] / group_1_size))
  return(new_centroids)
}

### Transform Groups to Cluster Nums for plotting###
assign_cluster_num <- function(groups) {
  clusters <- matrix(c(0), nrow=ncol(groups))
  
  for (i in 1:dim(clusters)[1]) {
    for (k in 1:dim(groups)[1]) {
      if(t(groups)[i,k] == 1) {
        clusters[i] = k
      }
    }
  }
  return(clusters)
}

################################################################################

distances <- calc_distances(iris_numeric, centroids)
groups <- assign_group(distances)

while(TRUE) {
  
  #Calc new groups
  new_centroids <- assign_centroid(iris_numeric, groups)
  new_distances <- calc_distances(iris_numeric, new_centroids)
  new_groups <- assign_group(new_distances)
  
  if(identical(groups, new_groups)) { # New Groups match old groups, let's break, they haven't changed
    break
  }
  else {
    #reset vars for next loop
    groups <- new_groups
  }
}

built_in_r_funct <- kmeans(iris_numeric, 3)

print("Number of elements in cluster 1 for built in function:")
print(sum(built_in_r_funct$cluster==1))

print("Number of elements in cluster 2 for built in function:")
print(sum(built_in_r_funct$cluster==2))

print("Number of elements in cluster 3 for built in function:")
print(sum(built_in_r_funct$cluster==3))

print("Number of elements in cluster 1 for user defined function:")
print(sum(groups[1,] == 1))

print("Number of elements in cluster 2 for user defined function:")
print(sum(groups[2,] == 1))

print("Number of elements in cluster 3 for user defined function:")
print(sum(groups[3,] == 1))


library(cluster)

clusplot(iris, built_in_r_funct$cluster, color=TRUE, shade=TRUE, lines=0, main="Built in R Function")

clusplot(iris, assign_cluster_num(groups), color=TRUE, shade=TRUE, lines=0, main="Alex defined R Function")
graphics.off()